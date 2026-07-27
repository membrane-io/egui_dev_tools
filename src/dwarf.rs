//! DWARF-backed callstacks and live locals for the widget picker.
//!
//! The picker already captures a callstack inside `Plugin::on_widget_under_pointer`, which
//! egui calls from *inside* `Context::create_widget` — synchronously, within the app's own
//! widget-building call stack. Two things follow from that, and this module is both of them.
//!
//! **Names.** Each captured `CallSite` carries a module-relative code offset, which
//! `dwarf-explorer` resolves against the running module's own debug info. That replaces the
//! `Error.stack` *string* parse in [`crate::widget_inspect`], which only yields Rust symbols
//! if the page is set up with `wasm-stack-trace`.
//!
//! **Locals.** When the hook runs, every frame from `App::ui` down to the `ui.button(…)` call
//! is still on the shadow stack, so their locals are real addresses — readable, and writable.
//! That is the one moment they can be read, which is why the inspector is drawn from inside
//! the hook rather than deferred to the end of the pass.
//!
//! wasm32-only: a native backtrace's addresses aren't module offsets into any DWARF we hold,
//! and native has no shadow stack to walk. Native keeps the `backtrace`-crate path.

use dwarf_explorer_egui::{
    DevTools, Rows, StackCapture,
    dwarf_explorer::{Frame, IndexStats},
};
use egui::{Context, Id, Order, WidgetRect};

use crate::SourceLocation;
use crate::symbol_parser::Symbol;
use crate::widget_inspect::{ParsedFrame, ResolvedFrame};

/// How deep a captured stack may be. V8 reads `Error.stackTraceLimit` when the `Error` is
/// *constructed*, so this is set once, up front — not in the resolve path. egui call paths
/// run deep (panels, layouts, containers, the widget itself), and a stack truncated at V8's
/// default of 10 would cut off the app frames that are the entire point.
const STACK_LIMIT: u32 = 1000;

/// The DWARF side of the widget picker: the debug info, and which widget is pinned.
pub struct Dwarf {
    /// `None` until the page hands us the module's bytes — see [`Self::load`].
    dev: Option<DevTools>,
    /// The widget we're probing, mirroring `Context::set_probed_widget`. `Some` means the
    /// live-locals window is up and the picker is off.
    pinned: Option<Id>,
    /// Units to index per frame, adapted to the frame time we're actually getting.
    index_budget: usize,
    /// The pass [`Self::probed_widget_ui`] last drew in — see the guard there.
    drawn_pass: Option<u64>,
}

impl Default for Dwarf {
    fn default() -> Self {
        Self {
            dev: None,
            pinned: None,
            index_budget: 1,
            drawn_pass: None,
        }
    }
}

impl Dwarf {
    /// Read the running module's own `.wasm` — debug sections and all — out of the page.
    ///
    /// Nobody hands them to us. `wasm-stack-trace` already retains the exact bytes every
    /// module was compiled from (it needs them to symbolicate), and now exposes that map, so
    /// we look ourselves up by `wasm_bindgen::module()`. That is the *only* correct source:
    /// re-fetching `gaze_bg.wasm` can return a build the dev server has since replaced, and
    /// every code offset on the stack would then resolve to the wrong function — silently,
    /// because a wrong answer looks exactly like a right one.
    ///
    /// It also means this works under `dev_hot.sh`, where dx's own glue loads the module and
    /// there is no point in the page we could hook to be given anything.
    pub fn load(&mut self) -> Result<(), String> {
        dwarf_explorer_egui::set_stack_trace_limit(STACK_LIMIT);
        self.dev = Some(DevTools::self_inspect_from_page().map_err(|e| e.to_string())?);
        Ok(())
    }

    pub fn is_loaded(&self) -> bool {
        self.dev.is_some()
    }

    /// What to tell the user in the picker's header. `None` once the DWARF is ready and
    /// there's nothing to say.
    pub fn status(&self) -> Option<String> {
        match self.index_stats() {
            None => Some("no DWARF — is wasm-stack-trace.js loaded before the module?".to_owned()),
            Some(s) if s.is_complete() => None,
            Some(s) => Some(format!(
                "indexing DWARF… {}/{} units",
                s.units_done, s.units_total
            )),
        }
    }

    /// How far the type index has got. The picker shows this, and refuses to resolve until
    /// it reads complete — see [`Self::is_ready`].
    pub fn index_stats(&self) -> Option<IndexStats> {
        self.dev.as_ref().map(|d| d.db().index_stats())
    }

    /// Can we resolve a callstack yet?
    ///
    /// Gated on the index being *complete*, not merely loaded, because the first resolution
    /// forces a full DWARF walk (`resolve_addr` → `subprogram_at_offset` → `index_all`) and
    /// on a module this size that is seconds of frozen main thread. [`Self::warm_index`]
    /// spreads that cost over idle frames instead; until it's done, the picker falls back to
    /// the `Error.stack` path.
    pub fn is_ready(&self) -> bool {
        self.index_stats().is_some_and(|s| s.is_complete())
    }

    /// Grow the type index a little, without dropping a frame.
    ///
    /// The budget is in units, and units vary in size by orders of magnitude, so a fixed count
    /// would stutter on some targets and crawl on others. Instead it adapts to the frame time
    /// we're actually getting: back off when frames go long, open up when they're comfortable.
    pub fn warm_index(&mut self, ctx: &Context) {
        let Some(dev) = self.dev.as_mut() else {
            return;
        };
        if dev.db().index_stats().is_complete() {
            return;
        }
        let dt = ctx.input(|i| i.stable_dt);
        if dt > 1.0 / 30.0 {
            self.index_budget = (self.index_budget / 2).max(1);
        } else if dt < 1.0 / 50.0 {
            self.index_budget = (self.index_budget * 2).min(64);
        }
        dev.db().index_advance(self.index_budget);
        // Keep frames coming until it's done, even if nothing else is animating.
        ctx.request_repaint();
    }

    /// Resolve a captured stack against DWARF, in the shape the picker's overlay already
    /// renders. Returns `None` if the DWARF isn't ready, so the caller can fall back.
    ///
    /// The `Error` must not have had its `.stack` read yet — see [`StackCapture::from_js_error`].
    ///
    /// `&mut` because a frame may be executing code from a hot-patch module we haven't seen
    /// before, whose DWARF is loaded on first sight.
    pub fn resolve(&mut self, err: wasm_bindgen::JsValue) -> Option<Vec<ParsedFrame>> {
        if !self.is_ready() {
            return None;
        }
        let dev = self.dev.as_mut()?;
        let capture = StackCapture::from_js_error(err);
        Some(
            dev.resolve_frames(&capture)
                .iter()
                // Drop the FFI glue between `new Error()` and `Callstack::capture`: the
                // `#[wasm_bindgen]` `Error::new` wrapper and its `externref shim`. They are
                // real wasm frames of our own module, so neither the module check nor a code
                // offset can reject them — but their DWARF is worthless. The shim has no
                // subprogram at all, and the wrapper's address falls inside a *stale*
                // subprogram range (wasm-bindgen's walrus pass rewrote code offsets but not
                // the ranges), naming whatever function now sits there — an
                // `assert_receiver_is_total_eq`, a `set_transform` — always at line 0. Code
                // that actually ran has a covering line-table row; requiring one keeps exactly
                // the real frames, which is what the old `Error.stack` path did implicitly.
                .filter(|frame| frame.line.is_some())
                .map(parsed_frame)
                .collect(),
        )
    }

    // ---------------------------------------------------------------------------------
    // The pin
    // ---------------------------------------------------------------------------------

    pub fn pinned(&self) -> Option<Id> {
        self.pinned
    }

    /// Probe this widget from now on: egui will call `on_probed_widget` inside its call stack
    /// every frame, pointer or no pointer, and we sample the live locals there.
    pub fn pin(&mut self, ctx: &Context, id: Id) {
        self.pinned = Some(id);
        ctx.set_probed_widget(Some(id));
    }

    pub fn unpin(&mut self, ctx: &Context) {
        self.pinned = None;
        ctx.set_probed_widget(None);
    }

    /// Draw the live-locals window for the pinned widget.
    ///
    /// **Called from inside the probed widget's `create_widget`**, which is the only place the
    /// app's frames are still on the stack. Everything below this point on the stack —
    /// `App::ui`, the panel, the container, the widget's own builder — has live locals right
    /// now, and won't a microsecond after this returns.
    ///
    /// Building widgets here re-enters `create_widget` and so re-enters this hook; egui's
    /// dispatch skips an already-locked plugin, which is what makes that terminate.
    pub fn probed_widget_ui(&mut self, ctx: &Context, widget: &WidgetRect) {
        let Some(dev) = self.dev.as_mut() else {
            return;
        };
        if !self.pinned.is_some_and(|id| id == widget.id) {
            return;
        }
        // Once per pass, even though the hook fires on *every* `create_widget` for the
        // pinned id — and there is more than one: `Ui::interact` and `Response::interact`
        // re-register the same widget to upgrade its senses. Drawing the window again
        // would create every widget in it a second time (duplicate-id errors on every
        // chevron), and the second copy's state stores fight the first's clicks.
        let pass = ctx.cumulative_pass_nr();
        if self.drawn_pass == Some(pass) {
            return;
        }
        self.drawn_pass = Some(pass);

        let mut keep_open = true;
        // Held by reference, not moved into the window: egui lays a title into a galley and
        // drops the source `String` before the body closure runs, and this frame's own DWARF
        // slot would then point at a freed allocation — which the inspector would happily
        // show you, garbage and all.
        let title = format!("live locals — {:?}", widget.id.short_debug_format());
        let window = egui::Window::new(title.as_str())
            .id(Id::new("egui_dev_tools_dwarf_locals"))
            .order(Order::Tooltip)
            .collapsible(false)
            .resizable(true)
            // No `vscroll`: the table inside scrolls itself, and to do that it must be handed a
            // *bounded* height. A scrolling window gives it an infinite one, and it lays out
            // every row instead of the visible handful.
            .vscroll(false)
            .default_width(620.0)
            .default_height(680.0)
            .anchor(egui::Align2::RIGHT_BOTTOM, egui::vec2(-8.0, -8.0))
            .open(&mut keep_open);

        let ectx = ctx.clone();
        dev.with_live_frames(|stack| {
            window.show(&ectx, |ui| {
                if stack.frames().is_empty() {
                    ui.weak("no wasm frames captured here.");
                    return;
                }
                // Keys every frame's open/closed state (as root + stack position), so it
                // must survive re-pinning: one constant per window, nothing captured.
                let frames_root = Id::new("egui_dev_tools_dwarf_locals");
                ui.horizontal(|ui| {
                    ui.weak(
                        "live — green locals are editable; edits write the running frame's memory",
                    );
                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        if ui.small_button("Collapse all").clicked() {
                            stack.set_all_frames_open(ui.ctx(), frames_root, false);
                        }
                        if ui.small_button("Expand all").clicked() {
                            stack.set_all_frames_open(ui.ctx(), frames_root, true);
                        }
                    });
                });
                ui.separator();

                // Every capture starts with the picker's own plumbing: this hook, the
                // plugin dispatch, egui internals — the same run of frames on every
                // widget, ending at `Context::create_widget`, which is what called us.
                // The app's stack is everything outside it, so start there. Numbering
                // keeps the captured indices, so `#N` still names the same frame the
                // full backtrace would.
                let first = stack
                    .frames()
                    .iter()
                    .position(|f| f.function.as_deref() == Some("create_widget"))
                    .map_or(0, |i| i + 1);
                let ectx = ui.ctx().clone();
                let mut rows = Rows::new();
                for i in first..stack.frames().len() {
                    stack.frame_rows(&ectx, &mut rows, frames_root, i, i);
                }
                stack.table(ui, "dev_tools_dwarf_locals", &rows);
            });
        });

        if !keep_open {
            self.unpin(ctx);
        }
    }
}

/// A DWARF [`Frame`] in the shape the picker's overlay already knows how to draw.
///
/// The crate name — which is what the APP/EGUI/STD filtering keys on — comes from the **mangled**
/// `linkage_name`, demangled here. It cannot come from `Frame::function`: that is `DW_AT_name`,
/// the *short* name (`create_widget`, `ui`), and a short name can't tell you whose it is.
fn parsed_frame(f: &Frame) -> ParsedFrame {
    let Some(demangled) = f
        .linkage_name
        .as_deref()
        .map(|n| rustc_demangle::demangle(n).to_string())
        .or_else(|| f.function.clone())
    else {
        // No `DW_TAG_subprogram` covers this address, so we have no symbol — the crate was built
        // without one. `debug = 0` is the obvious way to get here; `debug = 1` is the subtle one,
        // because rustc reads that as *line-tables-only* and emits no subprogram DIEs at all. So
        // we often know the file and line and still cannot name the function.
        //
        // Report it as a failed frame rather than inventing a symbol: a placeholder string would
        // (a) make `Symbol::parse` emit a parse error into the UI, and (b) parse to a crate that
        // matches neither egui nor std, so the frame would classify as *user code* and show by
        // default — burying the real frames under the ones we couldn't read.
        return ParsedFrame::Failed(match (&f.file, f.line) {
            (Some(file), Some(line)) => {
                format!("no symbol @ {:#x} — {file}:{line}", f.module_addr)
            }
            _ => format!("no symbol @ {:#x}", f.module_addr),
        });
    };

    ParsedFrame::Parsed(ResolvedFrame {
        symbol: Symbol::parse(&demangled),
        // `Db::frames` resolves one subprogram per address; it doesn't expand the inline tree, so
        // nothing here is a synthetic inlined frame.
        inlined: false,
        location: SourceLocation {
            path: f.file.clone().unwrap_or_default(),
            line: f.line.unwrap_or(0) as usize,
            column: f.column.unwrap_or(0) as usize,
        },
        original: format!("{:#010x}  {demangled}", f.module_addr),
    })
}

#[expect(unsafe_code)]
#[cfg(not(target_feature = "atomics"))]
// Safety: if there's no atomics, there's no multithreading.
unsafe impl Send for Dwarf {}

#[expect(unsafe_code)]
#[cfg(not(target_feature = "atomics"))]
// Safety: if there's no atomics, there's no multithreading.
unsafe impl Sync for Dwarf {}
