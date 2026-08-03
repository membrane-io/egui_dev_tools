use crate::symbol_parser::Symbol;
use crate::{SourceLocation, open_file};
use egui::emath::{Align2, OrderedFloat, Pos2, Rect, Vec2, pos2, vec2};
use egui::epaint::{
    Color32, FontId, Stroke, StrokeKind,
    text::{LayoutJob, TextFormat},
};
use egui::{
    Align, Context, CursorIcon, Event, Id, Key, LayerId, MouseWheelUnit, Painter, Plugin, RawInput,
    Shape, Spacing, Ui, WidgetRect,
};

pub struct Config {
    /// Whether to show the egui stack frames.
    show_egui_frames: bool,

    /// Whether to show std/alloc stack frames.
    show_std_frames: bool,

    /// Whether to show all other stack frames including JavaScript and unparsed frames.
    show_all_frames: bool,

    /// When true, only widgets that sense clicks are considered.
    clickable_only: bool,
}

impl Config {
    pub fn new() -> Self {
        Self {
            show_egui_frames: false,
            show_std_frames: false,
            show_all_frames: false,
            clickable_only: false,
        }
    }
}

impl std::fmt::Debug for Config {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Config")
            .field("show_egui_frames", &self.show_egui_frames)
            .field("show_std_frames", &self.show_std_frames)
            .field("show_all_frames", &self.show_all_frames)
            .field("clickable_only", &self.clickable_only)
            .finish()
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            show_egui_frames: false,
            show_std_frames: false,
            show_all_frames: false,
            clickable_only: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedFrame {
    pub symbol: Symbol,
    pub inlined: bool,
    pub location: SourceLocation,

    /// The original, unparsed stack trace line. Only available on wasm32 where stack traces are
    /// captured as raw text from `Error.stack`.
    #[cfg(target_arch = "wasm32")]
    pub original: String,
}

impl ResolvedFrame {
    pub fn source_location(&self) -> &SourceLocation {
        &self.location
    }
}

impl ResolvedFrame {
    fn is_user_code(&self) -> bool {
        !self.is_std_code() && !self.is_egui_code()
    }

    fn is_egui_code(&self) -> bool {
        let crate_ = self.symbol.crate_();
        crate_ == "egui"
            || crate_ == "objc2"
            || crate_ == "objc2_app_kit"
            || crate_ == "winit"
            || crate_ == "eframe"
            || crate_ == "egui_extras"
            || crate_ == "egui_tiles"
            || crate_ == "egui_dev_tools"
    }

    fn is_std_code(&self) -> bool {
        let crate_ = self.symbol.crate_();
        crate_ == "core"
            || crate_ == "std"
            || crate_ == "alloc"
            || crate_ == "js_sys"
            || crate_ == "backtrace"
            || crate_ == "<unknown>"
            || crate_ == "subsecond"
    }
}

/// A callstack frame that has been mapped to a location in the source code. Note that a single
/// frame of the original callstack can be mapped to multiple locations in the source code due to
/// inlining.
#[derive(Debug)]
pub(crate) enum ParsedFrame {
    Parsed(ResolvedFrame),
    Failed(String),
}

impl ParsedFrame {
    fn is_user_code(&self) -> bool {
        match self {
            ParsedFrame::Parsed(location) => location.is_user_code(),
            _ => false,
        }
    }
    fn is_ui_add(&self) -> bool {
        match self {
            ParsedFrame::Parsed(location) => {
                location.symbol.type_() == "Ui" && location.symbol.function() == "add"
            }
            _ => false,
        }
    }
}

#[cfg(all(feature = "dwarf", target_arch = "wasm32"))]
use crate::dwarf::Dwarf;

/// No DWARF on this target or without the feature: the picker keeps its `Error.stack` path
/// and every call here is a no-op. A stand-in rather than `#[cfg]` at each use site, so
/// `render` and the hooks stay one code path.
#[cfg(not(all(feature = "dwarf", target_arch = "wasm32")))]
#[derive(Default)]
struct Dwarf;

#[cfg(not(all(feature = "dwarf", target_arch = "wasm32")))]
impl Dwarf {
    fn warm_index(&mut self, _ctx: &Context) {}
    fn pin(&mut self, _ctx: &Context, _id: Id) {}
    fn status(&self) -> Option<String> {
        None
    }
}

/// DWARF-resolved frames for this capture, or `None` to fall back to the `Error.stack` parse
/// — which is what happens on native, without the feature, and while the index is still
/// being walked.
fn resolve_dwarf(dwarf: &mut Dwarf, callstack: &Callstack) -> Option<Vec<ParsedFrame>> {
    #[cfg(all(feature = "dwarf", target_arch = "wasm32"))]
    {
        return dwarf.resolve(callstack.js_error());
    }
    #[cfg(not(all(feature = "dwarf", target_arch = "wasm32")))]
    {
        let _ = (dwarf, callstack);
        None
    }
}

pub struct WidgetInspect {
    /// Configuration
    config: Config,

    /// The running module's own debug info, and the widget we're probing with it.
    dwarf: Dwarf,

    /// Whether the user asked to pin the selected widget (⌘-click) and read its locals live.
    /// Separate from `clicked`, which opens the source instead.
    pin_requested: bool,

    /// Whether the widget inspect is enabled.
    pub enabled: bool,

    /// The index of the selected widget. Used to navigate the callstacks with the mouse wheel.
    selected_widget: usize,

    /// The offset of the scroll wheel. Used to navigate the callstacks with the mouse wheel.
    scroll_offset: f32,

    /// Whether the user just clicked. We need to track this separately from normal
    /// `Response::clicked` as to not interfere with normal widget interactions.
    clicked: bool,

    /// Whether to copy the current callstack to the clipboard. (i.e. Copy command received)
    should_copy_to_clipboard: bool,

    /// Captured callstacks and `Ui` spacing for widgets under the pointer.
    widgets: Vec<(Id, Callstack, Spacing)>,

    /// The top location of the widget under the pointer. Used to reset the selected widget if the user moves the pointer to some other part of the UI.
    last_top_location: Option<Id>,

    /// Frame index to open (set by number keys 1-9).
    open_frame_index: Option<usize>,

    /// Rects where this plugin last painted its overlays.
    painted_rects: Vec<Rect>,

    /// Keys whose press we swallowed, so we can swallow their release too. Otherwise the app
    /// sees a release without a press and can end up in a stuck state.
    consumed_keys: Vec<Key>,
}

impl WidgetInspect {
    pub fn new() -> Self {
        Self::new_with_config(Config::default())
    }

    pub fn new_with_config(config: Config) -> Self {
        // Read our own debug info straight out of the page. Nothing has to hand it to us, so
        // this works under `dev_hot.sh` too, where dx's glue loads the module itself. Cheap:
        // `Db::from_bytes` copies the `.debug_*` sections and nothing more — the index is built
        // lazily, a slice per frame, in `on_end_pass`.
        let mut dwarf = Dwarf::default();
        #[cfg(all(feature = "dwarf", target_arch = "wasm32"))]
        if let Err(e) = dwarf.load() {
            log::warn!("widget picker: no DWARF, falling back to Error.stack parsing ({e})");
        }

        WidgetInspect {
            config,
            dwarf,
            pin_requested: false,
            enabled: false,
            selected_widget: 0,
            scroll_offset: 0.0,
            clicked: false,
            should_copy_to_clipboard: false,
            widgets: vec![],
            last_top_location: None,
            open_frame_index: None,
            painted_rects: vec![],
            consumed_keys: vec![],
        }
    }

    /// Get the rects where this plugin last painted its overlays.
    fn consume_key(&mut self, key: Key) {
        if !self.consumed_keys.contains(&key) {
            self.consumed_keys.push(key);
        }
    }

    pub fn painted_rects(&self) -> &[Rect] {
        &self.painted_rects
    }
}

impl Plugin for WidgetInspect {
    fn debug_name(&self) -> &'static str {
        "WidgetInspectPlugin"
    }

    fn on_end_pass(&mut self, ui: &mut Ui) {
        // Walk a slice of the DWARF each frame. The first callstack resolution needs the whole
        // index, and doing it on demand would freeze the app for seconds on a large module.
        self.dwarf.warm_index(ui.ctx());
        crate::hot_call(|| self.render(ui));
    }

    fn input_hook(&mut self, input: &mut RawInput) {
        input.events.retain(|e| {
            // Swallow the release of every press we swallowed, even after we got disabled in
            // between, so the app never sees a release without a matching press.
            if let Event::Key {
                key,
                pressed: false,
                ..
            } = e
                && let Some(index) = self
                    .consumed_keys
                    .iter()
                    .position(|consumed| consumed == key)
            {
                self.consumed_keys.swap_remove(index);
                return false;
            }

            if let Event::Key {
                key: key @ Key::I,
                repeat: false,
                pressed: true,
                modifiers,
                ..
            } = e
                && modifiers.command
            {
                self.enabled = !self.enabled;
                self.consume_key(*key);
                return false;
            }

            if self.enabled {
                let keep = match e {
                    // Ignore clicks
                    Event::PointerButton {
                        pressed, modifiers, ..
                    } => {
                        if *pressed {
                            // ⌘-click pins the widget and opens its live locals; a plain click
                            // opens the source, as it always has.
                            if modifiers.command {
                                self.pin_requested = true;
                            } else {
                                self.clicked = true;
                            }
                        }
                        false
                    }
                    Event::Touch { .. } => {
                        // TODO: handle touch-click
                        false
                    }
                    Event::MouseWheel { delta, unit, .. } => {
                        if *unit == MouseWheelUnit::Line || *unit == MouseWheelUnit::Page {
                            self.selected_widget = self
                                .selected_widget
                                .saturating_add_signed(delta.y.signum() as isize);
                        } else {
                            self.scroll_offset += delta.y;
                            if self.scroll_offset <= -4.0 {
                                self.selected_widget = self.selected_widget.saturating_add(1);
                                self.scroll_offset = 0.0;
                            } else if self.scroll_offset >= 4.0 {
                                self.selected_widget = self.selected_widget.saturating_sub(1);
                                self.scroll_offset = 0.0;
                            }
                        }
                        false
                    }
                    Event::Key {
                        key: Key::Tab,
                        repeat: false,
                        pressed: true,
                        ..
                    } => {
                        // Three verbosity levels for now: app, egui, and std/alloc
                        let config = &mut self.config;
                        if !config.show_egui_frames {
                            config.show_egui_frames = true;
                        } else if !config.show_std_frames {
                            config.show_std_frames = true;
                        } else if !config.show_all_frames {
                            config.show_all_frames = true;
                        } else {
                            config.show_egui_frames = false;
                            config.show_std_frames = false;
                            config.show_all_frames = false;
                        }
                        false
                    }
                    Event::Key {
                        key: Key::Space,
                        pressed: true,
                        ..
                    } => {
                        self.config.clickable_only = !self.config.clickable_only;
                        false
                    }
                    Event::Key {
                        key: Key::Escape,
                        pressed: true,
                        ..
                    } => {
                        self.enabled = false;
                        false
                    }
                    Event::Key {
                        key: Key::ArrowDown,
                        pressed: true,
                        ..
                    } => {
                        self.selected_widget = self.selected_widget.saturating_add(1);
                        false
                    }
                    Event::Key {
                        key: Key::ArrowUp,
                        pressed: true,
                        ..
                    } => {
                        self.selected_widget = self.selected_widget.saturating_sub(1);
                        false
                    }
                    Event::Key {
                        key: Key::Enter,
                        pressed: true,
                        ..
                    } => {
                        self.clicked = true;
                        false
                    }
                    Event::Copy => {
                        self.should_copy_to_clipboard = true;
                        false
                    }
                    Event::Key {
                        key,
                        pressed: true,
                        modifiers,
                        ..
                    } => match *key {
                        key if key >= Key::Num1 && key <= Key::Num9 => {
                            self.open_frame_index = Some(key as usize - Key::Num1 as usize);
                            false
                        }
                        key if key >= Key::A && key <= Key::Z && !modifiers.command => {
                            self.open_frame_index = Some(key as usize - Key::A as usize + 9);
                            false
                        }
                        _ => true,
                    },
                    // The text that the keys we handle would otherwise type
                    Event::Text(_) => false,
                    // Let everything else through
                    _ => true,
                };
                if !keep
                    && let Event::Key {
                        key, pressed: true, ..
                    } = e
                {
                    self.consume_key(*key);
                }
                keep
            } else {
                true
            }
        });
    }

    #[cfg(debug_assertions)]
    fn on_widget_under_pointer(&mut self, _ctx: &Context, widget: &WidgetRect, spacing: &Spacing) {
        if self.config.clickable_only && !widget.sense.senses_click() {
            return;
        }
        // Some widgets call `Context::create_widget` twice, once during creation and once after all of its
        // call because it's the callstack that creates it. The second call contains the final
        // rect but it doesn't matter since we get it at the end of the frame directly from
        // `Context`.
        if let Some(index) = self.widgets.iter().position(|(id, ..)| *id == widget.id) {
            let removed = self.widgets.remove(index);
            self.widgets.push(removed);
            return;
        }
        self.widgets
            .push((widget.id, Callstack::capture(), spacing.clone()));
    }

    /// The pinned widget, sampled from *inside* the call stack that builds it — the only place
    /// its callers' locals are still on the shadow stack. Unlike `on_widget_under_pointer` this
    /// fires wherever the pointer is, so the user can reach into the window and edit.
    #[cfg(debug_assertions)]
    fn on_probed_widget(&mut self, ctx: &Context, widget: &WidgetRect, _spacing: &Spacing) {
        #[cfg(all(feature = "dwarf", target_arch = "wasm32"))]
        crate::hot_call(|| self.dwarf.probed_widget_ui(ctx, widget));
        #[cfg(not(all(feature = "dwarf", target_arch = "wasm32")))]
        let _ = (ctx, widget);
    }
}

impl WidgetInspect {
    pub fn show(ui: &mut Ui) {
        let handle = ui.ctx().plugin::<WidgetInspect>();
        handle.lock().render(ui);
    }

    fn render(&mut self, ui: &mut Ui) {
        let ctx = ui.ctx();
        let &mut Self {
            enabled,
            ref mut selected_widget,
            scroll_offset: _,
            ref mut clicked,
            ref mut should_copy_to_clipboard,
            ref mut widgets,
            ref mut last_top_location,
            ref config,
            ref mut open_frame_index,
            ref mut painted_rects,
            ref mut dwarf,
            pin_requested: _,
            consumed_keys: _,
        } = self;

        if !enabled {
            painted_rects.clear();
            return;
        } else if widgets.is_empty() {
            ctx.set_cursor_icon(CursorIcon::NotAllowed);
            return;
        }

        // Read responses for all widgets under the pointer
        let mut widgets = std::mem::take(widgets)
            .into_iter()
            .filter_map(|(id, callstack, spacing)| {
                let (sense, interact_rect) = ctx
                    .viewport(|viewport| viewport.this_pass.widgets.get(id).copied())
                    .map(|widget| (widget.sense, widget.interact_rect))
                    .unzip();

                ctx.read_response(id)
                    .map(|response| (response.rect, response.layer_id))
                    .map(|(rect, layer)| {
                        (
                            id,
                            callstack,
                            rect,
                            layer,
                            sense,
                            interact_rect.unwrap_or(rect),
                            spacing,
                        )
                    })
            })
            .collect::<Vec<_>>();

        // Transform rects to screen space.
        for (_, _, rect, layer, _, interact_rect, _) in widgets.iter_mut() {
            let transform = ctx.layer_transform_to_global(*layer).unwrap_or_default();
            *rect = transform * *rect;
            *interact_rect = transform * *interact_rect;
        }

        // Sort by area. Does this help?
        widgets.sort_by_key(|(_, _, rect, _, _, _, _)| OrderedFloat(rect.area()));

        // Reset the selected widget if the user moves the pointer to some other part of the UI
        let top_location = widgets.first().map(|(id, ..)| *id);
        if top_location != *last_top_location {
            *selected_widget = 0;
            *last_top_location = top_location;
        }

        *selected_widget = (*selected_widget).clamp(0, widgets.len() - 1);
        let selected = widgets.remove(*selected_widget);
        // The module's own debug info if we have it; the `Error.stack` string parse if we
        // don't — on native, without the feature, or while the index is still being walked.
        let resolved = resolve_dwarf(dwarf, &selected.1).unwrap_or_else(|| selected.1.resolve());

        let filter_frame = |frame: &ParsedFrame| match frame {
            ParsedFrame::Parsed(location) => {
                // Ignore these shims
                !location.symbol.function().contains("vtable.shim") &&
                    config.show_all_frames || // Show all
                    (!location.is_std_code() || config.show_std_frames) &&
                    (!location.is_egui_code() || config.show_egui_frames)
            }
            _ => config.show_all_frames,
        };

        // Copy raw callstack to clipboard
        if *should_copy_to_clipboard {
            log::error!("Copying callstack to clipboard");
            let callstack = resolved
                .iter()
                .map(|frame| match frame {
                    #[cfg(target_arch = "wasm32")]
                    ParsedFrame::Parsed(location) => location.original.to_string(),
                    #[cfg(not(target_arch = "wasm32"))]
                    ParsedFrame::Parsed(location) => format!("{:?}", location.location),
                    ParsedFrame::Failed(error) => format!("Failed to parse frame: {}", error),
                })
                .collect::<Vec<_>>()
                .join("\n");
            ui.copy_text(callstack);
            *should_copy_to_clipboard = false;
        }

        // Find the last egui call after the last user code. Always include this so we know
        // which egui widget is being called.
        let first_frames = resolved
            .iter()
            .position(|frame| {
                filter_frame(frame)
                    && match frame {
                        ParsedFrame::Parsed(location) => location.is_user_code(),
                        _ => false,
                    }
            })
            .map(|i| {
                // If the code calls `Ui::add` (double dispatch), keep two frames
                let mut start = i.saturating_sub(1);
                if resolved[start].is_ui_add() {
                    start = start.saturating_sub(1);
                }
                start..i
            })
            .unwrap_or_default();

        // Filter callstack frames to reduce noise
        let resolved = resolved
            .into_iter()
            .enumerate()
            .filter(|(i, frame)| first_frames.contains(i) || filter_frame(frame))
            .map(|(_, frame)| frame)
            .collect::<Vec<_>>();

        if !resolved.is_empty() {
            ctx.set_cursor_icon(CursorIcon::PointingHand);
        }

        // First user code frame (where the user would want to navigate to)
        let most_significant_frame = resolved
            .iter()
            .position(|frame| match frame {
                ParsedFrame::Parsed(location) => location.is_user_code(),
                _ => false,
            })
            .unwrap_or_default();

        // Handle click/enter to open source of most significant frame, or number key for specific frame
        let frame_to_open = std::mem::take(open_frame_index)
            .or_else(|| std::mem::take(clicked).then_some(most_significant_frame));
        if let Some(frame_idx) = frame_to_open {
            if let Some(frame) = resolved.get(frame_idx).and_then(|frame| match frame {
                ParsedFrame::Parsed(location) => Some(location),
                _ => None,
            }) {
                open_file(ctx, frame.source_location());
                self.enabled = false;
            }
        }

        // ⌘-click: probe this widget from here on. egui then calls `on_probed_widget` from
        // inside its call stack every frame — pointer or no pointer — which is the only place
        // its callers' locals are still live. Turning the picker off hands input back, so the
        // window that appears is one you can actually reach and edit.
        if std::mem::take(&mut self.pin_requested) {
            self.dwarf.pin(ctx, selected.0);
            self.enabled = false;
        }

        let painter = ctx.debug_painter();

        // Darken everything except the selected widget
        let mut bg_rects = vec![ctx.content_rect()];
        cut_rects(&mut bg_rects, selected.2, 0.0);
        for rect in bg_rects {
            painter.rect_filled(rect, 0.0, Color32::from_black_alpha(128));
        }

        // Paint border of non-selected widgets
        let count = widgets.len();
        let opacity = (1.0 / count as f32).max(1.0 / 255.0).min(0.2);
        for (_, _, rect, _, _, _, _) in widgets {
            let stroke = (1.0, Color32::LIGHT_BLUE.gamma_multiply(opacity));
            painter.rect_stroke(rect, 0.0, stroke, StrokeKind::Outside);
        }

        // Paint border of selected widget
        let (id, _, rect, layer_id, _, interact_rect, spacing) = selected;
        let stroke = (1.0, Color32::MAGENTA.gamma_multiply(0.7));
        painter.rect_stroke(interact_rect, 0.0, stroke, StrokeKind::Outside);
        if rect != interact_rect {
            let mut parts = vec![rect];
            cut_rects(&mut parts, interact_rect, 0.5);
            for part in parts {
                painter.add(Shape::dashed_line(
                    &[part.left_top(), part.right_top()],
                    stroke,
                    3.0,
                    3.0,
                ));
                painter.add(Shape::dashed_line(
                    &[part.right_top(), part.right_bottom()],
                    stroke,
                    3.0,
                    3.0,
                ));
                painter.add(Shape::dashed_line(
                    &[part.right_bottom(), part.left_bottom()],
                    stroke,
                    3.0,
                    3.0,
                ));
                painter.add(Shape::dashed_line(
                    &[part.left_bottom(), part.left_top()],
                    stroke,
                    3.0,
                    3.0,
                ));
            }
        }

        let (pointer_pos, shift_pressed) = ctx.input(|i| {
            (
                i.pointer.latest_pos().unwrap_or_default(),
                i.modifiers.shift,
            )
        });
        let dwarf_status = self.dwarf.status();
        *painted_rects = paint_info(
            &painter,
            &config,
            dwarf_status.as_deref(),
            *selected_widget,
            count,
            pointer_pos,
            id,
            layer_id,
            rect,
            &spacing,
            resolved,
            most_significant_frame,
            shift_pressed,
        );
    }
}

fn paint_info(
    painter: &Painter,
    config: &Config,
    // What the DWARF path has to say for itself, if anything — "still indexing", "no module
    // bytes". `None` once it's ready and working, when there's nothing worth the pixels.
    dwarf_status: Option<&str>,
    index: usize,
    count: usize,
    pointer_pos: Pos2,
    id: Id, // TODO: show Id
    layer_id: LayerId,
    rect: Rect,
    spacing: &Spacing,
    callstack: Vec<ParsedFrame>,
    most_significant_frame: usize,
    shift_pressed: bool,
) -> Vec<Rect> {
    #[cfg(not(target_arch = "wasm32"))]
    let _ = shift_pressed;
    let ctx = painter.ctx();

    // Print width and height:
    let text_color = if ctx.global_style().visuals.dark_mode {
        Color32::WHITE
    } else {
        Color32::BLACK
    };
    painter.debug_text(
        rect.left_center() + 4.0 * Vec2::LEFT,
        Align2::RIGHT_CENTER,
        text_color,
        format!("H: {:.1}", rect.height()),
    );
    painter.debug_text(
        rect.center_top() + 3.0 * Vec2::UP,
        Align2::CENTER_BOTTOM,
        text_color,
        format!("W: {:.1}", rect.width()),
    );
    let show_on_left = pointer_pos.x > ctx.content_rect().center().x;

    const SELECTED_MARKER: &str = "⏺";
    const UNSELECTED_MARKER: &str = "⏺";
    const POINTER_OFFSET: Vec2 = vec2(36.0, -48.0);
    const MARGIN: f32 = 8.0;
    const GAP: f32 = 4.0;

    // All text formats (could these be constants?)
    let font = FontId::monospace(10.0);
    let text_color = Color32::WHITE;
    let strong = TextFormat {
        font_id: font.clone(),
        color: text_color,
        valign: Align::Center,
        ..Default::default()
    };
    let weak = TextFormat {
        font_id: font.clone(),
        color: Color32::WHITE.gamma_multiply(0.6),
        valign: Align::Center,
        ..Default::default()
    };
    let space_width = ctx.fonts_mut(|f| f.glyph_width(&strong.font_id, ' '));
    let row_height = ctx.fonts_mut(|f| f.row_height(&strong.font_id));
    let strong_small = TextFormat {
        font_id: FontId::monospace(10.0),
        color: text_color,
        line_height: Some(row_height),
        valign: Align::Center,
        ..Default::default()
    };
    let weak_small = TextFormat {
        font_id: FontId::monospace(10.0),
        color: Color32::WHITE.gamma_multiply(0.6),
        line_height: Some(row_height),
        valign: Align::Center,
        ..Default::default()
    };
    let selected_marker_format = TextFormat {
        font_id: FontId::monospace(8.0),
        valign: Align::Center,
        line_height: Some(row_height + 0.0),
        color: Color32::MAGENTA,
        ..Default::default()
    };
    let unselected_marker_format = TextFormat {
        color: Color32::DARK_GRAY,
        ..selected_marker_format.clone()
    };

    // Pack everything in one layout job
    let mut header_job = LayoutJob::default();
    {
        let stroke = Stroke::new(1.0, strong_small.color);
        header_job.append(&format!("Widget "), 0.0, weak_small.clone());
        header_job.append(
            &format!("{:?} ", id.short_debug_format()),
            0.0,
            strong_small.clone(),
        );
        header_job.append(&format!("Layer "), 0.0, weak_small.clone());
        header_job.append(
            &format!(
                "{:?} {:?}",
                layer_id.order,
                layer_id.id.short_debug_format()
            ),
            0.0,
            strong_small.clone(),
        );
        header_job.append(&format!("#{index}"), 0.0, strong_small.clone());
        header_job.append(&format!(" of {count}"), 0.0, weak_small.clone());
        header_job.append("     Scroll or ↑↓ to select\n", 0.0, weak_small.clone());
        header_job.append("Source ", 0.0, weak_small.clone());
        header_job.append(
            "APP",
            0.0,
            TextFormat {
                underline: stroke,
                ..strong_small.clone()
            },
        );
        header_job.append(" ", 0.0, weak_small.clone());
        header_job.append(
            "EGUI",
            0.0,
            TextFormat {
                underline: config.show_egui_frames.then(|| stroke).unwrap_or_default(),
                ..strong_small.clone()
            },
        );
        header_job.append(" ", 0.0, weak_small.clone());
        header_job.append(
            "STD",
            0.0,
            TextFormat {
                underline: config.show_std_frames.then(|| stroke).unwrap_or_default(),
                ..strong_small.clone()
            },
        );
        header_job.append(" ", 0.0, weak_small.clone());
        header_job.append(
            "ALL",
            0.0,
            TextFormat {
                underline: config.show_all_frames.then(|| stroke).unwrap_or_default(),
                ..strong_small.clone()
            },
        );
        header_job.append("  Tab to cycle\n", 0.0, weak_small.clone());
        header_job.append("Sense  ", 0.0, weak_small.clone());
        header_job.append(
            "CLICK",
            0.0,
            TextFormat {
                underline: config.clickable_only.then(|| stroke).unwrap_or_default(),
                ..strong_small.clone()
            },
        );
        header_job.append(" ", 0.0, weak_small.clone());
        header_job.append(
            "ANY",
            0.0,
            TextFormat {
                underline: (!config.clickable_only).then(|| stroke).unwrap_or_default(),
                ..strong_small.clone()
            },
        );
        header_job.append("  Space to toggle", 0.0, weak_small.clone());
        header_job.append("\nOpen   ", 0.0, weak_small.clone());
        header_job.append("CLICK", 0.0, strong_small.clone());
        header_job.append(" source   ", 0.0, weak_small.clone());
        header_job.append("⌘CLICK", 0.0, strong_small.clone());
        header_job.append(" live locals", 0.0, weak_small.clone());
        if let Some(status) = dwarf_status {
            header_job.append(&format!("\n{status}"), 0.0, weak_small.clone());
        }
    }

    // Maps a frame to a string/format to be shown on the left side. When `shift_pressed` is true
    // (wasm32 only), we show the original, unparsed stack trace line instead of the resolved symbol.
    let left_side = |frame: &ParsedFrame| match frame {
        ParsedFrame::Parsed(location) => {
            let format = if location.is_user_code() {
                strong.clone()
            } else {
                weak.clone()
            };
            #[cfg(target_arch = "wasm32")]
            if shift_pressed {
                return (location.original.clone(), format);
            }
            let indent = if location.inlined { "  " } else { "" };
            let max_function_len = 80usize
                .saturating_sub(location.symbol.type_().len())
                .max(10);

            const ELLIPSIS: &str = "…";
            let function = if location.symbol.function().len() > max_function_len {
                location
                    .symbol
                    .function()
                    .chars()
                    .take(max_function_len - 1)
                    .collect::<String>()
                    + &ELLIPSIS
            } else {
                location.symbol.function().to_string()
            };
            (
                format!("{}{}::{}", indent, location.symbol.type_(), function,),
                format,
            )
        }
        ParsedFrame::Failed(text) => (
            format!("! {}", text.chars().take(800).collect::<String>()),
            weak.clone(),
        ),
    };

    // Maps a frame to a string/format to be shown on the right side
    let right_side = |frame: &ParsedFrame| match frame {
        ParsedFrame::Parsed(resolved) => {
            let format = if resolved.is_user_code() {
                strong_small.clone()
            } else {
                weak_small.clone()
            };
            let location = resolved.source_location();
            (
                format!(
                    " {}/{}:{}",
                    resolved.symbol.crate_(),
                    location.filename(),
                    location.line,
                ),
                format,
            )
        }
        _ => ("-".to_string(), weak.clone()),
    };

    // Collect the left and right sides
    let columns = callstack
        .iter()
        .map(|frame| (left_side(frame), right_side(frame)));

    // Build the left and right columns layout jobs
    let mut left_job = LayoutJob::default();
    let mut right_job = LayoutJob::default();
    let row_count = callstack.len();
    let number_format = TextFormat {
        font_id: FontId::monospace(10.0),
        color: Color32::YELLOW,
        valign: Align::Center,
        line_height: Some(row_height),
        ..Default::default()
    };
    for (i, ((left, left_format), (right, right_format))) in columns.into_iter().enumerate() {
        // Show 1-9 then a-z for quick navigation
        let label = if i < 9 {
            format!("{}", i + 1)
        } else if i < 9 + 26 {
            format!("{}", (b'a' + (i - 9) as u8) as char)
        } else {
            " ".to_string()
        };
        left_job.append(&label, 0.0, number_format.clone());
        if i == most_significant_frame {
            left_job.append(SELECTED_MARKER, space_width, selected_marker_format.clone());
        } else if callstack[i].is_user_code() {
            left_job.append(
                UNSELECTED_MARKER,
                space_width,
                unselected_marker_format.clone(),
            );
        } else {
            left_job.append(
                UNSELECTED_MARKER,
                space_width,
                TextFormat {
                    color: Color32::TRANSPARENT,
                    ..selected_marker_format.clone()
                },
            );
        };
        left_job.append(&left, space_width, left_format.clone());
        right_job.append(&right, 0.0, right_format.clone());
        if i < row_count - 1 {
            left_job.append("\n", 0.0, left_format.clone());
            right_job.append("\n", 0.0, right_format.clone());
        }
    }

    #[cfg(target_arch = "wasm32")]
    if row_count == 0 {
        left_job.append(
            "Callstack was not available or we couldn't parse Rust symbols from it.\n",
            0.0,
            weak_small.clone(),
        );
        left_job.append(
            "Consider setting up `wasm-stack-trace` to get proper Rust symbols.\n\n",
            0.0,
            weak_small.clone(),
        );
        left_job.append("  More info: ", 0.0, weak_small.clone());
        left_job.append(
            "https://github.com/membrane-io/wasm-stack-trace\n",
            0.0,
            strong_small.clone(),
        );
    }

    let header_galley = painter.layout_job(header_job);
    let left_galley = painter.layout_job(left_job);
    let right_galley = painter.layout_job(right_job);

    let header_size = header_galley.size() + 2.0 * Vec2::splat(MARGIN);
    let left_size = left_galley.size() + 2.0 * Vec2::splat(MARGIN);
    let right_size = right_galley.size() + 2.0 * Vec2::splat(MARGIN);
    let body_size = vec2(left_size.x + right_size.x, left_size.y.max(right_size.y));

    let total_size = vec2(
        header_size.x.max(body_size.x),
        header_size.y + body_size.y + GAP,
    );
    let mut bounds = Rect::from_min_size(pointer_pos + POINTER_OFFSET, total_size);

    // Use the side of the screen with more space
    if show_on_left {
        bounds = bounds.translate(vec2(-total_size.x, 0.0) - 2.0 * POINTER_OFFSET);
    };

    // Don't go above the top of the screen. If possible, don't go below the bottom of the screen.
    let screen_rect = ctx.content_rect();
    if bounds.bottom() > screen_rect.bottom() {
        bounds = bounds.translate(-vec2(
            0.0,
            (bounds.bottom() - screen_rect.bottom()).min(bounds.top()),
        ));
    }

    // Compute rects for each part
    let header_rect = Align2::LEFT_TOP.align_size_within_rect(header_size, bounds);
    let body_rect = Align2::LEFT_BOTTOM.align_size_within_rect(body_size, bounds);
    let left_rect = Align2::LEFT_BOTTOM.align_size_within_rect(left_size, bounds);
    let right_rect = Align2::RIGHT_BOTTOM.align_size_within_rect(right_size, bounds);

    // Paint background rects
    let bg_fill = Color32::from_black_alpha(180);
    let bg_stroke = Stroke::new(1.0, Color32::WHITE.gamma_multiply(0.1));
    painter.rect(body_rect, 0.0, bg_fill, bg_stroke, StrokeKind::Outside);
    painter.rect(header_rect, 0.0, bg_fill, bg_stroke, StrokeKind::Outside);

    // Paint text
    painter.galley(
        left_rect.left_top() + Vec2::splat(MARGIN),
        left_galley,
        text_color,
    );
    painter.galley(
        right_rect.left_top() + Vec2::splat(MARGIN),
        right_galley,
        text_color,
    );

    painter.galley(
        header_rect.left_top() + Vec2::splat(MARGIN),
        header_galley,
        text_color,
    );

    let spacing_rect = paint_spacing_box(painter, spacing, rect);

    vec![header_rect, body_rect, spacing_rect]
}

/// Paint the `Ui` spacing of the selected widget in a box adjacent to it.
///
/// Tries each side of the widget in order (top, right, bottom, left) and uses the first one with
/// enough room within the viewport. The box is always clamped to stay inside the viewport.
fn paint_spacing_box(painter: &Painter, spacing: &Spacing, widget_rect: Rect) -> Rect {
    const MARGIN: f32 = 8.0;
    const GAP: f32 = 4.0;

    let font = FontId::monospace(10.0);
    let strong = TextFormat {
        font_id: font.clone(),
        color: Color32::WHITE,
        valign: Align::Center,
        ..Default::default()
    };
    let weak = TextFormat {
        font_id: font,
        color: Color32::WHITE.gamma_multiply(0.6),
        valign: Align::Center,
        ..Default::default()
    };

    let rows = [
        ("interact size:", spacing.interact_size),
        ("item spacing:", spacing.item_spacing),
    ];

    // Pad labels and values so the values align horizontally (the font is monospace)
    let label_width = rows.iter().map(|(label, _)| label.len()).max().unwrap_or(0);
    let x_values = rows.map(|(_, value)| format!("{:.1}", value.x));
    let y_values = rows.map(|(_, value)| format!("{:.1}", value.y));
    let x_width = x_values.iter().map(|value| value.len()).max().unwrap_or(0);
    let y_width = y_values.iter().map(|value| value.len()).max().unwrap_or(0);

    let mut job = LayoutJob::default();
    for (i, (label, _)) in rows.iter().enumerate() {
        job.append(&format!("{label:<label_width$} "), 0.0, weak.clone());
        job.append(
            &format!("{:>x_width$} x {:>y_width$}", x_values[i], y_values[i]),
            0.0,
            strong.clone(),
        );
        if i + 1 < rows.len() {
            job.append("\n", 0.0, strong.clone());
        }
    }

    let galley = painter.layout_job(job);
    let size = galley.size() + 2.0 * Vec2::splat(MARGIN);

    let viewport = painter.ctx().viewport_rect();
    let center = widget_rect.center();

    // Anchor point on each side. We pick the first side (top, right, bottom, left) that has enough
    // room within the viewport for the box.
    let top = (widget_rect.top() - viewport.top() >= size.y + GAP)
        .then(|| pos2(center.x - size.x / 2.0, widget_rect.top() - GAP - size.y));
    let right = (viewport.right() - widget_rect.right() >= size.x + GAP)
        .then(|| pos2(widget_rect.right() + GAP, center.y - size.y / 2.0));
    let bottom = (viewport.bottom() - widget_rect.bottom() >= size.y + GAP)
        .then(|| pos2(center.x - size.x / 2.0, widget_rect.bottom() + GAP));
    let left = (widget_rect.left() - viewport.left() >= size.x + GAP)
        .then(|| pos2(widget_rect.left() - GAP - size.x, center.y - size.y / 2.0));

    // Fall back to the top edge of the widget if no side has enough room.
    let min = top
        .or(right)
        .or(bottom)
        .or(left)
        .unwrap_or_else(|| pos2(center.x - size.x / 2.0, widget_rect.top() - GAP - size.y));

    // Clamp so the box always stays fully within the viewport.
    let min = pos2(
        min.x.clamp(viewport.left(), viewport.right() - size.x),
        min.y.clamp(viewport.top(), viewport.bottom() - size.y),
    );
    let box_rect = Rect::from_min_size(min, size);

    let bg_fill = Color32::from_black_alpha(180);
    let bg_stroke = Stroke::new(1.0, Color32::WHITE.gamma_multiply(0.1));
    painter.rect(box_rect, 0.0, bg_fill, bg_stroke, StrokeKind::Outside);
    painter.galley(
        box_rect.left_top() + Vec2::splat(MARGIN),
        galley,
        Color32::WHITE,
    );

    box_rect
}

/// Given a list of rects, cut a hole in them. In other words, any rect that intersects with the hole is replaced with
/// multiple smaller ones that don't intercept.
pub fn cut_rects(rects: &mut Vec<Rect>, hole: Rect, mut min_side: f32) {
    const EPSILON: f32 = 0.001;
    min_side = min_side.max(EPSILON);

    let big_enough = |rect: &Rect| -> bool {
        let Vec2 { x, y } = rect.size();
        x >= min_side && y >= min_side
    };
    let mut i = rects.len();
    while i > 0 {
        i -= 1;
        let rect = rects[i];
        // Rect representing the overlap with the hole
        let overlap = rect.intersect(hole);

        if big_enough(&overlap) {
            // Remove the current rect and replace with smaller ones. Note that the smaller ones might
            // still intersect with the hole but they are added to the list to be potentially split again.
            rects.swap_remove(i);

            // Left-top rect
            if overlap.min.y > rect.min.y {
                let lt = Rect {
                    min: rect.min,
                    max: overlap.right_top(),
                };

                if big_enough(&lt) {
                    rects.push(lt);
                }
            }

            // Right-top rect
            if overlap.max.x < rect.max.x {
                let rt = Rect {
                    min: Pos2 {
                        x: overlap.max.x,
                        y: rect.min.y,
                    },
                    max: Pos2 {
                        x: rect.max.x,
                        y: overlap.max.y,
                    },
                };
                if big_enough(&rt) {
                    rects.push(rt);
                }
            }

            // Right-bottom rect
            if overlap.max.y < rect.max.y {
                let rb = Rect {
                    min: overlap.left_bottom(),
                    max: rect.max,
                };
                if big_enough(&rb) {
                    rects.push(rb);
                }
            }

            // Left-bottom rect
            if overlap.min.x > rect.min.x {
                let lb = Rect {
                    min: Pos2 {
                        x: rect.min.x,
                        y: overlap.min.y,
                    },
                    max: Pos2 {
                        x: overlap.min.x,
                        y: rect.max.y,
                    },
                };
                if big_enough(&lb) {
                    rects.push(lb);
                }
            }
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
struct Callstack(Vec<backtrace::Frame>);

#[cfg(not(target_arch = "wasm32"))]
impl Callstack {
    fn capture() -> Self {
        let mut frames = Vec::new();
        backtrace::trace(|frame| {
            frames.push(frame.clone());
            true
        });
        Callstack(frames)
    }

    fn resolve(&self) -> Vec<ParsedFrame> {
        let mut parsed_frames = Vec::new();
        for frame in &self.0 {
            let mut count = 0;
            backtrace::resolve_frame(frame, |resolved| {
                let Some(path) = resolved.filename().map(|path| path.to_string_lossy()) else {
                    parsed_frames.push(ParsedFrame::Failed(
                        resolved
                            .name()
                            .map_or_else(|| "<unknown>".to_string(), |name| name.to_string()),
                    ));
                    return;
                };
                let line = resolved.lineno().map(|line| line as usize);
                let column = resolved.colno().map(|col| col as usize);
                let inlined = count > 0;
                let name = resolved
                    .name()
                    .map(|name| name.to_string())
                    .unwrap_or_default();
                parsed_frames.push(ParsedFrame::Parsed(ResolvedFrame {
                    symbol: Symbol::parse(&name),
                    location: SourceLocation {
                        path: path.into_owned(),
                        line: line.unwrap_or(0),
                        column: column.unwrap_or(0),
                    },
                    inlined,
                }));
                count += 1;
            });
        }
        parsed_frames
    }
}

#[cfg(target_arch = "wasm32")]
struct Callstack(js_sys::Error);

#[expect(unsafe_code)]
#[cfg(target_arch = "wasm32")]
#[cfg(not(target_feature = "atomics"))]
// Safety: if there's no atomics, there's no multithreading
unsafe impl Sync for Callstack {}

#[expect(unsafe_code)]
#[cfg(target_arch = "wasm32")]
#[cfg(not(target_feature = "atomics"))]
// Safety: if there's no atomics, there's no multithreading
unsafe impl Send for Callstack {}

#[cfg(target_arch = "wasm32")]
impl Callstack {
    fn capture() -> Self {
        Callstack(js_sys::Error::new(""))
    }

    /// The underlying JS `Error`, for the DWARF path to read `CallSite.getPosition()` off.
    ///
    /// Only valid while `.stack` has never been read *as a string*: the first read is what
    /// runs `prepareStackTrace`, and V8 memoizes whatever it returned. So a capture is
    /// resolved one way or the other, never both.
    #[cfg(feature = "dwarf")]
    fn js_error(&self) -> wasm_bindgen::JsValue {
        self.0.clone().into()
    }

    /// Get the raw stack trace as a string (i.e. without parsing). Note that reading `Error.stack`
    /// is CPU intensive and can take several milliseconds to complete , due DWARF parsing and
    /// formatting.
    fn raw(&self) -> Option<String> {
        js_sys::Reflect::get(&self.0, &wasm_bindgen::JsValue::from_str("stack"))
            .ok()
            .and_then(|stack| stack.as_string())
    }

    fn resolve(&self) -> Vec<ParsedFrame> {
        let stack = self.raw().unwrap_or_default();
        stack
            .split("\n")
            .filter_map(|original| {
                let Some((_, rest)) = original.split_once(" at ") else {
                    return None;
                };
                let Some((symbol, rest)) = rest.rsplit_once(" (") else {
                    return Some(ParsedFrame::Failed(original.to_owned()));
                };
                let (symbol, inlined) = if symbol.ends_with(" [inlined]") {
                    (symbol.trim_end_matches(" [inlined]"), true)
                } else {
                    (symbol, false)
                };
                let symbol = Symbol::parse(symbol);
                if symbol.type_() == "<unknown-type>" {
                    return Some(ParsedFrame::Failed(original.to_owned()));
                }
                let Some((path, rest)) = rest.split_once(":") else {
                    return Some(ParsedFrame::Failed(original.to_owned()));
                };
                if !path.ends_with(".rs") {
                    return Some(ParsedFrame::Failed(original.to_owned()));
                }
                let Some((line, rest)) = rest.split_once(":") else {
                    return Some(ParsedFrame::Failed(original.to_owned()));
                };
                let Some((column, _rest)) = rest.split_once(")") else {
                    return Some(ParsedFrame::Failed(original.to_owned()));
                };

                Some(ParsedFrame::Parsed(ResolvedFrame {
                    symbol,
                    location: SourceLocation {
                        path: path.to_owned(),
                        line: line.parse().unwrap_or(0),
                        column: column.parse().unwrap_or(0),
                    },
                    inlined,
                    original: original.to_owned(),
                }))
            })
            .collect::<Vec<_>>()
    }
}

//
