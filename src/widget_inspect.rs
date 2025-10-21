use egui::emath::{Align2, OrderedFloat, Pos2, Rect, Vec2, vec2};
use egui::epaint::{
    Color32, FontId, Stroke, StrokeKind,
    text::{LayoutJob, TextFormat},
};
use egui::{Align, Context, CursorIcon, Event, Id, Key, Painter, Plugin, RawInput, WidgetRect};

impl Plugin for WidgetInspect {
    fn debug_name(&self) -> &'static str {
        "WidgetInspectPlugin"
    }

    fn on_end_pass(&mut self, ctx: &Context) {
        let &mut Self {
            enabled,
            ref mut selected_widget,
            scroll_offset: _,
            ref mut clicked,
            ref mut widgets,
            ref mut last_top_location,
            ref config,
        } = self;

        if !enabled {
            return;
        } else if widgets.is_empty() {
            ctx.set_cursor_icon(CursorIcon::NotAllowed);
            return;
        }

        // Read responses for all widgets under the pointer
        let mut widgets = std::mem::take(widgets)
            .into_iter()
            .filter_map(|(id, callstack)| {
                ctx.read_response(id)
                    .map(|response| (response.rect, response.layer_id))
                    .map(|(rect, layer)| (id, callstack, rect, layer))
            })
            .collect::<Vec<_>>();

        // Consider layer transforms
        for (_, _, rect, layer) in widgets.iter_mut() {
            *rect = ctx.layer_transform_to_global(*layer).unwrap_or_default() * *rect;
        }

        // Sort by area. Does this help?
        widgets.sort_by_key(|(_, _, rect, _)| OrderedFloat(rect.area()));

        // Reset the selected widget if the user moves the pointer to some other part of the UI
        let top_location = widgets.first().map(|(id, _, _, _)| *id);
        if top_location != *last_top_location {
            *selected_widget = 0;
            *last_top_location = top_location;
        }

        *selected_widget = (*selected_widget).clamp(0, widgets.len() - 1);
        let selected = widgets.remove(*selected_widget);
        let resolved = selected.1.resolve();

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

        // Handle click to open source of most significant frame
        if std::mem::take(clicked) {
            if let Some(location) =
                resolved
                    .get(most_significant_frame)
                    .and_then(|frame| match frame {
                        ParsedFrame::Parsed(location) => Some(location),
                        _ => None,
                    })
            {
                match config
                    .file_opener
                    .as_ref()
                    .map(|file_opener| file_opener(ctx, location))
                {
                    Some(Ok(())) => {
                        self.enabled = false;
                    }
                    Some(Err(err)) => {
                        log::error!("ERROR: opening source: {:?}", err);
                    }
                    None => {}
                }
            }
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
        for (_, _, rect, _) in widgets {
            let stroke = (1.0, Color32::LIGHT_BLUE.gamma_multiply(opacity));
            painter.rect_stroke(rect, 0.0, stroke, StrokeKind::Outside);
        }

        // Paint border of selected widget
        let (id, _, rect, _) = selected;
        let stroke = (1.0, Color32::MAGENTA.gamma_multiply(0.7));
        painter.rect_stroke(rect, 0.0, stroke, StrokeKind::Outside);

        let pointer_pos = ctx.input(|i| i.pointer.latest_pos().unwrap_or_default());
        paint_info(
            &painter,
            &config,
            *selected_widget,
            count,
            pointer_pos,
            id,
            rect,
            resolved,
            most_significant_frame,
        );
    }

    fn input_hook(&mut self, input: &mut RawInput) {
        for event in input.events.iter() {
            match event {
                Event::Key {
                    key: Key::I,
                    repeat: false,
                    pressed: true,
                    modifiers,
                    ..
                } if modifiers.command => {
                    self.enabled = !self.enabled;
                }
                _ => {}
            }
        }
        if self.enabled {
            input.events.retain(|e| {
                match e {
                    // Ignore clicks
                    Event::PointerButton { pressed, .. } => {
                        if *pressed {
                            self.clicked = true;
                        }
                        false
                    }
                    Event::Touch { .. } => {
                        // TODO: handle touch-click
                        false
                    }
                    Event::MouseWheel { delta, .. } => {
                        self.scroll_offset += delta.y;
                        if self.scroll_offset <= -4.0 {
                            self.selected_widget = self.selected_widget.saturating_add(1);
                            self.scroll_offset = 0.0;
                        } else if self.scroll_offset >= 4.0 {
                            self.selected_widget = self.selected_widget.saturating_sub(1);
                            self.scroll_offset = 0.0;
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
                        key: Key::Escape,
                        pressed: true,
                        ..
                    } => {
                        self.enabled = false;
                        false
                    }
                    Event::Key {
                        key: Key::J | Key::ArrowDown,
                        pressed: true,
                        ..
                    } => {
                        self.selected_widget = self.selected_widget.saturating_add(1);
                        false
                    }
                    Event::Key {
                        key: Key::K | Key::ArrowUp,
                        pressed: true,
                        ..
                    } => {
                        self.selected_widget = self.selected_widget.saturating_sub(1);
                        false
                    }
                    // Let everything else through
                    _ => true,
                }
            });
        }
    }

    #[cfg_attr(not(debug_assertions), expect(dead_code))]
    fn on_widget_under_pointer(&mut self, _ctx: &Context, widget: &WidgetRect) {
        // Some widgets call `Context::create_widget` twice, once during creation and once after all of its
        // call because it's the callstack that creates it. The second call contains the final
        // rect but it doesn't matter since we get it at the end of the frame directly from
        // `Context`.
        if let Some(index) = self.widgets.iter().position(|(id, _)| *id == widget.id) {
            let removed = self.widgets.remove(index);
            self.widgets.push(removed);
            return;
        }
        self.widgets.push((widget.id, Callstack::capture()));
    }
}

pub type FileOpener = Box<dyn Fn(&Context, &SourceLocation) -> Result<(), String> + Send + Sync>;

pub struct Config {
    /// How to open the source code.
    file_opener: Option<FileOpener>,

    /// Whether to show the egui stack frames.
    show_egui_frames: bool,

    /// Whether to show std/alloc stack frames.
    show_std_frames: bool,

    /// Whether to show all other stack frames including JavaScript and unparsed frames.
    show_all_frames: bool,
}

impl Config {
    pub fn new(file_opener: Option<FileOpener>) -> Self {
        Self {
            file_opener,
            show_egui_frames: false,
            show_std_frames: false,
            show_all_frames: false,
        }
    }
}

impl std::fmt::Debug for Config {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Config")
            .field("show_egui_frames", &self.show_egui_frames)
            .field("show_std_frames", &self.show_std_frames)
            .field("show_all_frames", &self.show_all_frames)
            .finish()
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            file_opener: None,
            show_egui_frames: false,
            show_std_frames: false,
            show_all_frames: false,
        }
    }
}

pub struct WidgetInspect {
    /// Configuration
    config: Config,

    /// Whether the widget inspect is enabled.
    enabled: bool,

    /// The index of the selected widget. Used to navigate the callstacks with the mouse wheel.
    selected_widget: usize,

    /// The offset of the scroll wheel. Used to navigate the callstacks with the mouse wheel.
    scroll_offset: f32,

    /// Whether the user just clicked. We need to track this separately from normal
    /// `Response::clicked` as to not interfere with normal widget interactions.
    clicked: bool,

    /// Captured callstacks for widgets under the pointer.
    widgets: Vec<(Id, Callstack)>,

    /// The top location of the widget under the pointer. Used to reset the selected widget if the user moves the pointer to some other part of the UI.
    last_top_location: Option<Id>,
}

#[derive(Debug, Clone)]
pub struct Symbol(String);

impl Symbol {
    pub fn function(&self) -> &str {
        self.0
            .rsplit("::")
            .skip_while(|s| *s == "λ")
            .next()
            .map(|s| s.as_ptr() as usize - self.0.as_ptr() as usize)
            .map(|offset| &self.0[offset..])
            .unwrap_or("")
    }

    pub fn type_(&self) -> &str {
        if self.0.starts_with("<") {
            // Trait functions are formatted as "<mod::mod::Type as Trait>::function"
            self.0
                .split_once(" as ")
                .and_then(|(left, _)| {
                    left.rsplit("::").skip_while(|s| *s == "λ").nth(1)
                    // .map(|s| s.as_ptr() as usize - left.as_ptr() as usize)
                    // .map(|offset| &left[offset..])
                })
                .unwrap_or("")
        } else {
            self.0
                .rsplit("::")
                .skip_while(|s| *s == "λ")
                .nth(1)
                .unwrap_or("")
        }
    }

    pub fn crate_(&self) -> &str {
        self.0
            .trim_start_matches('<')
            .trim_start_matches("dyn ")
            .split("::")
            .next()
            .unwrap_or("")
    }
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub symbol: Symbol,
    pub path: String,
    pub line: usize,
    pub column: usize,
    pub inlined: bool,
}

impl SourceLocation {
    fn filename(&self) -> &str {
        self.path.rsplit('/').next().unwrap_or(self.path.as_str())
    }

    fn is_user_code(&self) -> bool {
        !self.is_std_code() && !self.is_egui_code()
    }

    fn is_egui_code(&self) -> bool {
        let crate_ = self.symbol.crate_();
        crate_ == "egui"
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
    }
}

/// A callstack frame that has been mapped to a location in the source code. Note that a single
/// frame of the original callstack can be mapped to multiple locations in the source code due to
/// inlining.
#[derive(Debug)]
enum ParsedFrame {
    Parsed(SourceLocation),
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

impl WidgetInspect {
    pub fn new(config: Config) -> Self {
        WidgetInspect {
            config,
            enabled: false,
            selected_widget: 0,
            scroll_offset: 0.0,
            clicked: false,
            widgets: vec![],
            last_top_location: None,
        }
    }
}

fn paint_info(
    painter: &Painter,
    config: &Config,
    index: usize,
    count: usize,
    pointer_pos: Pos2,
    id: Id, // TODO: show Id
    rect: Rect,
    callstack: Vec<ParsedFrame>,
    most_significant_frame: usize,
) {
    let ctx = painter.ctx();

    // Print width and height:
    let text_color = if ctx.style().visuals.dark_mode {
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
    let font = FontId::monospace(11.0);
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
        header_job.append(&format!("{:?} ", id), 0.0, strong_small.clone());
        header_job.append(&format!("#{index}"), 0.0, strong_small.clone());
        header_job.append(&format!(" of {count}"), 0.0, weak_small.clone());
        header_job.append("     Scroll/J/K to select\n", 0.0, weak_small.clone());
        header_job.append("Filter ", 0.0, weak_small.clone());
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
        header_job.append("  Tab to cycle", 0.0, weak_small.clone());
    }

    // Maps a frame to a string/format to be shown on the left side
    let left_side = |frame: &ParsedFrame| match frame {
        ParsedFrame::Parsed(location) => {
            let format = if location.is_user_code() {
                strong.clone()
            } else {
                weak.clone()
            };
            let indent = if location.inlined { "  " } else { "" };
            (
                format!(
                    "{}{}::{}",
                    indent,
                    location.symbol.type_(),
                    location.symbol.function()
                ),
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
        ParsedFrame::Parsed(location) => {
            let format = if location.is_user_code() {
                strong_small.clone()
            } else {
                weak_small.clone()
            };
            (
                format!(
                    " {}/{}:{}",
                    location.symbol.crate_(),
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
    for (i, ((left, left_format), (right, right_format))) in columns.into_iter().enumerate() {
        if i == most_significant_frame {
            left_job.append(SELECTED_MARKER, 0.0, selected_marker_format.clone());
        } else if callstack[i].is_user_code() {
            left_job.append(UNSELECTED_MARKER, 0.0, unselected_marker_format.clone());
        } else {
            left_job.append(
                UNSELECTED_MARKER,
                0.0,
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
                let Some(name) = resolved.name().map(|name| {
                    let full = name.to_string();
                    full.rsplit_once("::")
                        .filter(|(_, hash)| {
                            hash.starts_with("h")
                                && hash[1..].chars().all(|c| c.is_ascii_hexdigit())
                        })
                        .map(|(left, _hash)| left.to_string())
                        .unwrap_or(full)
                }) else {
                    return;
                };
                let Some(path) = resolved.filename().map(|path| path.to_string_lossy()) else {
                    parsed_frames.push(ParsedFrame::Failed(name));
                    return;
                };
                let line = resolved.lineno().map(|line| line as usize);
                let column = resolved.colno().map(|col| col as usize);
                let inlined = count > 0;
                parsed_frames.push(ParsedFrame::Parsed(SourceLocation {
                    symbol: Symbol(name.replace("{{closure}}", "λ")),
                    path: path.into_owned(),
                    line: line.unwrap_or(0),
                    column: column.unwrap_or(0),
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
            .filter_map(|line| {
                let Some((_, rest)) = line.split_once(" at ") else {
                    return None;
                };
                let Some((symbol, rest)) = rest.split_once(" (") else {
                    return Some(ParsedFrame::Failed(line.to_owned()));
                };
                let Some((path, rest)) = rest.split_once(":") else {
                    return Some(ParsedFrame::Failed(line.to_owned()));
                };
                if !path.ends_with(".rs") {
                    return Some(ParsedFrame::Failed(line.to_owned()));
                }
                let Some((line, rest)) = rest.split_once(":") else {
                    return Some(ParsedFrame::Failed(line.to_owned()));
                };
                let Some((column, rest)) = rest.split_once(")") else {
                    return Some(ParsedFrame::Failed(line.to_owned()));
                };
                let inlined = rest.contains("inlined");
                Some(ParsedFrame::Parsed(SourceLocation {
                    symbol: Symbol(symbol.replace("{{closure}}", "λ")),
                    path: path.to_owned(),
                    line: line.parse().unwrap_or(0),
                    column: column.parse().unwrap_or(0),
                    inlined,
                }))
            })
            .collect::<Vec<_>>()
    }
}
