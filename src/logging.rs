use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt;

use egui::Label;
use egui_tracing::tracing_subscriber::prelude::*;

const MAX_LOG_EVENTS: usize = 5_000;

hot_static::hot_static!(static EVENT_COLLECTOR: egui_tracing::EventCollector);
hot_static::hot_static!(static LOG_CONTEXT: RefCell<Vec<(&'static str, ContextValue)>>);

/// The global event collector. Lazily initialized on first access and recovered
/// after a subsecond hot-patch via [`hot_static`].
pub fn event_collector() -> &'static egui_tracing::EventCollector {
    EVENT_COLLECTOR.get_or_init(|| {
        egui_tracing::EventCollector::default().with_max_events(Some(MAX_LOG_EVENTS))
    })
}

fn log_context() -> &'static RefCell<Vec<(&'static str, ContextValue)>> {
    LOG_CONTEXT.get_or_init(|| RefCell::new(Vec::new()))
}

#[derive(Clone)]
pub enum ContextValue {
    EguiId(u64),
    U64(u64),
    Str(Cow<'static, str>),
}

impl fmt::Display for ContextValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EguiId(v) => write!(f, "{:04X}", *v as u16),
            Self::U64(v) => write!(f, "{v}"),
            Self::Str(s) => f.write_str(s),
        }
    }
}

impl From<egui::Id> for ContextValue {
    fn from(id: egui::Id) -> Self {
        Self::EguiId(id.value())
    }
}

impl From<u64> for ContextValue {
    fn from(v: u64) -> Self {
        Self::U64(v)
    }
}

impl From<&'static str> for ContextValue {
    fn from(s: &'static str) -> Self {
        Self::Str(Cow::Borrowed(s))
    }
}

impl From<String> for ContextValue {
    fn from(s: String) -> Self {
        Self::Str(Cow::Owned(s))
    }
}

pub struct LogScope {
    _private: (),
}

impl LogScope {
    pub fn new(key: &'static str, value: impl Into<ContextValue>) -> Self {
        log_context().borrow_mut().push((key, value.into()));
        Self { _private: () }
    }

    pub fn id(id: egui::Id) -> Self {
        Self::new("id", id)
    }
}

impl Drop for LogScope {
    fn drop(&mut self) {
        log_context().borrow_mut().pop();
    }
}

fn drain_context_fields(fields: &mut Vec<(String, String)>) {
    for (key, value) in log_context().borrow().iter() {
        fields.push(((*key).to_owned(), value.to_string()));
    }
}

struct MultiLogger {
    inner: Box<dyn log::Log>,
}

impl log::Log for MultiLogger {
    fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
        self.inner.enabled(metadata)
    }

    fn log(&self, record: &log::Record<'_>) {
        self.inner.log(record);

        let mut event = egui_tracing::tracing::CollectedEvent::from_log_record(record);
        drain_context_fields(&mut event.fields);
        event_collector().collect(event);
    }

    fn flush(&self) {
        self.inner.flush();
    }
}

pub fn init(inner_logger: Box<dyn log::Log>) {
    let multi = MultiLogger {
        inner: inner_logger,
    };
    log::set_max_level(log::LevelFilter::Debug);
    log::set_boxed_logger(Box::new(multi)).ok();

    // Set MultiLogger BEFORE the tracing subscriber. try_init() calls
    // set_global_default (succeeds) then LogTracer::init (fails because
    // our logger is already set). We ignore the error — the subscriber
    // is installed and log events flow through MultiLogger directly.
    let collector = event_collector().clone();
    let _ = egui_tracing::tracing_subscriber::registry()
        .with(collector)
        .try_init();
}

pub fn set_frame(frame: u64) {
    event_collector().set_frame(frame);
}

pub fn begin_frame() {
    event_collector().begin_frame();
}

pub fn end_frame() {
    event_collector().end_frame();
}

pub fn show_logs(ui: &mut egui::Ui) {
    // ui.set_min_size(ui.max_rect().size());
    let collector = event_collector().clone();
    let output = egui_tracing::Logs::new(collector).show(ui);
    if let Some(source) = output.goto_source {
        crate::open_file(
            ui.ctx(),
            &crate::SourceLocation {
                path: source.file.to_owned(),
                line: source.line as usize,
                column: 0,
            },
        );
    }
}

pub use egui_tracing::tracing::{CollectedEvent, Level};

/// Returns (total_event_count, warn_or_error_count_since_index).
pub fn count_warnings_since(since: usize) -> (usize, usize) {
    event_collector().count_at_level_since(since, egui_tracing::tracing::Level::WARN)
}

/// Returns the most recent warn/error events since `since` index (most recent first, up to `limit`).
pub fn recent_warnings_since(since: usize, limit: usize) -> Vec<CollectedEvent> {
    event_collector().recent_at_level_since(since, egui_tracing::tracing::Level::WARN, limit)
}

type OnClickFn = Box<dyn Fn(&egui::Context) + Send + Sync>;

/// A floating notification that surfaces the count of warnings/errors recorded since the
/// last acknowledgement. Hovering shows a tooltip with the most recent entries; clicking
/// invokes the configured `on_click` callback (typically: open the log viewer) and
/// acknowledges all currently-pending warnings.
///
/// Register once at app startup via `ctx.add_plugin(LogNotificationPlugin::new().on_click(...))`.
/// Call `acknowledge()` from elsewhere (e.g. when the log viewer is on screen) to clear the
/// notification without going through the click path.
pub struct LogNotificationPlugin {
    last_acknowledged: usize,
    on_click: Option<OnClickFn>,
    anchor: egui::Align2,
    offset: egui::Vec2,
}

impl Default for LogNotificationPlugin {
    fn default() -> Self {
        Self {
            last_acknowledged: 0,
            on_click: None,
            anchor: egui::Align2::RIGHT_BOTTOM,
            offset: egui::vec2(-8.0, -42.0),
        }
    }
}

impl LogNotificationPlugin {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the click handler. Invoked after the click has marked all current warnings as
    /// acknowledged, so handlers can safely focus the log view without producing a feedback
    /// loop.
    pub fn on_click(mut self, f: impl Fn(&egui::Context) + Send + Sync + 'static) -> Self {
        self.on_click = Some(Box::new(f));
        self
    }

    /// Override the screen anchor for the notification (default: right-bottom).
    pub fn anchor(mut self, anchor: egui::Align2, offset: egui::Vec2) -> Self {
        self.anchor = anchor;
        self.offset = offset;
        self
    }

    /// Mark all currently-recorded warnings as acknowledged, hiding the notification until
    /// new warnings arrive.
    pub fn acknowledge_all(&mut self) {
        let (total, _) = count_warnings_since(0);
        self.last_acknowledged = total;
    }
}

impl egui::Plugin for LogNotificationPlugin {
    fn debug_name(&self) -> &'static str {
        "LogNotificationPlugin"
    }

    fn on_end_pass(&mut self, ui: &mut egui::Ui) {
        crate::hot_call(|| self.render_notification(ui));
    }
}

impl LogNotificationPlugin {
    fn render_notification(&mut self, ui: &mut egui::Ui) {
        let (total, unseen_count) = count_warnings_since(self.last_acknowledged);
        if unseen_count == 0 {
            return;
        }

        let ctx = ui.ctx().clone();
        let mut clicked = false;

        egui::Area::new(egui::Id::new("egui_dev_tools::log_notification"))
            .order(egui::Order::Foreground)
            .anchor(self.anchor, self.offset)
            .show(&ctx, |ui| {
                let color = ui.visuals().error_fg_color;
                let frame = egui::Frame::new()
                    .fill(color.gamma_multiply(0.15))
                    .stroke(egui::Stroke::new(1.0_f32, color.gamma_multiply(0.5)))
                    .corner_radius(egui::CornerRadius::same(4))
                    .inner_margin(egui::MarginF32::symmetric(8.0, 4.0));

                let response = frame
                    .show(ui, |ui| {
                        ui.horizontal(|ui| {
                            ui.spacing_mut().item_spacing.x = 4.0;
                            paint_warning_triangle(ui, color);
                            let label = if unseen_count == 1 {
                                "1 warning".to_owned()
                            } else {
                                format!("{unseen_count} warnings")
                            };
                            ui.label(egui::RichText::new(label).small().strong().color(color));
                        });
                    })
                    .response
                    .interact(egui::Sense::click())
                    .on_hover_cursor(egui::CursorIcon::PointingHand);

                if response.hovered() {
                    let last_ack = self.last_acknowledged;
                    let available_height = (ui.content_rect().height() - 80.0).max(0.0);
                    egui::Tooltip::for_widget(&response).show(|ui| {
                        ui.set_max_width(600.0_f32.min(ui.content_rect().width()));
                        let mut painted_count = 0;
                        const MAX_PAINTED_COUNT: usize = 100;
                        for event in recent_warnings_since(last_ack, MAX_PAINTED_COUNT) {
                            ui.horizontal(|ui| {
                                ui.spacing_mut().item_spacing.x = 4.0;
                                let is_error = event.level <= Level::ERROR;
                                let level_color = if is_error {
                                    ui.visuals().error_fg_color
                                } else {
                                    ui.visuals().warn_fg_color
                                };
                                let level_str = if is_error { "ERROR" } else { " WARN" };
                                ui.label(egui::RichText::new(level_str).small().color(level_color));
                                let msg =
                                    event.message.as_deref().unwrap_or(&event.collapsed_summary);
                                ui.add(Label::new(egui::RichText::new(msg).small()).truncate());
                                painted_count += 1;
                            });
                            if ui.min_rect().height() >= available_height {
                                break;
                            }
                        }
                        if unseen_count > painted_count {
                            ui.weak(format!("…and {} more", unseen_count - painted_count));
                        }
                    });
                }

                clicked = response.clicked();
            });

        if clicked {
            self.last_acknowledged = total;
            if let Some(on_click) = &self.on_click {
                on_click(&ctx);
            }
        }
    }
}

/// Paint a small filled warning triangle with an exclamation mark. Self-contained so the
/// plugin doesn't drag in an icon-font dependency.
fn paint_warning_triangle(ui: &mut egui::Ui, color: egui::Color32) {
    let size = egui::Vec2::splat(14.0);
    let (rect, _) = ui.allocate_exact_size(size, egui::Sense::hover());
    let painter = ui.painter();
    let pad = 1.0;
    let top = egui::pos2(rect.center().x, rect.top() + pad);
    let left = egui::pos2(rect.left() + pad, rect.bottom() - pad);
    let right = egui::pos2(rect.right() - pad, rect.bottom() - pad);
    painter.add(egui::Shape::convex_polygon(
        vec![top, right, left],
        color,
        egui::Stroke::NONE,
    ));

    let bang_color = ui.visuals().window_fill;
    let bang_x = rect.center().x;
    let stem_top = rect.top() + 5.0;
    let stem_bottom = rect.bottom() - 5.0;
    painter.line_segment(
        [
            egui::pos2(bang_x, stem_top),
            egui::pos2(bang_x, stem_bottom),
        ],
        egui::Stroke::new(1.5_f32, bang_color),
    );
    painter.circle_filled(egui::pos2(bang_x, rect.bottom() - 3.0), 0.9, bang_color);
}
