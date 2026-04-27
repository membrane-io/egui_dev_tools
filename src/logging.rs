use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt;

use egui_tracing::tracing_subscriber::prelude::*;

const MAX_LOG_EVENTS: usize = 5_000;

thread_local! {
  pub static EVENT_COLLECTOR: egui_tracing::EventCollector =
    egui_tracing::EventCollector::default().with_max_events(Some(MAX_LOG_EVENTS));
  static LOG_CONTEXT: RefCell<Vec<(&'static str, ContextValue)>> = const { RefCell::new(Vec::new()) };
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
    LOG_CONTEXT.with(|ctx| ctx.borrow_mut().push((key, value.into())));
    Self { _private: () }
  }

  pub fn id(id: egui::Id) -> Self {
    Self::new("id", id)
  }
}

impl Drop for LogScope {
  fn drop(&mut self) {
    LOG_CONTEXT.with(|ctx| {
      ctx.borrow_mut().pop();
    });
  }
}

fn drain_context_fields(fields: &mut Vec<(String, String)>) {
  LOG_CONTEXT.with(|ctx| {
    for (key, value) in ctx.borrow().iter() {
      fields.push(((*key).to_owned(), value.to_string()));
    }
  });
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
    EVENT_COLLECTOR.with(|c| c.collect(event));
  }

  fn flush(&self) {
    self.inner.flush();
  }
}

pub fn init(inner_logger: Box<dyn log::Log>) {
  let multi = MultiLogger { inner: inner_logger };
  log::set_max_level(log::LevelFilter::Debug);
  log::set_boxed_logger(Box::new(multi)).ok();

  // Set MultiLogger BEFORE the tracing subscriber. try_init() calls
  // set_global_default (succeeds) then LogTracer::init (fails because
  // our logger is already set). We ignore the error — the subscriber
  // is installed and log events flow through MultiLogger directly.
  let collector = EVENT_COLLECTOR.with(|c| c.clone());
  let _ = egui_tracing::tracing_subscriber::registry().with(collector).try_init();
}

pub fn begin_frame() {
  EVENT_COLLECTOR.with(|c| c.begin_frame());
}

pub fn end_frame() {
  EVENT_COLLECTOR.with(|c| c.end_frame());
}

pub fn show_logs(ui: &mut egui::Ui) {
  // ui.set_min_size(ui.max_rect().size());
  let collector = EVENT_COLLECTOR.with(|c| c.clone());
  ui.add(egui_tracing::Logs::new(collector));
}
