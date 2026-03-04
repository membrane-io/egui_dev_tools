use egui::{
  Button, CollapsingHeader, Context, DragValue, Label, Rangef, Response, RichText, ScrollArea, TextEdit, TextStyle, Ui,
  Widget,
  emath::{Pos2, Rect, TSTransform, Vec2},
  epaint::Color32,
  plugin::{Plugin, TypedPluginHandle},
  vec2,
};
use egui_table::{AutoSizeMode, CellInfo, Column, HeaderCellInfo, Table, TableDelegate};
use regex::Regex;
use std::collections::{BTreeMap, HashMap};
use std::sync::OnceLock;
use std::{any::Any, ops::Range};

use crate::{SourceLocation, open_file};

pub use egui::Id;

pub const KEY_DELIMITER: &str = "/";
const HIERARCHY_INDENT: f32 = 8.0;

static PLUGIN_HANDLE: OnceLock<TypedPluginHandle<DebugValsPlugin>> = OnceLock::new();

/// Metadata about where a debug val was defined.
#[derive(Clone)]
pub struct DebugValMetadata {
  /// File path, line number, and column (e.g., "src/app.rs:42:10").
  /// Used to render a navigation button.
  pub file_line_col: &'static str,

  /// Module path (e.g., "my_crate::my_module").
  /// Used as the label for vals without custom keys.
  pub module_path: &'static str,

  /// User-provided custom key, if any. Used as the label when provided.
  pub custom_key: Option<String>,

  /// Whether this value is written by the app (`val_mut!`) rather than tuned by the user (`val!`).
  /// App-controlled values are display-only and should NOT be persisted.
  pub app_controlled: bool,
}

impl DebugValMetadata {
  /// Returns the display label: custom key if provided, otherwise module_path.
  pub fn display_label(&self) -> &str {
    self.custom_key.as_deref().unwrap_or(self.module_path).trim_matches('"')
  }
}

/// UI rendering hints for a debug val (e.g. min/max for numeric drag values, options for strings).
///
/// ```ignore
/// val!(u32, "age", min = 18, max = 100)
/// val!(f32, "speed", min = 0.0, max = 1.0, speed = 0.01)
/// val!(String, "name", options = ["foo", "bar", "baz", "qux"])
/// ```
#[derive(Clone, Default)]
pub struct ValParams {
  pub min: Option<f64>,
  pub max: Option<f64>,
  pub speed: Option<f64>,
  pub options: Option<Vec<String>>,
  pub suffix: Option<&'static str>,
}

impl ValParams {
  pub fn min(mut self, v: impl Into<f64>) -> Self {
    self.min = Some(v.into());
    self
  }
  pub fn max(mut self, v: impl Into<f64>) -> Self {
    self.max = Some(v.into());
    self
  }
  pub fn speed(mut self, v: impl Into<f64>) -> Self {
    self.speed = Some(v.into());
    self
  }
  pub fn suffix(mut self, s: &'static str) -> Self {
    self.suffix = Some(s);
    self
  }
  pub fn options(mut self, opts: impl IntoIterator<Item = impl Into<String>>) -> Self {
    self.options = Some(opts.into_iter().map(|s| s.into()).collect());
    self
  }
}

/// Trait for values that can be debugged with automatic UI rendering.
pub trait DebugVal: Any + Send + Sync {
  /// Render only the value control (e.g. DragValue, checkbox) without label or source button.
  fn render_value_ui(&mut self, _ui: &mut Ui, _metadata: &DebugValMetadata, _params: &ValParams) {}

  /// Clone this value into a boxed trait object.
  fn clone_boxed(&self) -> Box<dyn DebugVal>;

  /// Persist this value into egui's data store.
  fn save_persisted(&self, ctx: &Context, id: Id);

  /// Load a previously persisted value from egui's data store.
  fn load_persisted(ctx: &Context, id: Id) -> Option<Box<dyn DebugVal>>
  where
    Self: Sized;
}

struct DebugValEntry {
  value: Box<dyn DebugVal>,
  metadata: DebugValMetadata,
  params: ValParams,
}

/// Plugin that stores all debug values.
pub struct DebugValsPlugin {
  values: HashMap<Id, DebugValEntry>,
  order: BTreeMap<String, Id>,
  ctx: Option<Context>,
  show_app_controlled: bool,
  hidden_key_prefix_filter: String,
}

impl DebugValsPlugin {
  fn show_app_controlled_id() -> Id {
    Id::new("gaze/debug_vals/show_app_controlled")
  }

  fn hidden_key_prefix_filter_id() -> Id {
    Id::new("gaze/debug_vals/hidden_key_prefix_filter")
  }

  pub fn new() -> Self {
    Self {
      values: HashMap::new(),
      order: BTreeMap::new(),
      ctx: None,
      show_app_controlled: false,
      hidden_key_prefix_filter: String::new(),
    }
  }

  fn persist_show_app_controlled(&self, ctx: &Context) {
    ctx.data_mut(|data| data.insert_persisted(Self::show_app_controlled_id(), self.show_app_controlled));
  }

  fn persist_hidden_key_prefix_filter(&self, ctx: &Context) {
    ctx.data_mut(|data| {
      data.insert_persisted(Self::hidden_key_prefix_filter_id(), self.hidden_key_prefix_filter.clone());
    });
  }

  /// Get a value from storage, or insert the default if it doesn't exist.
  /// For user-controlled values, tries to restore from egui's persisted store first.
  /// Params are always updated to reflect the latest call-site configuration.
  pub fn get_or_insert<T>(&mut self, id: Id, metadata: DebugValMetadata, params: ValParams) -> T
  where
    T: Clone + Default + DebugVal + 'static,
  {
    if let Some(entry) = self.values.get_mut(&id) {
      entry.params = params.clone();
      if let Some(typed_val) = (entry.value.as_ref() as &dyn Any).downcast_ref::<T>() {
        return typed_val.clone();
      }
    }

    if !metadata.app_controlled {
      if let Some(ctx) = &self.ctx {
        if let Some(loaded) = T::load_persisted(ctx, id) {
          if let Some(typed) = (loaded.as_ref() as &dyn Any).downcast_ref::<T>() {
            let val = typed.clone();
            let label = metadata.display_label().to_string();
            self.values.insert(id, DebugValEntry { value: loaded, metadata, params });
            self.order.insert(label, id);
            return val;
          }
        }
      }
    }

    let default_val = T::default();
    let label = metadata.display_label().to_string();
    self.values.insert(id, DebugValEntry { value: Box::new(default_val.clone()), metadata, params });
    self.order.insert(label, id);
    default_val
  }

  /// Like `get_or_insert`, but uses a caller-provided default instead of `T::default()`.
  pub fn get_or_insert_with<T>(&mut self, id: Id, metadata: DebugValMetadata, params: ValParams, default: T) -> T
  where
    T: Clone + DebugVal + 'static,
  {
    if let Some(entry) = self.values.get_mut(&id) {
      entry.params = params.clone();
      if let Some(typed_val) = (entry.value.as_ref() as &dyn Any).downcast_ref::<T>() {
        return typed_val.clone();
      }
    }

    if !metadata.app_controlled {
      if let Some(ctx) = &self.ctx {
        if let Some(loaded) = T::load_persisted(ctx, id) {
          if let Some(typed) = (loaded.as_ref() as &dyn Any).downcast_ref::<T>() {
            let val = typed.clone();
            let label = metadata.display_label().to_string();
            self.values.insert(id, DebugValEntry { value: loaded, metadata, params });
            self.order.insert(label, id);
            return val;
          }
        }
      }
    }

    let label = metadata.display_label().to_string();
    self.values.insert(id, DebugValEntry { value: Box::new(default.clone()), metadata, params });
    self.order.insert(label, id);
    default
  }

  /// Set a value in storage.
  pub fn set<T>(&mut self, id: Id, metadata: DebugValMetadata, params: ValParams, value: T)
  where
    T: Clone + DebugVal + 'static,
  {
    let label = metadata.display_label().to_string();
    self.values.insert(id, DebugValEntry { value: Box::new(value), metadata, params });
    self.order.insert(label, id);
  }

  /// Remove all values whose `custom_key` matches `prefix` exactly or starts with `prefix/`.
  #[profiling::function]
  pub fn clear(&mut self, prefix: &str) {
    let lower = format!("{prefix}{KEY_DELIMITER}");
    let upper = format!("{prefix}~");
    use std::ops::Bound;
    let to_remove: Vec<Id> = self
      .order
      .extract_if((Bound::Included(lower), Bound::Excluded(upper)), |_, _| true)
      .map(|(_, id)| id)
      // .range()
      // .map(|(label, id)| (label.clone(), *id))
      // .iter()
      // .filter_map(|(label, id)| {
      //   let ck = self.values.get(id)?.metadata.custom_key.as_deref()?;
      //   (ck == prefix || ck.starts_with(&with_delim)).then(|| (label.clone(), *id))
      // })
      .collect();
    for id in to_remove {
      // self.order.remove(&label);
      self.values.remove(&id);
    }
  }

  /// Iterate over all values in alphabetical order by display label.
  pub fn for_each_ordered<F>(&mut self, mut f: F)
  where
    F: FnMut(&DebugValMetadata, &mut Box<dyn DebugVal>),
  {
    let ids: Vec<Id> = self.order.values().copied().collect();
    for id in ids {
      if let Some(entry) = self.values.get_mut(&id) {
        f(&entry.metadata, &mut entry.value);
      }
    }
  }
}

impl Default for DebugValsPlugin {
  fn default() -> Self {
    Self::new()
  }
}

impl Plugin for DebugValsPlugin {
  fn debug_name(&self) -> &'static str {
    "DebugValsPlugin"
  }

  fn setup(&mut self, ctx: &Context) {
    self.show_app_controlled =
      ctx.data_mut(|data| data.get_persisted::<bool>(Self::show_app_controlled_id())).unwrap_or(false);
    self.hidden_key_prefix_filter =
      ctx.data_mut(|data| data.get_persisted::<String>(Self::hidden_key_prefix_filter_id())).unwrap_or_default();
    let handle = ctx.plugin::<DebugValsPlugin>();
    if PLUGIN_HANDLE.set(handle).is_err() {
      panic!("DebugValsPlugin initialized twice");
    }
    self.ctx = Some(ctx.clone());
  }
}

/// A handle to a debug value that writes back on drop.
pub struct ValHandle<T: Clone + Default + DebugVal> {
  id: Id,
  metadata: DebugValMetadata,
  params: ValParams,
  value: T,
}

impl<T: Clone + Default + DebugVal + 'static> ValHandle<T> {
  pub fn new(id: Id, metadata: DebugValMetadata, params: ValParams) -> Self {
    let id = Id::new(metadata.custom_key.as_deref().unwrap_or(metadata.file_line_col));
    let value = if let Some(handle) = PLUGIN_HANDLE.get() {
      let mut guard = handle.lock();
      guard.get_or_insert::<T>(id, metadata.clone(), params.clone())
    } else {
      T::default()
    };

    Self { id, metadata, params, value }
  }

  pub fn with_default(id: Id, metadata: DebugValMetadata, params: ValParams, default: impl Into<T>) -> Self {
    let id = Id::new(metadata.custom_key.as_deref().unwrap_or(metadata.file_line_col));
    let default = default.into();
    let value = if let Some(handle) = PLUGIN_HANDLE.get() {
      let mut guard = handle.lock();
      guard.get_or_insert_with::<T>(id, metadata.clone(), params.clone(), default)
    } else {
      default
    };

    Self { id, metadata, params, value }
  }
}

impl<T: Clone + Default + DebugVal> std::ops::Deref for ValHandle<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl<T: Clone + Default + DebugVal> std::ops::DerefMut for ValHandle<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.value
  }
}

impl<T: Clone + Default + DebugVal + 'static> Drop for ValHandle<T> {
  fn drop(&mut self) {
    if let Some(handle) = PLUGIN_HANDLE.get() {
      let mut guard = handle.lock();
      guard.set(self.id, self.metadata.clone(), self.params.clone(), self.value.clone());
    }
  }
}

#[macro_export]
macro_rules! show_debug {
  () => {{
    let module_path = module_path!();
    let module = module_path.rsplit_once("::").map(|(_, name)| name).unwrap_or(module_path);
    val!(bool, module, "show debug")
  }};
}

/// Get a debug value (read-only). Uses file/line/column as key if no custom key provided.
/// Returns the value directly, not a handle.
///
/// ```ignore
/// val!(f32)                                                    // anonymous, no params
/// val!(f32, "speed")                                           // named, no params
/// val!(f32, "speed", min = 0.0, max = 10.0)                    // named, with params
/// val!(u32, "age", default = 25, min = 18, max = 100)          // with default
/// val!(String, "title", default = "mr", options = ["mr", "ms"]) // string with default + dropdown
/// val!(f32, "physics", "gravity")                              // hierarchical key
/// ```
#[macro_export]
macro_rules! val {
  ($ty:ty) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let key = $crate::vals::Id::new(file_line_col);
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: None,
      app_controlled: false,
    };
    *$crate::vals::ValHandle::<$ty>::new(key, metadata, $crate::vals::ValParams::default())
  }};
  ($ty:ty, $key:expr, default = $default:expr $(, $param:ident = $value:expr)* $(,)?) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let custom_key = format!("{:?}", $key);
    let id = $crate::vals::Id::new(custom_key.as_str());
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: Some(custom_key),
      app_controlled: false,
    };
    let params = $crate::vals::ValParams::default()$(.$param($value))*;
    *$crate::vals::ValHandle::<$ty>::with_default(id, metadata, params, $default)
  }};
  ($ty:ty, $key:expr, $($param:ident = $value:expr),+ $(,)?) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let custom_key = format!("{:?}", $key);
    let id = $crate::vals::Id::new(custom_key.as_str());
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: Some(custom_key),
      app_controlled: false,
    };
    let params = $crate::vals::ValParams::default()$(.$param($value))+;
    *$crate::vals::ValHandle::<$ty>::new(id, metadata, params)
  }};
  ($ty:ty, $key:expr) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let custom_key = format!("{:?}", $key);
    let id = $crate::vals::Id::new(custom_key.as_str());
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: Some(custom_key),
      app_controlled: false,
    };
    *$crate::vals::ValHandle::<$ty>::new(id, metadata, $crate::vals::ValParams::default())
  }};
  ($ty:ty, $key1:expr, $($rest:expr),+ $(,)?) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let custom_key = {
      let mut key = format!("{:?}", $key1);
      $(
        key.push_str($crate::vals::KEY_DELIMITER);
        key.push_str(&format!("{:?}", $rest));
      )+
      key
    };
    let id = $crate::vals::Id::new(custom_key.as_str());
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: Some(custom_key),
      app_controlled: false,
    };
    *$crate::vals::ValHandle::<$ty>::new(id, metadata, $crate::vals::ValParams::default())
  }};
}

/// Get a mutable debug value handle. Uses file/line/column as key if no custom key provided.
/// Returns a `ValHandle<T>` that can be modified and writes back on drop.
#[macro_export]
macro_rules! val_mut {
  ($ty:ty) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let id = $crate::vals::Id::new(file_line_col);
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: None,
      app_controlled: true,
    };
    $crate::vals::ValHandle::<$ty>::new(id, metadata, $crate::vals::ValParams::default())
  }};
  ($ty:ty, $key:expr, default = $default:expr $(, $param:ident = $value:expr)* $(,)?) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let custom_key = format!("{:?}", $key);
    let id = $crate::vals::Id::new(custom_key.as_str());
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: Some(custom_key),
      app_controlled: true,
    };
    let params = $crate::vals::ValParams::default()$(.$param($value))*;
    $crate::vals::ValHandle::<$ty>::with_default(id, metadata, params, $default)
  }};
  ($ty:ty, $key:expr, $($param:ident = $value:expr),+ $(,)?) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let custom_key = format!("{:?}", $key);
    let id = $crate::vals::Id::new(custom_key.as_str());
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: Some(custom_key),
      app_controlled: true,
    };
    let params = $crate::vals::ValParams::default()$(.$param($value))+;
    $crate::vals::ValHandle::<$ty>::new(id, metadata, params)
  }};
  ($ty:ty, $key:expr) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let custom_key = format!("{:?}", $key);
    let id = $crate::vals::Id::new(custom_key.as_str());
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: Some(custom_key),
      app_controlled: true,
    };
    $crate::vals::ValHandle::<$ty>::new(id, metadata, $crate::vals::ValParams::default())
  }};
  ($ty:ty, $key1:expr, $($rest:expr),+ $(,)?) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let custom_key = {
      let mut key = format!("{:?}", $key1);
      $(
        key.push_str($crate::vals::KEY_DELIMITER);
        key.push_str(&format!("{:?}", $rest));
      )+
      key
    };
    let id = $crate::vals::Id::new(custom_key.as_str());
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: Some(custom_key),
      app_controlled: true,
    };
    $crate::vals::ValHandle::<$ty>::new(id, metadata, $crate::vals::ValParams::default())
  }};
}

#[inline]
pub fn set_val_with_key<T>(key: impl ::std::fmt::Debug, value: T)
where
  T: Clone + Default + DebugVal + 'static,
{
  *val_mut!(T, key) = value;
}

#[inline]
pub fn set_val_with_keys<T>(key1: impl ::std::fmt::Debug, key2: impl ::std::fmt::Debug, value: T)
where
  T: Clone + Default + DebugVal + 'static,
{
  *val_mut!(T, key1, key2) = value;
}

#[macro_export]
macro_rules! set_val {
  ($key:expr, $value:expr) => {{ $crate::vals::set_val_with_key($key, $value) }};
  ($key1:expr, $key2:expr, $value:expr) => {{ $crate::vals::set_val_with_keys($key1, $key2, $value) }};
  ($key1:expr, $key2:expr, $key3:expr, $value:expr) => {{ $crate::vals::set_val_with_keys($key1, $key2, $key3, $value) }};
}

/// Remove a val and all children vals whose key starts with the given prefix.
pub fn clear_val_by_key(prefix: &str) {
  if let Some(handle) = PLUGIN_HANDLE.get() {
    handle.lock().clear(prefix);
  }
}

/// Remove a val and all children vals whose key starts with the given prefix.
///
/// Accepts one or more key segments that are joined with `KEY_DELIMITER`.
/// ```ignore
/// clear_val!("group");               // clears "group" and "group/..."
/// clear_val!("group", "subgroup");   // clears "group/subgroup" and "group/subgroup/..."
/// ```
#[macro_export]
macro_rules! clear_val {
  ($key:expr) => {{
    let prefix = format!("{:?}", $key);
    $crate::vals::clear_val_by_key(&prefix);
  }};
  ($key1:expr, $($rest:expr),+ $(,)?) => {{
    let prefix = {
      let mut key = format!("{:?}", $key1);
      $(
        key.push_str($crate::vals::KEY_DELIMITER);
        key.push_str(&format!("{:?}", $rest));
      )+
      key
    };
    $crate::vals::clear_val_by_key(&prefix);
  }};
}

/// A widget for viewing and editing registered debug values.
///
/// # Example
/// ```ignore
/// ui.add(DebugValues::new());
/// // or with builder pattern:
/// ui.add(DebugValues::new().show_empty_message(false).key("some/key/path").header(false));
/// ```
#[must_use = "You should put this widget in a ui with `ui.add(widget);`"]
pub struct DebugValues {
  show_empty_message: bool,
  filter_key: Option<String>,
  show_header: bool,
  can_navigate: bool,
  striped: bool,
  resizable_columns: bool,
  compact: bool,
}

impl DebugValues {
  pub fn new() -> Self {
    Self {
      show_empty_message: true,
      filter_key: None,
      show_header: true,
      can_navigate: true,
      striped: false,
      resizable_columns: true,
      compact: false,
    }
  }

  /// Whether to show a message when the plugin is not initialized.
  /// Default is `true`.
  pub fn show_empty_message(mut self, show: bool) -> Self {
    self.show_empty_message = show;
    self
  }

  /// Only render values matching this key path.
  pub fn key(mut self, key: impl Into<String>) -> Self {
    self.filter_key = Some(key.into());
    self
  }

  /// Whether to render the widget header.
  /// Default is `true`.
  pub fn header(mut self, show: bool) -> Self {
    self.show_header = show;
    self
  }

  /// Whether to show the navigate-to-source button column.
  /// Default is `true`.
  pub fn can_navigate(mut self, show: bool) -> Self {
    self.can_navigate = show;
    self
  }

  /// Whether to use alternating row backgrounds.
  /// Default is `false`.
  pub fn striped(mut self, striped: bool) -> Self {
    self.striped = striped;
    self
  }

  /// Whether the name and value columns are resizable.
  /// Default is `true`.
  pub fn resizable_columns(mut self, resizable: bool) -> Self {
    self.resizable_columns = resizable;
    self
  }

  /// Use a smaller row height (17 instead of 19).
  /// Default is `false`.
  pub fn compact(mut self, compact: bool) -> Self {
    self.compact = compact;
    self
  }
}

impl Default for DebugValues {
  fn default() -> Self {
    Self::new()
  }
}

impl Widget for DebugValues {
  fn ui(self, ui: &mut Ui) -> Response {
    let key_filter_segments = self.filter_key.as_ref().map(|key| {
      key
        .split(KEY_DELIMITER)
        .filter(|segment| !segment.is_empty())
        .map(|segment| segment.trim_matches('"').to_string())
        .collect::<Vec<String>>()
    });
    let response = ui.vertical(|ui| {
      if let Some(handle) = PLUGIN_HANDLE.get() {
        let mut plugin = handle.lock();
        let style = ui.style_mut();
        style.spacing.interact_size.y = if self.compact { 15.0 } else { 19.0 };
        style.spacing.item_spacing = vec2(0.0, 0.0);
        style.spacing.indent = HIERARCHY_INDENT;
        style.visuals.indent_has_left_vline = false;
        if self.compact {
          style.override_text_style = Some(TextStyle::Small);
        }

        if self.show_header {
          ui.horizontal(|ui| {
            ui.strong("Debug Vals");
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
              let response =
                ui.checkbox(&mut plugin.show_app_controlled, "👁").on_hover_text("Show app-controlled values");
              if response.changed() {
                plugin.persist_show_app_controlled(ui.ctx());
              }

              let is_filter_active = !plugin.hidden_key_prefix_filter.trim().is_empty();
              let filter_compile = compile_hidden_prefix_filter_regex(&plugin.hidden_key_prefix_filter);
              if is_filter_active {
                match filter_compile {
                  Ok(Some(_)) => {
                    ui.label(RichText::new("⚠").color(Color32::YELLOW))
                      .on_hover_text("Key filter active: matching key prefixes are hidden");
                  }
                  Ok(None) => {}
                  Err(err) => {
                    ui.label(RichText::new("⚠").color(Color32::LIGHT_RED))
                      .on_hover_text(format!("Invalid filter regex: {err}"));
                  }
                }
              }

              let filter_response = ui
                .add(
                  TextEdit::singleline(&mut plugin.hidden_key_prefix_filter)
                    .desired_width(180.0)
                    .hint_text("Hide key prefixes: foo|bar"),
                )
                .on_hover_text("Regex key prefixes to hide (use | to separate multiple prefixes)");
              if filter_response.changed() {
                plugin.persist_hidden_key_prefix_filter(ui.ctx());
              }
            });
          });
        }

        let hidden_prefix_filter = compile_hidden_prefix_filter_regex(&plugin.hidden_key_prefix_filter).ok().flatten();

        let DebugValsPlugin { values, order, show_app_controlled, .. } = &*plugin;

        let entries: Vec<(Vec<String>, Id)> = order
          .iter()
          .filter(|(_, id)| *show_app_controlled || values.get(id).map_or(true, |e| !e.metadata.app_controlled))
          .filter_map(|(label, &id)| {
            let segments: Vec<String> = label.split(KEY_DELIMITER).map(|s| s.trim_matches('"').to_string()).collect();
            if let Some(prefix_filter) = hidden_prefix_filter.as_ref() {
              let normalized_key = segments.join(KEY_DELIMITER);
              if prefix_filter.is_match(&normalized_key) {
                return None;
              }
            }
            if let Some(filter_segments) = key_filter_segments.as_ref() {
              if segments.len() < filter_segments.len() {
                return None;
              }
              if !segments.starts_with(&filter_segments[..]) {
                return None;
              }
              let filtered_segments = &segments[filter_segments.len()..];
              let mut filtered_segments = filtered_segments.to_vec();
              if filtered_segments.is_empty() {
                filtered_segments = segments.last().map_or_else(|| vec![String::new()], |name| vec![name.to_string()]);
              }
              return Some((filtered_segments, id));
            }
            Some((segments, id))
          })
          .collect();

        let mut root = TreeNode::default();
        for (segments, id) in &entries {
          root.insert(segments, *id);
        }

        let table_opts = TableOptions {
          can_navigate: self.can_navigate,
          striped: self.striped,
          resizable_columns: self.resizable_columns,
        };
        ui.vertical(|ui| {
          ScrollArea::vertical().show(ui, |ui| {
            render_tree(ui, &root, &mut plugin, &table_opts, self.compact);
          });
        });
      } else if self.show_empty_message {
        ui.label("DebugVals plugin not initialized");
      }
    });

    response.response
  }
}

fn compile_hidden_prefix_filter_regex(input: &str) -> Result<Option<Regex>, regex::Error> {
  let prefixes: Vec<&str> = input.split('|').map(str::trim).filter(|part| !part.is_empty()).collect();
  if prefixes.is_empty() {
    return Ok(None);
  }

  let alternation = prefixes.into_iter().map(|prefix| format!("(?:{prefix})")).collect::<Vec<String>>().join("|");
  let pattern = format!("^(?:{alternation})(?:{}|$)", regex::escape(KEY_DELIMITER));
  Regex::new(&pattern).map(Some)
}

#[derive(Default)]
struct TreeNode<'a> {
  branches: BTreeMap<&'a str, TreeNode<'a>>,
  leaves: Vec<(&'a str, Id)>,
}

impl<'a> TreeNode<'a> {
  fn insert(&mut self, segments: &'a [String], id: Id) {
    if segments.len() == 1 {
      self.leaves.push((&segments[0], id));
    } else {
      self.branches.entry(&segments[0]).or_default().insert(&segments[1..], id);
    }
  }
}

struct ValsTableDelegate<'a> {
  leaves: &'a [(String, Id)],
  plugin: &'a mut DebugValsPlugin,
  interact_height: f32,
  can_navigate: bool,
  striped: bool,
}

impl TableDelegate for ValsTableDelegate<'_> {
  fn default_row_height(&self) -> f32 {
    self.interact_height + 1.0 // 1.0 to separate intractive controls vertically
  }

  fn header_cell_ui(&mut self, _ui: &mut Ui, _cell: &HeaderCellInfo) {}

  fn row_ui(&mut self, ui: &mut Ui, row_nr: u64) {
    if self.striped && row_nr % 2 == 1 {
      ui.painter().rect_filled(ui.available_rect_before_wrap(), 0.0, ui.visuals().faint_bg_color);
    }
  }

  fn cell_ui(&mut self, ui: &mut Ui, cell: &CellInfo) {
    let (leaf_name, id) = &self.leaves[cell.row_nr as usize];
    if let Some(entry) = self.plugin.values.get_mut(id) {
      let metadata = DebugValMetadata {
        file_line_col: entry.metadata.file_line_col,
        module_path: entry.metadata.module_path,
        custom_key: Some(leaf_name.clone()),
        app_controlled: entry.metadata.app_controlled,
      };
      match cell.col_nr {
        0 => {
          let label = Label::new(metadata.display_label()).truncate();
          ui.add(label).on_hover_text(metadata.file_line_col);
        }
        1 => {
          ui.add_space(2.0);
          ui.spacing_mut().item_spacing = vec2(1.0, 1.0);
          entry.value.render_value_ui(ui, &metadata, &entry.params);
          if !entry.metadata.app_controlled {
            entry.value.save_persisted(ui.ctx(), *id);
          }
        }
        2 => {
          ui.add_space(1.0);
          if ui
            .add(
              Button::new(RichText::new("{}").monospace().size(10.0))
                .min_size(Vec2::splat(self.interact_height))
                .frame(false)
                .frame_when_inactive(false),
            )
            .on_hover_text(metadata.file_line_col)
            .clicked()
          {
            let source = SourceLocation { path: metadata.file_line_col.to_string(), line: 0, column: 0 };
            open_file(ui.ctx(), &source);
          }
          ui.add_space(1.0);
        }
        _ => {}
      }
    }
  }
}

struct TableOptions {
  can_navigate: bool,
  striped: bool,
  resizable_columns: bool,
}

fn render_tree(ui: &mut Ui, node: &TreeNode<'_>, plugin: &mut DebugValsPlugin, opts: &TableOptions, compact: bool) {
  // Nested keys first
  for (name, child) in &node.branches {
    let display_name = (*name).to_string();
    let current = child;

    CollapsingHeader::new(&display_name).id_salt(display_name).default_open(true).show(ui, |ui| {
      render_tree(ui, current, plugin, opts, compact);
    });
  }

  let all_leaves = node.leaves.iter().map(|(name, id)| (name.to_string(), *id));
  let all_leaves: Vec<_> = all_leaves.collect();

  if !all_leaves.is_empty() {
    let interact_height = ui.spacing().interact_size.y;
    let btn_col_width = interact_height + 2.0;
    let available = ui.available_width();
    let reserved = if opts.can_navigate { btn_col_width } else { 0.0 };
    let name_col_width = (available * 0.35).clamp(60.0, 300.0);
    let value_col_width = (available - name_col_width - reserved).max(60.0);

    let mut columns = vec![
      Column::new(name_col_width).resizable(opts.resizable_columns).range(Rangef::new(40.0, 400.0)),
      Column::new(value_col_width).resizable(opts.resizable_columns).range(Rangef::new(40.0, f32::INFINITY)),
    ];
    if opts.can_navigate {
      columns.push(Column::new(btn_col_width).resizable(false).range(Rangef::new(btn_col_width, btn_col_width)));
    }

    ui.scope(|ui| {
      let row_height = interact_height + 1.0;
      ui.set_max_height(row_height * all_leaves.len() as f32);
      let mut delegate = ValsTableDelegate {
        leaves: &all_leaves,
        plugin,
        interact_height,
        can_navigate: opts.can_navigate,
        striped: opts.striped,
      };
      let auto_size = if compact { AutoSizeMode::Never } else { AutoSizeMode::Always };
      Table::new()
        .id_salt("__vals_leaves")
        .num_rows(all_leaves.len() as u64)
        .columns(columns)
        .headers(vec![])
        .auto_size_mode(auto_size)
        .show(ui, &mut delegate);
    });
  }
}

#[macro_export]
macro_rules! show_vals {
  ($ui:expr) => {
    $ui.add($crate::vals::DebugValues::new().header(false).can_navigate(false).compact(true).resizable_columns(false))
  };
  ($ui:expr, $key:expr) => {{
    let key = format!("{:?}", $key);
    $ui.add($crate::vals::DebugValues::new().key(key).header(false).can_navigate(false).compact(true).resizable_columns(false))
  }};
  ($ui:expr, $key:expr, $($rest:expr),+ $(,)?) => {{
    let keys: ::std::vec::Vec<::std::string::String> = ::std::vec![format!("{:?}", $key), $(format!("{:?}", $rest)),+];
    $ui.add(
      $crate::vals::DebugValues::new()
        .key(keys.join($crate::vals::KEY_DELIMITER))
        .header(false)
        .can_navigate(false)
        .compact(true)
        .resizable_columns(false),
    )
  }};
}

macro_rules! impl_debug_val {
  ($ty:ty, |$self:ident, $ui:ident, $meta:ident, $params:ident| $control:expr) => {
    impl DebugVal for $ty {
      #[allow(unused_variables)]
      fn render_value_ui(&mut $self, $ui: &mut Ui, $meta: &DebugValMetadata, $params: &ValParams) {
        $control
      }
      fn clone_boxed(&self) -> Box<dyn DebugVal> {
        Box::new(self.clone())
      }
      fn save_persisted(&self, ctx: &Context, id: Id) {
        ctx.data_mut(|d| d.insert_persisted(id, self.clone()));
      }
      fn load_persisted(ctx: &Context, id: Id) -> Option<Box<dyn DebugVal>> where Self: Sized {
        ctx.data_mut(|d| d.get_persisted::<Self>(id)).map(|v| Box::new(v) as _)
      }
    }
  };
}

macro_rules! impl_debug_val_numeric {
  ($ty:ty, $speed:expr, $format:literal) => {
    impl_debug_val!($ty, |self, ui, metadata, params| {
      if metadata.app_controlled {
        ui.add(Label::new(RichText::new(format!($format, self)).monospace()));
      } else {
        let speed = params.speed.unwrap_or($speed) as f32;
        let mut dv = DragValue::new(self).speed(speed);
        if let (Some(min), Some(max)) = (params.min, params.max) {
          dv = dv.range(min as $ty..=max as $ty);
        } else if let Some(min) = params.min {
          dv = dv.range(min as $ty..=<$ty>::MAX);
        } else if let Some(max) = params.max {
          dv = dv.range(<$ty>::MIN..=max as $ty);
        }
        if let Some(suffix) = params.suffix {
          dv = dv.suffix(suffix);
        }
        ui.add(dv);
      }
    });
  };
}

impl_debug_val_numeric!(f32, 0.1, "{:.3}");
impl_debug_val_numeric!(f64, 0.1, "{:.3}");
impl_debug_val_numeric!(i32, 1.0, "{}");
impl_debug_val_numeric!(u32, 1.0, "{}");
impl_debug_val_numeric!(i16, 1.0, "{}");
impl_debug_val_numeric!(u16, 1.0, "{}");
impl_debug_val_numeric!(usize, 1.0, "{}");
impl_debug_val_numeric!(isize, 1.0, "{}");

impl_debug_val!(bool, |self, ui, metadata, _params| {
  if metadata.app_controlled {
    let text = if *self { "true" } else { "false" };
    ui.add(Label::new(RichText::new(text).monospace()));
  } else {
    ui.checkbox(self, "").on_hover_text(metadata.file_line_col);
  }
});

impl_debug_val!(String, |self, ui, metadata, params| {
  if metadata.app_controlled {
    if self.is_empty() {
      ui.add(Label::new(RichText::new("(empty)").weak()).truncate());
    } else {
      ui.add(Label::new(self.as_str()).truncate());
    }
  } else if let Some(options) = &params.options {
    let selected = if self.is_empty() { "(none)" } else { self.as_str() };
    egui::ComboBox::from_id_salt(metadata.file_line_col).selected_text(selected).show_ui(ui, |ui| {
      for opt in options {
        ui.selectable_value(self, opt.clone(), opt.as_str());
      }
    });
  } else {
    let mut temp = self.as_str().to_string();
    let desired_width = if ui.is_sizing_pass() { 200.0 } else { ui.available_width() };
    if ui.add(TextEdit::singleline(&mut temp).desired_width(desired_width)).changed() {
      *self = temp;
    }
  }
});

impl_debug_val!(Color32, |self, ui, metadata, _params| {
  if metadata.app_controlled {
    ui.add(Label::new(RichText::new(format!("{:?}", *self)).monospace()));
  } else {
    egui::color_picker::color_edit_button_srgba(ui, self, egui::color_picker::Alpha::Opaque);
  }
});

impl_debug_val!(Vec2, |self, ui, metadata, params| {
  if metadata.app_controlled {
    ui.add(Label::new(RichText::new(format!("x: {:.3} y: {:.3}", self.x, self.y)).monospace()));
  } else {
    let speed = params.speed.unwrap_or(0.1) as f32;
    ui.label("x:");
    ui.add(DragValue::new(&mut self.x).speed(speed));
    ui.label("y:");
    ui.add(DragValue::new(&mut self.y).speed(speed));
  }
});

impl_debug_val!(Pos2, |self, ui, metadata, params| {
  if metadata.app_controlled {
    ui.add(Label::new(RichText::new(format!("x: {:.3} y: {:.3}", self.x, self.y)).monospace()));
  } else {
    let speed = params.speed.unwrap_or(0.1) as f32;
    ui.label("x:");
    ui.add(DragValue::new(&mut self.x).speed(speed));
    ui.label("y:");
    ui.add(DragValue::new(&mut self.y).speed(speed));
  }
});

impl_debug_val!(TSTransform, |self, ui, metadata, _params| {
  if metadata.app_controlled {
    ui.add(Label::new(
      RichText::new(format!("tx: {:.3} ty: {:.3} scale: {:.3}", self.translation.x, self.translation.y, self.scaling))
        .monospace(),
    ));
  } else {
    ui.label("tx:");
    ui.add(DragValue::new(&mut self.translation.x).speed(0.1));
    ui.label("ty:");
    ui.add(DragValue::new(&mut self.translation.y).speed(0.1));
    ui.label("scale:");
    ui.add(DragValue::new(&mut self.scaling).speed(0.01));
  }
});

impl_debug_val!(Rect, |self, ui, metadata, _params| {
  if metadata.app_controlled {
    ui.add(Label::new(
      RichText::new(format!("min ({:.3}, {:.3}) max ({:.3}, {:.3})", self.min.x, self.min.y, self.max.x, self.max.y))
        .monospace(),
    ));
  } else {
    ui.horizontal(|ui| {
      ui.label("min x:");
      ui.add(DragValue::new(&mut self.min.x).speed(0.1));
      ui.label("y:");
      ui.add(DragValue::new(&mut self.min.y).speed(0.1));
      ui.label("max x:");
      ui.add(DragValue::new(&mut self.max.x).speed(0.1));
      ui.label("y:");
      ui.add(DragValue::new(&mut self.max.y).speed(0.1));
    });
  }
});

impl_debug_val!(Range<i32>, |self, ui, metadata, _params| {
  if metadata.app_controlled {
    ui.add(Label::new(RichText::new(format!("{}..{}", self.start, self.end)).monospace()));
  } else {
    ui.horizontal(|ui| {
      ui.label("start:");
      ui.add(DragValue::new(&mut self.start).speed(0.1));
      ui.label("end:");
      ui.add(DragValue::new(&mut self.end).speed(0.1));
    });
  }
});
