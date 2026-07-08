use egui::{
    Button, Context, DragValue, Label, Margin, Painter, Rangef, Response, RichText, Sense,
    Shadow, Stroke, TextEdit, TextStyle, Ui, Widget,
    collapsing_header::paint_default_icon,
    emath::{Pos2, Rect, TSTransform, Vec2},
    epaint::{Color32, Shape},
    plugin::{Plugin, TypedPluginHandle},
    vec2,
};
use egui_table::{AutoSizeMode, CellInfo, Column, HeaderCellInfo, Table, TableDelegate};
use regex::Regex;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::sync::Mutex;
use std::{any::Any, ops::Range};

thread_local! {
  static ACTIVE_VAL_PLACER: Cell<Option<Id>> = const { Cell::new(None) };
  static ACTIVE_VAL_PREFIX: RefCell<String> = const { RefCell::new(String::new()) };
}

pub fn active_val_placer() -> Option<Id> {
    ACTIVE_VAL_PLACER.with(|c| c.get())
}

pub fn active_val_prefix() -> String {
    ACTIVE_VAL_PREFIX.with(|c| c.borrow().clone())
}

pub fn prefixed_key(key: &str) -> String {
    ACTIVE_VAL_PREFIX.with(|c| {
        let prefix = c.borrow();
        if prefix.is_empty() {
            key.to_string()
        } else {
            format!("{}{KEY_DELIMITER}{key}", *prefix)
        }
    })
}

fn auto_place(val_id: Id) {
    if let Some(widget_id) = active_val_placer() {
        with_plugin(|p| p.placed.push((val_id, widget_id)));
    }
}

/// Guard that automatically places any `val!` / `val_handle!` created while it is alive,
/// and optionally prefixes their keys.
///
/// Use at the top of a widget's render function:
/// ```ignore
/// fn render(&self, ui: &mut Ui) {
///   let _group = ValGroup::new(ui, "my_widget");
///   let speed = val!(f32, "speed", default = 1.0);
///   // key becomes "my_widget/speed", automatically placed on this widget
/// }
/// ```
pub struct ValGroup {
    previous_placer: Option<Id>,
    previous_prefix: String,
}

impl ValGroup {
    pub fn new(ui: &Ui, name: &str) -> Self {
        Self::with_id(ui.unique_id(), name)
    }

    pub fn with_id(widget_id: Id, name: &str) -> Self {
        let previous_placer = ACTIVE_VAL_PLACER.with(|c| c.replace(Some(widget_id)));
        let previous_prefix = ACTIVE_VAL_PREFIX.with(|c| {
            let mut current = c.borrow_mut();
            let prev = current.clone();
            if current.is_empty() {
                *current = name.to_string();
            } else {
                current.push_str(KEY_DELIMITER);
                current.push_str(name);
            }
            prev
        });
        Self {
            previous_placer,
            previous_prefix,
        }
    }
}

impl Drop for ValGroup {
    fn drop(&mut self) {
        ACTIVE_VAL_PLACER.with(|c| c.set(self.previous_placer));
        ACTIVE_VAL_PREFIX.with(|c| *c.borrow_mut() = std::mem::take(&mut self.previous_prefix));
    }
}

/// Place and prefix all `val!` / `val_handle!` calls in the current scope.
///
/// ```ignore
/// fn render(&self, ui: &mut Ui) {
///   val_group!(ui, "my_widget");
///   let speed = val!(f32, "speed", default = 1.0);
///   // key becomes "my_widget/speed", automatically placed
/// }
///
/// fn other(ui: &mut Ui) {
///   val_group!(ui);
///   // prefix defaults to enclosing function/module name
/// }
/// ```
#[macro_export]
macro_rules! val_group {
    ($ui:expr, $name:expr) => {
        let _val_group_guard = $crate::vals::ValGroup::new($ui, $name);
    };
    ($ui:expr) => {
        let _module = module_path!();
        let _val_group_name = _module.rsplit_once("::").map_or(_module, |(_, name)| name);
        let _val_group_guard = $crate::vals::ValGroup::new($ui, _val_group_name);
    };
}

use crate::{SourceLocation, open_file};

pub use egui::Id;

pub const KEY_DELIMITER: &str = "/";
const HIERARCHY_INDENT: f32 = 8.0;

const PLACED_PALETTE: [Color32; 8] = [
    Color32::from_rgb(80, 200, 255),
    Color32::from_rgb(255, 200, 50),
    Color32::from_rgb(100, 255, 100),
    Color32::from_rgb(255, 150, 60),
    Color32::from_rgb(220, 120, 255),
    Color32::from_rgb(80, 255, 220),
    Color32::from_rgb(255, 100, 180),
    Color32::from_rgb(255, 80, 80),
];

fn placed_dot_color(prefix: &str) -> Color32 {
    let hash = prefix
        .bytes()
        .fold(0u32, |acc, b| acc.wrapping_mul(31).wrapping_add(b as u32));
    PLACED_PALETTE[hash as usize % PLACED_PALETTE.len()]
}

const LABEL_BG: Color32 = Color32::from_black_alpha(200);
const INDICATOR_GAP: f32 = 2.0;
const INDICATOR_RADIUS: f32 = 3.0;
const INDICATOR_RADIUS_HOVER: f32 = 6.0;
const INDICATOR_RADIUS_EXPANDED: f32 = 7.0;

pub struct LabelPlacer {
    occupied: Vec<Rect>,
}

impl LabelPlacer {
    pub fn new() -> Self {
        Self {
            occupied: Vec::new(),
        }
    }

    /// Find a position for an indicator of `size` near `anchor_rect`, avoiding overlap
    /// with previously placed indicators. Slides clockwise around the anchor rect
    /// starting from the right-top corner, skipping past occupied regions.
    /// Returns the top-left position of the indicator rect.
    pub fn place(&mut self, anchor_rect: Rect, size: Vec2) -> Pos2 {
        let gap = INDICATOR_GAP;

        let pos = self
            .try_slide(
                size,
                anchor_rect.top(),
                anchor_rect.bottom(),
                |t| Pos2::new(anchor_rect.right() + gap, t),
                |occ| occ.max.y,
            )
            .or_else(|| {
                self.try_slide(
                    size,
                    anchor_rect.right() - size.x,
                    anchor_rect.left(),
                    |t| Pos2::new(t, anchor_rect.bottom() + gap),
                    |occ| occ.min.x - size.x,
                )
            })
            .or_else(|| {
                self.try_slide(
                    size,
                    anchor_rect.bottom() - size.y,
                    anchor_rect.top(),
                    |t| Pos2::new(anchor_rect.left() - gap - size.x, t),
                    |occ| occ.min.y - size.y,
                )
            })
            .or_else(|| {
                self.try_slide(
                    size,
                    anchor_rect.left(),
                    anchor_rect.right() - size.x,
                    |t| Pos2::new(t, anchor_rect.top() - gap - size.y),
                    |occ| occ.max.x,
                )
            })
            .unwrap_or_else(|| Pos2::new(anchor_rect.right() + gap, anchor_rect.top()));

        self.occupied.push(Rect::from_min_size(pos, size));
        pos
    }

    fn try_slide(
        &self,
        size: Vec2,
        start: f32,
        end: f32,
        make_pos: impl Fn(f32) -> Pos2,
        push_past: impl Fn(&Rect) -> f32,
    ) -> Option<Pos2> {
        let forward = end >= start;
        let mut t = start;

        for _ in 0..self.occupied.len() + 1 {
            if (forward && t > end) || (!forward && t < end) {
                return None;
            }

            let pos = make_pos(t);
            let rect = Rect::from_min_size(pos, size);

            let mut blocked = false;
            let mut next_t = t;
            for occ in &self.occupied {
                if overlap_area(rect, *occ) > 0.0 {
                    blocked = true;
                    let push = push_past(occ);
                    if forward {
                        next_t = next_t.max(push);
                    } else {
                        next_t = next_t.min(push);
                    }
                }
            }

            if !blocked {
                return Some(pos);
            }
            t = next_t;
        }

        None
    }
}

fn placed_prefixes_from(plugin: &DebugValsPlugin) -> Vec<String> {
    let mut prefixes = BTreeSet::new();
    for (val_id, _) in &plugin.placed {
        if let Some(entry) = plugin.values.get(val_id) {
            let key = entry.metadata.display_label();
            let prefix = key.split_once(KEY_DELIMITER).map_or(key, |(p, _)| p);
            prefixes.insert(prefix.to_string());
        }
    }
    prefixes.into_iter().collect()
}

fn overlap_area(a: Rect, b: Rect) -> f32 {
    let x = (a.max.x.min(b.max.x) - a.min.x.max(b.min.x)).max(0.0);
    let y = (a.max.y.min(b.max.y) - a.min.y.max(b.min.y)).max(0.0);
    x * y
}

fn paint_indicator(painter: &Painter, center: Pos2, radius: f32, color: Color32, expanded: bool) {
    let stroke = Stroke::new(2.0, Color32::BLACK);
    painter.add(Shape::circle_stroke(center, radius, stroke));
    painter.add(Shape::circle_filled(center, radius, color));

    if expanded {
        let x_size = radius * 0.45;
        let x_stroke = Stroke::new(1.5, Color32::BLACK);
        painter.line_segment(
            [center - vec2(x_size, x_size), center + vec2(x_size, x_size)],
            x_stroke,
        );
        painter.line_segment(
            [
                center - vec2(-x_size, x_size),
                center + vec2(-x_size, x_size),
            ],
            x_stroke,
        );
    }
}

fn paint_dashed_rect(painter: &Painter, rect: Rect, color: Color32) {
    let stroke = Stroke::new(1.0, color);
    painter.add(Shape::dashed_line(
        &[rect.left_top(), rect.right_top()],
        stroke,
        2.0,
        4.0,
    ));
    painter.add(Shape::dashed_line(
        &[rect.left_bottom(), rect.right_bottom()],
        stroke,
        2.0,
        4.0,
    ));
    painter.add(Shape::dashed_line(
        &[rect.left_top(), rect.left_bottom()],
        stroke,
        2.0,
        4.0,
    ));
    painter.add(Shape::dashed_line(
        &[rect.right_top(), rect.right_bottom()],
        stroke,
        2.0,
        4.0,
    ));
}

static PLUGIN_HANDLE: Mutex<Option<TypedPluginHandle<DebugValsPlugin>>> = Mutex::new(None);

fn with_plugin<R>(f: impl FnOnce(&mut DebugValsPlugin) -> R) -> Option<R> {
    let guard = PLUGIN_HANDLE.lock().unwrap();
    guard.as_ref().map(|handle| f(&mut *handle.lock()))
}

pub fn refresh_plugin_handle(ctx: &Context) {
    *PLUGIN_HANDLE.lock().unwrap() = Some(ctx.plugin::<DebugValsPlugin>());
}

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
        self.custom_key
            .as_deref()
            .unwrap_or(self.module_path)
            .trim_matches('"')
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
    fn render_value_ui(&mut self, _ui: &mut Ui, _metadata: &DebugValMetadata, _params: &ValParams) {
    }

    /// Return a short string representation of the current value.
    fn display_value(&self) -> String;

    /// Return a Rust expression that reconstructs this value, for pasting into source code.
    fn as_rust_literal(&self) -> String;

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
    default: Box<dyn DebugVal>,
    metadata: DebugValMetadata,
    params: ValParams,
}

impl DebugValEntry {
    fn is_modified(&self) -> bool {
        self.value.display_value() != self.default.display_value()
    }
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Options {
    pub show_app_controlled: bool,
    pub hidden_key_prefix_filter: String,
    pub shown_placed_prefixes: BTreeSet<String>,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            show_app_controlled: false,
            hidden_key_prefix_filter: String::new(),
            shown_placed_prefixes: BTreeSet::new(),
        }
    }
}

impl Options {
    fn id() -> Id {
        Id::new("gaze/debug_vals/options")
    }

    fn load(ctx: &Context) -> Self {
        ctx.data_mut(|data| data.get_persisted::<Self>(Self::id()))
            .unwrap_or_default()
    }

    fn save(&self, ctx: &Context) {
        ctx.data_mut(|data| data.insert_persisted(Self::id(), self.clone()));
    }
}

/// Plugin that stores all debug values.
pub struct DebugValsPlugin {
    values: HashMap<Id, DebugValEntry>,
    order: BTreeMap<String, Id>,
    ctx: Option<Context>,
    placed: Vec<(Id, Id)>,
    expanded_placed_groups: HashSet<Id>,
    options: Options,
}

impl DebugValsPlugin {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            order: BTreeMap::new(),
            ctx: None,
            placed: Vec::new(),
            expanded_placed_groups: HashSet::new(),
            options: Options::default(),
        }
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

        let default_val = T::default();

        if !metadata.app_controlled {
            if let Some(ctx) = &self.ctx {
                if let Some(loaded) = T::load_persisted(ctx, id) {
                    if let Some(typed) = (loaded.as_ref() as &dyn Any).downcast_ref::<T>() {
                        let val = typed.clone();
                        let label = metadata.display_label().to_string();
                        self.values.insert(
                            id,
                            DebugValEntry {
                                value: loaded,
                                default: Box::new(default_val),
                                metadata,
                                params,
                            },
                        );
                        self.order.insert(label, id);
                        return val;
                    }
                }
            }
        }

        let label = metadata.display_label().to_string();
        self.values.insert(
            id,
            DebugValEntry {
                value: Box::new(default_val.clone()),
                default: Box::new(default_val.clone()),
                metadata,
                params,
            },
        );
        self.order.insert(label, id);
        default_val
    }

    /// Like `get_or_insert`, but uses a caller-provided default instead of `T::default()`.
    pub fn get_or_insert_with<T>(
        &mut self,
        id: Id,
        metadata: DebugValMetadata,
        params: ValParams,
        default: T,
    ) -> T
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
                        self.values.insert(
                            id,
                            DebugValEntry {
                                value: loaded,
                                default: Box::new(default),
                                metadata,
                                params,
                            },
                        );
                        self.order.insert(label, id);
                        return val;
                    }
                }
            }
        }

        let label = metadata.display_label().to_string();
        self.values.insert(
            id,
            DebugValEntry {
                value: Box::new(default.clone()),
                default: Box::new(default.clone()),
                metadata,
                params,
            },
        );
        self.order.insert(label, id);
        default
    }

    /// Set a value in storage.
    pub fn set<T>(&mut self, id: Id, metadata: DebugValMetadata, params: ValParams, value: T)
    where
        T: Clone + DebugVal + 'static,
    {
        let label = metadata.display_label().to_string();
        if let Some(entry) = self.values.get_mut(&id) {
            entry.value = Box::new(value);
            entry.metadata = metadata;
            entry.params = params;
        } else {
            self.values.insert(
                id,
                DebugValEntry {
                    value: Box::new(value.clone()),
                    default: Box::new(value),
                    metadata,
                    params,
                },
            );
        }
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
            .extract_if((Bound::Included(lower), Bound::Excluded(upper)), |_, _| {
                true
            })
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
        self.options = Options::load(ctx);
        *PLUGIN_HANDLE.lock().unwrap() = Some(ctx.plugin::<DebugValsPlugin>());
        self.ctx = Some(ctx.clone());
    }

    fn on_begin_pass(&mut self, ui: &mut Ui) {
        *PLUGIN_HANDLE.lock().unwrap() = Some(ui.ctx().plugin::<DebugValsPlugin>());
    }

    fn on_end_pass(&mut self, ui: &mut Ui) {
        crate::hot_call(|| self.render_placed_vals(ui.ctx()));
    }
}

impl DebugValsPlugin {
    /// Render placed val overlays. Can be called explicitly by the app at the end of its
    /// render loop for hot-reload support: since hot-reloaded code replaces the app's
    /// update fn but not the plugin's on_end_pass, calling this directly ensures the
    /// overlays use the latest reloaded code path.
    ///
    /// Uses `take` on `self.placed`, so calling from both the app and on_end_pass is safe —
    /// whichever runs first gets the data, the second call is a no-op.
    pub fn show(ctx: &Context) {
        with_plugin(|plugin| plugin.render_placed_vals(ctx));
    }

    fn render_placed_vals(&mut self, ctx: &Context) {
        let placed = std::mem::take(&mut self.placed);
        if self.options.shown_placed_prefixes.is_empty() {
            return;
        }

        let mut group_index: HashMap<Id, usize> = HashMap::new();
        let mut groups: Vec<(Id, WidgetGroup)> = Vec::new();

        for (val_id, widget_id) in &placed {
            let prefix = self
                .values
                .get(val_id)
                .map(|e| {
                    let key = e.metadata.display_label();
                    key.split_once(KEY_DELIMITER)
                        .map_or(key, |(p, _)| p)
                        .to_string()
                })
                .unwrap_or_default();

            if !self.options.shown_placed_prefixes.contains(&prefix) {
                continue;
            }

            let idx = *group_index.entry(*widget_id).or_insert_with(|| {
                let rect = ctx
                    .read_response(*widget_id)
                    .map(|response| {
                        let mut rect = response.rect;
                        if let Some(transform) = ctx.layer_transform_to_global(response.layer_id) {
                            rect = transform * rect;
                        }
                        rect
                    })
                    .unwrap_or(Rect::NOTHING);
                groups.push((
                    *widget_id,
                    WidgetGroup {
                        rect,
                        val_ids: Vec::new(),
                        prefix,
                    },
                ));
                groups.len() - 1
            });
            groups[idx].1.val_ids.push(*val_id);
        }

        let hover_size = vec2(INDICATOR_RADIUS_HOVER * 2.0, INDICATOR_RADIUS_HOVER * 2.0);
        let mut placer = LabelPlacer::new();
        let screen_rect = ctx.content_rect();

        for (widget_id, group) in &groups {
            if group.rect == Rect::NOTHING || !screen_rect.intersects(group.rect) {
                continue;
            }
            let indicator_pos = placer.place(group.rect, hover_size);
            let color = placed_dot_color(&group.prefix);
            let area_id = Id::new("placed-val-group").with(widget_id);
            let is_expanded = self.expanded_placed_groups.contains(widget_id);

            egui::Area::new(area_id)
                .order(egui::Order::Debug)
                .fixed_pos(indicator_pos)
                .interactable(true)
                .show(ctx, |ui| {
                    let (_allocated_rect, response) =
                        ui.allocate_exact_size(hover_size, Sense::click_and_drag());
                    let hovered = response.hovered();
                    // let active = hovered || is_expanded;

                    let radius = if is_expanded {
                        INDICATOR_RADIUS_EXPANDED + 1.0
                    } else if hovered {
                        INDICATOR_RADIUS_HOVER
                    } else {
                        INDICATOR_RADIUS
                    };
                    let dot_center = indicator_pos + hover_size * 0.5;
                    paint_indicator(&ctx.debug_painter(), dot_center, radius, color, is_expanded);

                    if hovered {
                        paint_dashed_rect(&ctx.debug_painter(), group.rect, color);
                    } else if let Some(pointer_pos) = ctx.input(|i| i.pointer.hover_pos()) {
                        let dist = group.rect.distance_to_pos(pointer_pos);
                        let max_dist = radius * 3.0;
                        if dist < max_dist {
                            let alpha = ((1.0 - dist / max_dist) * 128.0) as u8;
                            let faded = Color32::from_rgba_unmultiplied(
                                color.r(),
                                color.g(),
                                color.b(),
                                alpha,
                            );
                            paint_dashed_rect(&ctx.debug_painter(), group.rect, faded);
                        }
                    }

                    if response.clicked() {
                        if is_expanded {
                            self.expanded_placed_groups.remove(widget_id);
                        } else {
                            self.expanded_placed_groups.insert(*widget_id);
                        }
                    }

                    if is_expanded {
                        egui::Frame::new()
                            .fill(LABEL_BG)
                            .stroke(Stroke::new(1.0, Color32::from_gray(28)))
                            .corner_radius(3.0)
                            .inner_margin(Margin {
                                left: 6,
                                right: 2,
                                top: 2,
                                bottom: 2,
                            })
                            .show(ui, |ui| {
                                let style = ui.style_mut();
                                style.spacing.interact_size = vec2(15.0, 15.0);
                                style.spacing.item_spacing = vec2(0.0, 0.0);
                                style.override_text_style = Some(TextStyle::Small);

                                let rows: Vec<PlacedValRow> = group
                                    .val_ids
                                    .iter()
                                    .filter_map(|val_id| {
                                        let entry = self.values.get(val_id)?;
                                        let key = entry.metadata.display_label();
                                        let last = key
                                            .rsplit_once(KEY_DELIMITER)
                                            .map_or(key, |(_, last)| last);
                                        Some(PlacedValRow {
                                            name: last.to_string(),
                                            val_id: *val_id,
                                        })
                                    })
                                    .collect();

                                let interact_height = ui.spacing().interact_size.y;
                                let btn_col_width = interact_height * 3.0 + 6.0;
                                let columns = vec![
                                    Column::new(60.0)
                                        .resizable(true)
                                        .range(Rangef::new(40.0, 200.0)),
                                    Column::new(100.0)
                                        .resizable(true)
                                        .range(Rangef::new(40.0, f32::INFINITY)),
                                    Column::new(btn_col_width)
                                        .resizable(false)
                                        .range(Rangef::new(btn_col_width, btn_col_width)),
                                ];

                                let num_rows = rows.len() as u64;
                                ui.set_min_size(Vec2::new(
                                    500.0,
                                    (num_rows + 1) as f32 * interact_height * 1.0,
                                ));

                                let mut delegate = PlacedValsTableDelegate {
                                    rows: &rows,
                                    values: &mut self.values,
                                    interact_height,
                                };
                                Table::new()
                                    .id_salt(("__placed_vals_table", widget_id))
                                    .num_rows(num_rows)
                                    .columns(columns)
                                    .headers(vec![])
                                    .auto_size_mode(AutoSizeMode::Always)
                                    .show(ui, &mut delegate);
                            });
                    } else {
                        response.on_hover_ui_at_pointer(|ui| {
                            for val_id in &group.val_ids {
                                if let Some(entry) = self.values.get(val_id) {
                                    let key = entry.metadata.display_label();
                                    let last = key
                                        .rsplit_once(KEY_DELIMITER)
                                        .map_or(key, |(_, last)| last);
                                    ui.label(
                                        RichText::new(format!(
                                            "{last}: {}",
                                            entry.value.display_value()
                                        ))
                                        .monospace()
                                        .size(9.0),
                                    );
                                }
                            }
                        });
                    }
                });
        }
    }
}
struct WidgetGroup {
    rect: Rect,
    val_ids: Vec<Id>,
    prefix: String,
}

/// A handle to a debug value that writes back on drop.
pub struct ValHandle<T: Clone + DebugVal> {
    id: Id,
    metadata: DebugValMetadata,
    params: ValParams,
    value: T,
}

impl<T: Clone + Default + DebugVal + 'static> ValHandle<T> {
    pub fn new(id: Id, metadata: DebugValMetadata, params: ValParams) -> Self {
        let metadata = Self::apply_prefix(metadata);
        let id = Id::new(
            metadata
                .custom_key
                .as_deref()
                .unwrap_or(metadata.file_line_col),
        );
        let value = with_plugin(|p| p.get_or_insert::<T>(id, metadata.clone(), params.clone()))
            .unwrap_or_default();
        auto_place(id);
        Self {
            id,
            metadata,
            params,
            value,
        }
    }
}

impl<T: Clone + DebugVal + 'static> ValHandle<T> {
    pub fn get(&self) -> T {
        self.value.clone()
    }

    pub fn set(&mut self, val: T) {
        self.value = val;
    }

    fn apply_prefix(mut metadata: DebugValMetadata) -> DebugValMetadata {
        if let Some(ref key) = metadata.custom_key {
            let prefixed = prefixed_key(key);
            if prefixed != *key {
                metadata.custom_key = Some(prefixed);
            }
        }
        metadata
    }

    pub fn with_default(
        id: Id,
        metadata: DebugValMetadata,
        params: ValParams,
        default: impl Into<T>,
    ) -> Self {
        let metadata = Self::apply_prefix(metadata);
        let id = Id::new(
            metadata
                .custom_key
                .as_deref()
                .unwrap_or(metadata.file_line_col),
        );
        let default = default.into();
        let value = with_plugin(|p| {
            p.get_or_insert_with::<T>(id, metadata.clone(), params.clone(), default.clone())
        })
        .unwrap_or(default);
        auto_place(id);
        Self {
            id,
            metadata,
            params,
            value,
        }
    }

    pub fn place(self, ui: &Ui) -> Self {
        let id = self.id;
        let widget_id = ui.unique_id();
        with_plugin(|p| p.placed.push((id, widget_id)));
        self
    }
}

impl<T: Clone + DebugVal> std::ops::Deref for ValHandle<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: Clone + DebugVal> std::ops::DerefMut for ValHandle<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T: Clone + DebugVal + 'static> Drop for ValHandle<T> {
    fn drop(&mut self) {
        with_plugin(|p| {
            p.set(
                self.id,
                self.metadata.clone(),
                self.params.clone(),
                self.value.clone(),
            )
        });
    }
}

#[macro_export]
macro_rules! show_debug {
    () => {{
        let module_path = module_path!();
        let module = module_path
            .rsplit_once("::")
            .map(|(_, name)| name)
            .unwrap_or(module_path);
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
macro_rules! val_handle {
  ($ty:ty) => {{
    let file_line_col = concat!(file!(), ":", line!(), ":", column!());
    let key = $crate::vals::Id::new(file_line_col);
    let metadata = $crate::vals::DebugValMetadata {
      file_line_col,
      module_path: module_path!(),
      custom_key: None,
      app_controlled: false,
    };
    $crate::vals::ValHandle::<$ty>::new(key, metadata, $crate::vals::ValParams::default())
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
      app_controlled: false,
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
      app_controlled: false,
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
      app_controlled: false,
    };
    $crate::vals::ValHandle::<$ty>::new(id, metadata, $crate::vals::ValParams::default())
  }};
}

/// Get a mutable debug value handle. Uses file/line/column as key if no custom key provided.
/// Returns a `ValHandle<T>` that can be modified and writes back on drop.
/// ```ignore
/// let speed = val_mut!(bool, "speed", min = 0.0, max = 1.0).place(ui);
/// ```
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
    with_plugin(|p| p.clear(prefix));
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
            key.split(KEY_DELIMITER)
                .filter(|segment| !segment.is_empty())
                .map(|segment| segment.trim_matches('"').to_string())
                .collect::<Vec<String>>()
        });
        let response = ui.vertical(|ui| {
            let guard = PLUGIN_HANDLE.lock().unwrap();
            if let Some(handle) = guard.as_ref() {
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
                            let placed_prefixes = placed_prefixes_from(&plugin);
                            let placed_count = plugin.placed.len();
                            let caption = format!("Placed {placed_count}");
                            egui::ComboBox::from_id_salt("placed_prefixes_dropdown")
                                .selected_text(caption)
                                .show_ui(ui, |ui| {
                                    for prefix in &placed_prefixes {
                                        let color = placed_dot_color(prefix);
                                        let mut enabled =
                                            plugin.options.shown_placed_prefixes.contains(prefix);
                                        ui.horizontal(|ui| {
                                            let (dot_rect, _) = ui.allocate_exact_size(
                                                vec2(8.0, 8.0),
                                                egui::Sense::empty(),
                                            );
                                            ui.painter().circle_filled(
                                                dot_rect.center(),
                                                3.0,
                                                color,
                                            );
                                            if ui
                                                .toggle_value(&mut enabled, prefix.as_str())
                                                .changed()
                                            {
                                                if enabled {
                                                    plugin
                                                        .options
                                                        .shown_placed_prefixes
                                                        .insert(prefix.clone());
                                                } else {
                                                    plugin
                                                        .options
                                                        .shown_placed_prefixes
                                                        .remove(prefix);
                                                }
                                                plugin.options.save(ui.ctx());
                                            }
                                        });
                                    }
                                });

                            if ui
                                .toggle_value(&mut plugin.options.show_app_controlled, "Muts")
                                .changed()
                            {
                                plugin.options.save(ui.ctx());
                            }

                            let is_filter_active =
                                !plugin.options.hidden_key_prefix_filter.trim().is_empty();
                            let filter_compile = compile_hidden_prefix_filter_regex(
                                &plugin.options.hidden_key_prefix_filter,
                            );
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
                  TextEdit::singleline(&mut plugin.options.hidden_key_prefix_filter)
                    .desired_width(180.0)
                    .hint_text("Hide key prefixes: foo|bar"),
                )
                .on_hover_text("Regex key prefixes to hide (use | to separate multiple prefixes)");
                            if filter_response.changed() {
                                plugin.options.save(ui.ctx());
                            }
                        });
                    });
                }

                let hidden_prefix_filter =
                    compile_hidden_prefix_filter_regex(&plugin.options.hidden_key_prefix_filter)
                        .ok()
                        .flatten();

                let DebugValsPlugin {
                    values,
                    order,
                    options,
                    ..
                } = &*plugin;

                let entries: Vec<(Vec<String>, Id)> = order
                    .iter()
                    .filter(|(_, id)| {
                        options.show_app_controlled
                            || values.get(id).map_or(true, |e| !e.metadata.app_controlled)
                    })
                    .filter_map(|(label, &id)| {
                        let segments: Vec<String> = label
                            .split(KEY_DELIMITER)
                            .map(|s| s.trim_matches('"').to_string())
                            .collect();
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
                                filtered_segments = segments.last().map_or_else(
                                    || vec![String::new()],
                                    |name| vec![name.to_string()],
                                );
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

                let mut flat_rows = Vec::new();
                flatten_tree(&root, 0, "", ui.ctx(), &mut flat_rows);

                if !flat_rows.is_empty() {
                    let interact_height = ui.spacing().interact_size.y;
                    let btn_col_width = interact_height * 3.0 + 6.0;
                    let available = ui.available_width();
                    let reserved = if self.can_navigate {
                        btn_col_width
                    } else {
                        0.0
                    };
                    let name_col_width = (available * 0.35).clamp(60.0, 300.0);
                    let value_col_width = (available - name_col_width - reserved).max(60.0);

                    let mut columns = vec![
                        Column::new(name_col_width)
                            .resizable(self.resizable_columns)
                            .range(Rangef::new(40.0, 400.0)),
                        Column::new(value_col_width)
                            .resizable(self.resizable_columns)
                            .range(Rangef::new(40.0, f32::INFINITY)),
                    ];
                    if self.can_navigate {
                        columns.push(
                            Column::new(btn_col_width)
                                .resizable(false)
                                .range(Rangef::new(btn_col_width, btn_col_width)),
                        );
                    }

                    let auto_size = if self.compact {
                        AutoSizeMode::Never
                    } else {
                        AutoSizeMode::Always
                    };
                    let num_rows = flat_rows.len() as u64;
                    let mut delegate = ValsTableDelegate {
                        rows: &flat_rows,
                        plugin: &mut plugin,
                        interact_height,
                        striped: self.striped,
                    };
                    Table::new()
                        .id_salt("__vals_table")
                        .num_rows(num_rows)
                        .columns(columns)
                        .headers(vec![])
                        .auto_size_mode(auto_size)
                        .show(ui, &mut delegate);
                }
            } else if self.show_empty_message {
                ui.label("DebugVals plugin not initialized");
            }
        });

        response.response
    }
}

fn compile_hidden_prefix_filter_regex(input: &str) -> Result<Option<Regex>, regex::Error> {
    let prefixes: Vec<&str> = input
        .split('|')
        .map(str::trim)
        .filter(|part| !part.is_empty())
        .collect();
    if prefixes.is_empty() {
        return Ok(None);
    }

    let alternation = prefixes
        .into_iter()
        .map(|prefix| format!("(?:{prefix})"))
        .collect::<Vec<String>>()
        .join("|");
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
            self.branches
                .entry(&segments[0])
                .or_default()
                .insert(&segments[1..], id);
        }
    }
}

enum FlatRow {
    Section {
        name: String,
        depth: usize,
        id: Id,
    },
    Leaf {
        name: String,
        val_id: Id,
        depth: usize,
    },
}

fn flatten_tree(
    node: &TreeNode<'_>,
    depth: usize,
    path: &str,
    ctx: &Context,
    out: &mut Vec<FlatRow>,
) {
    for (name, child) in &node.branches {
        let section_path = if path.is_empty() {
            name.to_string()
        } else {
            format!("{path}/{name}")
        };
        let section_id = Id::new("__vals_section").with(&section_path);
        out.push(FlatRow::Section {
            name: name.to_string(),
            depth,
            id: section_id,
        });

        let expanded = ctx
            .data_mut(|data| data.get_persisted::<bool>(section_id))
            .unwrap_or(true);
        if expanded {
            flatten_tree(child, depth + 1, &section_path, ctx, out);
        }
    }

    for (name, id) in &node.leaves {
        out.push(FlatRow::Leaf {
            name: name.to_string(),
            val_id: *id,
            depth,
        });
    }
}

struct PlacedValRow {
    name: String,
    val_id: Id,
}

struct PlacedValsTableDelegate<'a> {
    rows: &'a [PlacedValRow],
    values: &'a mut HashMap<Id, DebugValEntry>,
    interact_height: f32,
}

impl TableDelegate for PlacedValsTableDelegate<'_> {
    fn default_row_height(&self) -> f32 {
        self.interact_height + 1.0
    }

    fn header_cell_ui(&mut self, _ui: &mut Ui, _cell: &HeaderCellInfo) {}

    fn cell_ui(&mut self, ui: &mut Ui, cell: &CellInfo) {
        let row = &self.rows[cell.row_nr as usize];
        if let Some(entry) = self.values.get_mut(&row.val_id) {
            let modified = entry.is_modified();
            let metadata = entry.metadata.clone();
            match cell.col_nr {
                0 => {
                    let dim = Color32::from_rgb(160, 160, 160);
                    let bright = Color32::from_rgb(220, 220, 220);
                    let color = if modified { bright } else { dim };
                    ui.add(
                        Label::new(RichText::new(&row.name).monospace().size(9.0).color(color))
                            .truncate(),
                    );
                }
                1 => {
                    ui.add_space(2.0);
                    ui.spacing_mut().item_spacing = vec2(4.0, 0.0);
                    ui.horizontal_wrapped(|ui| {
                        entry.value.render_value_ui(ui, &metadata, &entry.params);
                    });
                    if !entry.metadata.app_controlled {
                        entry.value.save_persisted(ui.ctx(), row.val_id);
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
                        let source = SourceLocation {
                            path: metadata.file_line_col.to_string(),
                            line: 0,
                            column: 0,
                        };
                        open_file(ui.ctx(), &source);
                    }
                    let literal = entry.value.as_rust_literal();
                    if ui
                        .add(
                            Button::new(RichText::new("=").monospace().size(10.0))
                                .min_size(Vec2::splat(self.interact_height))
                                .frame(false)
                                .frame_when_inactive(false),
                        )
                        .on_hover_text(format!("Copy: {literal}"))
                        .clicked()
                    {
                        ui.ctx().copy_text(literal);
                    }
                    let reset_response = ui.add_enabled(
                        modified,
                        Button::new(RichText::new("↺").size(10.0))
                            .min_size(Vec2::splat(self.interact_height))
                            .frame(false)
                            .frame_when_inactive(false),
                    );
                    if modified {
                        reset_response
                            .on_hover_text("Reset to default")
                            .clicked()
                            .then(|| {
                                entry.value = entry.default.clone_boxed();
                                entry.value.save_persisted(ui.ctx(), row.val_id);
                            });
                    }
                    ui.add_space(1.0);
                }
                _ => {}
            }
        }
    }
}

struct ValsTableDelegate<'a> {
    rows: &'a [FlatRow],
    plugin: &'a mut DebugValsPlugin,
    interact_height: f32,
    striped: bool,
}

impl ValsTableDelegate<'_> {
    fn toggle_section(&self, ui: &Ui, section_id: Id) {
        let expanded = ui
            .ctx()
            .data_mut(|data| data.get_persisted::<bool>(section_id))
            .unwrap_or(true);
        ui.ctx()
            .data_mut(|data| data.insert_persisted(section_id, !expanded));
        ui.ctx().request_repaint();
    }
}

impl TableDelegate for ValsTableDelegate<'_> {
    fn default_row_height(&self) -> f32 {
        self.interact_height + 1.0
    }

    fn header_cell_ui(&mut self, _ui: &mut Ui, _cell: &HeaderCellInfo) {}

    fn row_ui(&mut self, ui: &mut Ui, row_nr: u64) {
        let row = &self.rows[row_nr as usize];
        match row {
            FlatRow::Section { .. } => {
                ui.painter().rect_filled(
                    ui.available_rect_before_wrap(),
                    0.0,
                    ui.visuals().faint_bg_color,
                );
            }
            FlatRow::Leaf { .. } => {
                if self.striped && row_nr % 2 == 1 {
                    ui.painter().rect_filled(
                        ui.available_rect_before_wrap(),
                        0.0,
                        ui.visuals().faint_bg_color,
                    );
                }
            }
        }
    }

    fn cell_ui(&mut self, ui: &mut Ui, cell: &CellInfo) {
        let row = &self.rows[cell.row_nr as usize];
        match row {
            FlatRow::Section { name, depth, id } => {
                if cell.col_nr == 0 {
                    let indent = *depth as f32 * HIERARCHY_INDENT;
                    ui.add_space(indent);

                    let expanded = ui
                        .ctx()
                        .data_mut(|data| data.get_persisted::<bool>(*id))
                        .unwrap_or(true);
                    let openness = ui.ctx().animate_bool(*id, expanded);

                    let icon_size = Vec2::splat(self.interact_height * 0.6);
                    let (_, icon_response) = ui.allocate_exact_size(icon_size, Sense::click());
                    paint_default_icon(ui, openness, &icon_response);

                    let label_response =
                        ui.add(Label::new(RichText::new(name).strong()).truncate());

                    if icon_response.clicked() || label_response.clicked() {
                        self.toggle_section(ui, *id);
                    }
                }
            }
            FlatRow::Leaf {
                name,
                val_id,
                depth,
            } => {
                if let Some(entry) = self.plugin.values.get_mut(val_id) {
                    let modified = entry.is_modified();
                    let metadata = DebugValMetadata {
                        file_line_col: entry.metadata.file_line_col,
                        module_path: entry.metadata.module_path,
                        custom_key: Some(name.clone()),
                        app_controlled: entry.metadata.app_controlled,
                    };
                    match cell.col_nr {
                        0 => {
                            let indent = *depth as f32 * HIERARCHY_INDENT;
                            ui.add_space(indent);
                            let mut text = RichText::new(metadata.display_label());
                            if modified {
                                text = text.strong();
                            }
                            ui.add(Label::new(text).truncate())
                                .on_hover_text(metadata.file_line_col);
                        }
                        1 => {
                            ui.add_space(2.0);
                            ui.spacing_mut().item_spacing = vec2(1.0, 1.0);
                            entry.value.render_value_ui(ui, &metadata, &entry.params);
                            if !entry.metadata.app_controlled {
                                entry.value.save_persisted(ui.ctx(), *val_id);
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
                                let source = SourceLocation {
                                    path: metadata.file_line_col.to_string(),
                                    line: 0,
                                    column: 0,
                                };
                                open_file(ui.ctx(), &source);
                            }
                            let literal = entry.value.as_rust_literal();
                            if ui
                                .add(
                                    Button::new(RichText::new("=").monospace().size(10.0))
                                        .min_size(Vec2::splat(self.interact_height))
                                        .frame(false)
                                        .frame_when_inactive(false),
                                )
                                .on_hover_text(format!("Copy: {literal}"))
                                .clicked()
                            {
                                ui.ctx().copy_text(literal);
                            }
                            let reset_response = ui.add_enabled(
                                modified,
                                Button::new(RichText::new("↺").size(10.0))
                                    .min_size(Vec2::splat(self.interact_height))
                                    .frame(false)
                                    .frame_when_inactive(false),
                            );
                            if modified {
                                reset_response
                                    .on_hover_text("Reset to default")
                                    .clicked()
                                    .then(|| {
                                        entry.value = entry.default.clone_boxed();
                                        entry.value.save_persisted(ui.ctx(), *val_id);
                                    });
                            }
                            ui.add_space(1.0);
                        }
                        _ => {}
                    }
                }
            }
        }
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
  ($ty:ty, $display_fmt:literal, |$lit_self:ident| $literal:expr, |$self:ident, $ui:ident, $meta:ident, $params:ident| $control:expr) => {
    impl DebugVal for $ty {
      #[allow(unused_variables)]
      fn render_value_ui(&mut $self, $ui: &mut Ui, $meta: &DebugValMetadata, $params: &ValParams) {
        $control
      }
      fn display_value(&self) -> String {
        format!($display_fmt, self)
      }
      fn as_rust_literal(&$lit_self) -> String {
        $literal
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
        impl_debug_val!(
            $ty,
            $format,
            |self| format!($format, self),
            |self, ui, metadata, params| {
                if metadata.app_controlled {
                    ui.add(Label::new(
                        RichText::new(format!($format, self)).monospace(),
                    ));
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
            }
        );
    };
}

impl_debug_val_numeric!(f32, 0.01, "{:.3}");
impl_debug_val_numeric!(f64, 0.01, "{:.3}");
impl_debug_val_numeric!(i32, 0.1, "{}");
impl_debug_val_numeric!(u32, 0.1, "{}");
impl_debug_val_numeric!(i16, 0.1, "{}");
impl_debug_val_numeric!(u16, 0.1, "{}");
impl_debug_val_numeric!(usize, 0.1, "{}");
impl_debug_val_numeric!(isize, 0.1, "{}");

impl_debug_val!(
    bool,
    "{}",
    |self| format!("{}", self),
    |self, ui, metadata, _params| {
        if metadata.app_controlled {
            let text = if *self { "true" } else { "false" };
            ui.add(Label::new(RichText::new(text).monospace()));
        } else {
            ui.checkbox(self, "").on_hover_text(metadata.file_line_col);
        }
    }
);

impl_debug_val!(
    String,
    "{}",
    |self| format!("{:?}", self),
    |self, ui, metadata, params| {
        if metadata.app_controlled {
            if self.is_empty() {
                ui.add(Label::new(RichText::new("(empty)").weak()).truncate());
            } else {
                ui.add(Label::new(self.as_str()).truncate());
            }
        } else if let Some(options) = &params.options {
            let selected = if self.is_empty() {
                "(none)"
            } else {
                self.as_str()
            };
            egui::ComboBox::from_id_salt(metadata.file_line_col)
                .selected_text(selected)
                .show_ui(ui, |ui| {
                    for opt in options {
                        ui.selectable_value(self, opt.clone(), opt.as_str());
                    }
                });
        } else {
            let mut temp = self.as_str().to_string();
            let desired_width = if ui.is_sizing_pass() {
                200.0
            } else {
                ui.available_width()
            };
            if ui
                .add(TextEdit::singleline(&mut temp).desired_width(desired_width))
                .changed()
            {
                *self = temp;
            }
        }
    }
);

impl_debug_val!(
    Color32,
    "{:?}",
    |self| {
        let [r, g, b, a] = self.to_array();
        if a == 255 {
            format!("Color32::from_rgb({r}, {g}, {b})")
        } else {
            format!("Color32::from_rgba_premultiplied({r}, {g}, {b}, {a})")
        }
    },
    |self, ui, metadata, _params| {
        if metadata.app_controlled {
            ui.add(Label::new(
                RichText::new(format!("{:?}", *self)).monospace(),
            ));
        } else {
            egui::color_picker::color_edit_button_srgba(
                ui,
                self,
                egui::color_picker::Alpha::OnlyBlend,
            );
            ui.label("r:");
            ui.add(DragValue::new(&mut self[0]).speed(0.1));
            ui.label("g:");
            ui.add(DragValue::new(&mut self[1]).speed(0.1));
            ui.label("b:");
            ui.add(DragValue::new(&mut self[2]).speed(0.1));
            ui.label("a:");
            ui.add(DragValue::new(&mut self[3]).speed(0.1));
        }
    }
);

impl_debug_val!(
    Vec2,
    "{:?}",
    |self| format!("vec2({:.3}, {:.3})", self.x, self.y),
    |self, ui, metadata, params| {
        if metadata.app_controlled {
            ui.add(Label::new(
                RichText::new(format!("x: {:.3} y: {:.3}", self.x, self.y)).monospace(),
            ));
        } else {
            let speed = params.speed.unwrap_or(0.1) as f32;
            ui.label("x:");
            ui.add(DragValue::new(&mut self.x).speed(speed));
            ui.label("y:");
            ui.add(DragValue::new(&mut self.y).speed(speed));
        }
    }
);

impl_debug_val!(
    Pos2,
    "{:?}",
    |self| format!("pos2({:.3}, {:.3})", self.x, self.y),
    |self, ui, metadata, params| {
        if metadata.app_controlled {
            ui.add(Label::new(
                RichText::new(format!("x: {:.3} y: {:.3}", self.x, self.y)).monospace(),
            ));
        } else {
            let speed = params.speed.unwrap_or(0.1) as f32;
            ui.label("x:");
            ui.add(DragValue::new(&mut self.x).speed(speed));
            ui.label("y:");
            ui.add(DragValue::new(&mut self.y).speed(speed));
        }
    }
);

impl_debug_val!(
    TSTransform,
    "{:?}",
    |self| format!(
        "TSTransform {{ translation: vec2({:.3}, {:.3}), scaling: {:.3} }}",
        self.translation.x, self.translation.y, self.scaling
    ),
    |self, ui, metadata, _params| {
        if metadata.app_controlled {
            ui.add(Label::new(
                RichText::new(format!(
                    "tx: {:.3} ty: {:.3} scale: {:.3}",
                    self.translation.x, self.translation.y, self.scaling
                ))
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
    }
);

impl_debug_val!(
    Rect,
    "{:?}",
    |self| format!(
        "Rect::from_min_max(pos2({:.3}, {:.3}), pos2({:.3}, {:.3}))",
        self.min.x, self.min.y, self.max.x, self.max.y
    ),
    |self, ui, metadata, _params| {
        if metadata.app_controlled {
            ui.add(Label::new(
                RichText::new(format!(
                    "min ({:.3}, {:.3}) max ({:.3}, {:.3})",
                    self.min.x, self.min.y, self.max.x, self.max.y
                ))
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
    }
);
impl_debug_val!(
    Shadow,
    "{:?}",
    |self| {
        let [r, g, b, a] = self.color.to_array();
        format!(
            "Shadow {{ offset: [{}, {}], blur: {}, spread: {}, color: Color32::from_rgba_premultiplied({r}, {g}, {b}, {a}) }}",
            self.offset[0], self.offset[1], self.blur, self.spread
        )
    },
    |self, ui, metadata, _params| {
        if metadata.app_controlled {
            ui.add(Label::new(
                RichText::new(format!(
                    "offset: [{}, {}] blur: {} spread: {} color: {:?}",
                    self.offset[0], self.offset[1], self.blur, self.spread, self.color
                ))
                .monospace(),
            ));
        } else {
            // ui.horizontal(|ui| {
            ui.label("x:");
            let mut offset_x = self.offset[0] as i32;
            ui.add(DragValue::new(&mut offset_x).speed(0.01));
            let mut offset_y = self.offset[1] as i32;
            ui.label("y:");
            ui.add(DragValue::new(&mut offset_y).speed(0.01));
            self.offset[0] = offset_x as i8;
            self.offset[1] = offset_y as i8;

            ui.label("blur:");
            let mut blur = self.blur as i32;
            ui.add(DragValue::new(&mut blur).range(0..=64).speed(0.1));
            self.blur = blur as u8;

            ui.label("spread:");
            let mut spread = self.spread as i32;
            ui.add(DragValue::new(&mut spread).range(0..=64).speed(0.1));
            self.spread = spread as u8;

            ui.label("color:");
            let mut color = self.color;
            egui::color_picker::color_edit_button_srgba(
                ui,
                &mut color,
                egui::color_picker::Alpha::OnlyBlend,
            );
            self.color = color;
            // });
        }
    }
);

impl<T: Clone + DebugVal + Send + Sync + 'static> DebugVal for Option<T> {
    fn render_value_ui(&mut self, ui: &mut Ui, metadata: &DebugValMetadata, params: &ValParams) {
        match self {
            Some(inner) => inner.render_value_ui(ui, metadata, params),
            None => {
                ui.add(Label::new(RichText::new("None").weak()));
            }
        }
    }

    fn display_value(&self) -> String {
        match self {
            Some(inner) => inner.display_value(),
            None => "None".into(),
        }
    }

    fn as_rust_literal(&self) -> String {
        match self {
            Some(inner) => format!("Some({})", inner.as_rust_literal()),
            None => "None".into(),
        }
    }

    fn clone_boxed(&self) -> Box<dyn DebugVal> {
        Box::new(self.clone())
    }

    fn save_persisted(&self, _ctx: &Context, _id: Id) {}

    fn load_persisted(_ctx: &Context, _id: Id) -> Option<Box<dyn DebugVal>> {
        None
    }
}

impl_debug_val!(
    Range<i32>,
    "{:?}",
    |self| format!("{}..{}", self.start, self.end),
    |self, ui, metadata, _params| {
        if metadata.app_controlled {
            ui.add(Label::new(
                RichText::new(format!("{}..{}", self.start, self.end)).monospace(),
            ));
        } else {
            ui.horizontal(|ui| {
                ui.label("start:");
                ui.add(DragValue::new(&mut self.start).speed(0.1));
                ui.label("end:");
                ui.add(DragValue::new(&mut self.end).speed(0.1));
            });
        }
    }
);
