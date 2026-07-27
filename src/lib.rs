mod symbol_parser;
pub mod vals;
mod widget_inspect;

// Resolve the picker's callstacks against the running module's own DWARF, and read each
// frame's locals live out of the shadow stack. wasm-only: a native backtrace's addresses
// aren't module offsets into any DWARF we hold, and native has no shadow stack to walk.
#[cfg(all(feature = "dwarf", target_arch = "wasm32"))]
pub mod dwarf;

#[cfg(feature = "logging")]
pub mod logging;

use std::sync::Arc;

use egui::Id;
#[cfg(feature = "vals")]
pub use vals::{DebugValsPlugin, DebugValues, ValGroup, ValParams};
pub type ValsEditor = DebugValues;

/// Dummy macros for vals that returns a default value
#[cfg(not(feature = "vals"))]
mod dummies {
    #[macro_export]
    macro_rules! val {
        ($ty:ty) => {
            $ty::default()
        };
        ($ty:ty, $key:expr, default = $default:expr $(, $param:ident = $value:expr)* $(,)?) => {
            ::std::convert::Into::<$ty>::into($default)
        };
        ($ty:ty, $key:expr, $($param:ident = $value:expr),+ $(,)?) => {
            $ty::default()
        };
        ($ty:ty, $key:expr) => {
            $ty::default()
        };
        ($ty:ty, $key1:expr, $($rest:expr),+ $(,)?) => {
            $ty::default()
        };
    }

    #[macro_export]
    macro_rules! val_handle {
        ($ty:ty) => {
            DummyValHandle::<$ty>::default()
        };
        ($ty:ty, $key:expr, default = $default:expr $(, $param:ident = $value:expr)* $(,)?) => {
            DummyValHandle::<$ty>::new($default)
        };
        ($ty:ty, $key:expr, $($param:ident = $value:expr),+ $(,)?) => {
            DummyValHandle::<$ty>::new($ty::default())
        };
        ($ty:ty, $key:expr) => {
            DummyValHandle::<$ty>::new($ty::default())
        };
        ($ty:ty, $key1:expr, $($rest:expr),+ $(,)?) => {
            $ty::default()
        };
    }

    pub struct DummyValHandle<T: Copy + Default>(T);

    impl Deref for DummyValHandle<T> {
        type Target = T;
        fn deref(&self) -> &Self::Target {
            &self.value
        }
    }
    impl DerefMut for DummyValHandle<T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.value
        }
    }

    #[cfg(not(feature = "vals"))]
    #[macro_export]
    macro_rules! val_mut {
        ($ty:ty) => {
            ValHandle::<$ty>::new($ty::default())
        };
        ($ty:ty, $key:expr, default = $default:expr $(, $param:ident = $value:expr)* $(,)?) => {
            ValHandle::<$ty>::new(::std::convert::Into::<$ty>::into($default))
        };
        ($ty:ty, $key:expr, $($param:ident = $value:expr),+ $(,)?) => {
            ValHandle::<$ty>::new($ty::default())
        };
        ($ty:ty, $key:expr) => {
            ValHandle::<$ty>::new($ty::default())
        };
        ($ty:ty, $key1:expr, $($rest:expr),+ $(,)?) => {
            ValHandle::<$ty>::new($ty::default())
        };
    }
}

#[cfg(feature = "widget-inspect")]
pub use widget_inspect::{Config, WidgetInspect};

/// Run `f` through `subsecond::call` so hot-patched code inside it takes effect.
///
/// egui invokes plugin hooks through a `dyn Plugin` vtable built at registration time, so a patch
/// boundary placed by the host *around* the dispatch can't redirect into patched plugin code: the
/// vtable still points at the original method addresses. Plugins whose hook logic should hot-reload
/// must instead place the `subsecond::call` boundary *inside* their own hook body (i.e. their own
/// crate), which is what this helper does. In release builds `subsecond::call` is a direct call, so
/// this has no overhead.
#[inline]
pub fn hot_call<R>(f: impl FnMut() -> R) -> R {
    subsecond::call(f)
}

/// An app-specific function that opens a file at a given source location.
pub type FileOpener =
    Box<dyn Fn(&egui::Context, &SourceLocation) -> Result<(), String> + Send + Sync>;

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub path: String,
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    pub fn filename(&self) -> &str {
        self.path.rsplit('/').next().unwrap_or(self.path.as_str())
    }
}

fn file_opener_key() -> Id {
    Id::new("egui_dev_tools::file_opener")
}

/// Sets a function that will be called when egui_dev_tools wants to open a source file.
///
/// Note on `source.path`: it is not always an absolute filesystem path, and the convention differs
/// by source and target:
///  - Native callstack locations (e.g. the widget inspector) come from debuginfo and are absolute.
///  - Wasm callstack locations are parsed from a JS stack trace: they are full paths but may be
///    missing the leading `/`.
///  - `file!()` locations (e.g. log events surfaced by the logs viewer) are whatever path rustc was
///    invoked with — typically relative to the crate's workspace root.
///
/// An opener should normalize all of these to an absolute path before handing them to an editor.
/// The example below uses the workspace root (`CARGO_MANIFEST_DIR`) to disambiguate a full path
/// that is merely missing its leading `/` from a path that is relative to the workspace root.
///
/// Example for web that opens the file in cursor:
/// ```ignore
/// Some(Box::new(|ctx, source| {
///   let manifest_dir = env!("CARGO_MANIFEST_DIR");
///   let abs_path = if source.path.starts_with('/') {
///     source.path.clone()
///   } else if format!("/{}", source.path).starts_with(manifest_dir) {
///     format!("/{}", source.path)
///   } else {
///     format!("{manifest_dir}/{}", source.path)
///   };
///   ctx.open_url(egui::OpenUrl::same_tab(format!("cursor://file{abs_path}:{}", source.line)));
///   Ok(())
/// }))
/// ```
pub fn set_file_opener(ctx: &egui::Context, file_opener: FileOpener) {
    ctx.data_mut(|data| data.insert_temp::<Arc<FileOpener>>(file_opener_key(), file_opener.into()));
}

pub(crate) fn open_file(ctx: &egui::Context, location: &SourceLocation) {
    let Some(file_opener) = ctx.data(|data| data.get_temp::<Arc<FileOpener>>(file_opener_key()))
    else {
        log::error!(
            "No file opener set. Did you forget to call egui_dev_tools::set_file_opener? Ignoring {}",
            location.path
        );
        return;
    };
    if let Err(err) = file_opener(ctx, location) {
        log::error!("Failed to open file at {:?}: {:?}", location, err);
    }
}
