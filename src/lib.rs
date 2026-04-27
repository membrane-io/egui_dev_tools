mod symbol_parser;
pub mod vals;
mod widget_inspect;

#[cfg(feature = "logging")]
pub mod logging;

use std::sync::Arc;

use egui::Id;
#[cfg(feature = "vals")]
pub use vals::{DebugValsPlugin, DebugValues, ValParams};
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

/// An app-specific function that opens a file at a given source location.
pub type FileOpener = Box<dyn Fn(&egui::Context, &SourceLocation) -> Result<(), String> + Send + Sync>;

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
/// Example for web that opens the file in cursor:
/// ```
/// Some(Box::new(|ctx, source| {
///   ctx.open_url(egui::OpenUrl::same_tab(format!("cursor://file{}:{}", source.path, source.line)));
///   Ok(())
/// }))
/// ```
pub fn set_file_opener(ctx: &egui::Context, file_opener: FileOpener) {
  ctx.data_mut(|data| data.insert_temp::<Arc<FileOpener>>(file_opener_key(), file_opener.into()));
}

pub(crate) fn open_file(ctx: &egui::Context, location: &SourceLocation) {
  let Some(file_opener) = ctx.data(|data| data.get_temp::<Arc<FileOpener>>(file_opener_key())) else {
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
