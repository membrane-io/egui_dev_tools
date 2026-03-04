use eframe::egui::{self, Ui, Vec2};
use egui_dev_tools::{DebugValsPlugin, ValsEditor, val, val_mut};

fn main() -> eframe::Result {
  env_logger::init();

  let options = eframe::NativeOptions {
    viewport: egui::ViewportBuilder::default().with_inner_size([1280.0, 800.0]),
    ..Default::default()
  };

  eframe::run_native(
    "Debug Values Demo",
    options,
    Box::new(|cc| {
      cc.egui_ctx.add_plugin(DebugValsPlugin::new());
      Ok(Box::new(DemoApp::default()))
    }),
  )
}

#[derive(Default)]
struct DemoApp {
  counter: i32,
}

impl eframe::App for DemoApp {
  fn ui(&mut self, ui: &mut Ui, _frame: &mut eframe::Frame) {
    egui::Window::new("Demo").show(ui.ctx(), |ui| {
      self.demo_ui(ui);
    });

    egui::Window::new("Debug Values").show(ui.ctx(), |ui| {
      ui.add(ValsEditor::new());
    });
  }
}

impl DemoApp {
  fn demo_ui(&mut self, ui: &mut Ui) {
    ui.heading("Debug Values Demo");
    ui.separator();

    ui.label("This demonstrates the val! macro for debug values.");
    ui.label("Edit values in the 'Debug Values' window and see them persist!");
    ui.separator();

    // Use various debug values with auto-generated keys
    let enabled = val!(bool);
    let mut speed = val_mut!(f32);
    let mut count = val_mut!(i32);
    let mut offset = val_mut!(Vec2);

    // Use debug values with custom keys
    let player_speed = val!(f32, "player_speed");
    let show_ui = val!(bool, "show_ui");

    ui.label("Values with auto-generated keys:");
    ui.label(format!("Enabled: {}", enabled));
    ui.label(format!("Speed: {}", *speed));
    ui.label(format!("Count: {}", *count));
    ui.label(format!("Offset: {:?}", *offset));

    ui.separator();

    ui.label("Values with custom keys:");
    ui.label(format!("Player Speed: {}", player_speed));
    ui.label(format!("Show UI: {}", show_ui));

    ui.separator();

    // Demonstrate that values can be modified
    if ui.button("Increment counter").clicked() {
      self.counter += 1;
      *count = self.counter;
    }

    if enabled && show_ui {
      ui.colored_label(egui::Color32::GREEN, "Both debug flags are enabled!");
    }

    // Demonstrate reading and writing
    if ui.button("Double speed").clicked() {
      *speed *= 2.0;
    }

    if ui.button("Reset offset").clicked() {
      *offset = Vec2::ZERO;
    }
  }
}
