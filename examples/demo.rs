use eframe::egui::{self, Ui};
use egui_demo_lib::DemoWindows;
use egui_dev_tools::{Config, WidgetInspect};

fn main() -> eframe::Result {
    env_logger::init();

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([1280.0, 800.0]),
        ..Default::default()
    };

    eframe::run_native(
        "egui_dev_tools Demo",
        options,
        Box::new(|cc| {
            // Add the widget inspector plugin
            let plugin = WidgetInspect::new(Config::new(None));
            cc.egui_ctx.add_plugin(plugin);

            Ok(Box::new(DemoApp::default()))
        }),
    )
}

#[derive(Default)]
struct DemoApp {
    demo_windows: DemoWindows,
}

impl eframe::App for DemoApp {
    fn ui(&mut self, ui: &mut Ui, _frame: &mut eframe::Frame) {
        self.demo_windows.ui(ui);
    }
}
