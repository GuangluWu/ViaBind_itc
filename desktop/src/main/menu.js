const { Menu, dialog } = require("electron");

function setupMenu(context) {
  const template = [
    {
      label: context.appName,
      submenu: [
        {
          label: "About",
          click: () => {
            dialog.showMessageBox({
              type: "info",
              title: `About ${context.appName}`,
              message: `${context.appName} Desktop`,
              detail: `${context.appSlogan}\n\nDeveloper: ${context.developerName}\nEmail: ${context.developerEmail}\nWebsite: ${context.developerSite}\nVersion: ${context.versionSignature}\n\nElectron shell with local Shiny backend.`
            });
          }
        },
        { type: "separator" },
        {
          label: "Restart Backend",
          click: async () => {
            if (context.restartBackend) {
              await context.restartBackend();
            }
          }
        },
        {
          label: "Open Logs Directory",
          click: async () => {
            if (context.openLogsDir) {
              await context.openLogsDir();
            }
          }
        },
        {
          label: "Export Diagnostics Package...",
          click: async () => {
            if (context.exportDiagnostics) {
              await context.exportDiagnostics();
            }
          }
        },
        { type: "separator" },
        { role: "quit" }
      ]
    },
    {
      label: "View",
      submenu: [
        { role: "reload" },
        { role: "toggledevtools" },
        { role: "togglefullscreen" },
        { role: "resetzoom" },
        { role: "zoomin" },
        { role: "zoomout" }
      ]
    }
  ];

  Menu.setApplicationMenu(Menu.buildFromTemplate(template));
}

module.exports = { setupMenu };
