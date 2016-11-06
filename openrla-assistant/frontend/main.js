const { app, BrowserWindow } = require('electron');

let win;

const debug = false;

function createSplashWindow() {
  const splashWin = new BrowserWindow({
    width: 640,
    height: 480,
    frame: false,
    modal: true,
    parent: 'top',
    resizable: false,
  });
  splashWin.loadURL(`file://${__dirname}/view/splash.html`);
  splashWin.on('close', createWindow);
}

function createWindow () {
  win = new BrowserWindow({width: 800, height: 600});
  win.loadURL(`file://${__dirname}/view/index.html`);

  if (debug) {
    win.webContents.openDevTools();
  }

  win.on('closed', () => {
    win = null;
  });
}

app.on('ready', createSplashWindow);

app.on('window-all-closed', app.quit);
