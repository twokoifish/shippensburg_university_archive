const { app, BrowserWindow, Tray, Menu } = require('electron')
const ApplicationTracker = require('./libs/modules/application/ApplicationTracker')
const path = require('path')
let tray = null

/**
 *
 * @param window
 */
function loadWindow(window) {
    const loadedWindow = new BrowserWindow({
        width: 800,
        height: 600,
        webPreferences: {
            preload: path.join(__dirname, '/libs/pages/' + window + "/" + window + "_preload.js")
        }
    })
    loadedWindow.loadFile(__dirname + '/libs/pages/' + window + "/" + window + ".html")
}

/**
 * Creates a tray icon.
 */
function createTray () {

    if (process.platform === "darwin") {
        tray = new Tray(__dirname + '/libs/assets/icon.png')
    } else {
        tray = new Tray(__dirname + '/libs/assets/icon.ico')
    }

    const contextMenu = Menu.buildFromTemplate([
        { label: "Start", click() { ApplicationTracker.start() } },
        { label: "Pause", click() { ApplicationTracker.pause() } },
        { label: "Stop", click() { ApplicationTracker.stop() } },
        { label: "Statistics", click() { loadWindow("statistics") } },
        { label: "Settings", click() { loadWindow("settings") } },
        { label: "Quit", click() { app.quit() } }
    ])
    tray.setToolTip("Time Tracker")
    tray.setContextMenu(contextMenu)

    tray.on('double-click', function() {
        if (BrowserWindow.getAllWindows().length === 0) {
            loadWindow("settings")
        }
    })
}

app.whenReady().then(() => {

    createTray()

    app.on('activate', function () {
        if (BrowserWindow.getAllWindows().length === 0) {
            loadWindow("index")
        }
    })
})

app.on('window-all-closed', function () {
    
})

