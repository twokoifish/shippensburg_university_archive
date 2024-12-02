import { BrowserWindow, Tray, Menu, powerMonitor } from 'electron';
import SchemaBuilder from "../src/modules/dal/database/schemaBuilder/SchemaBuilder"
import {TrackerStartCommand} from "../src/modules/logicLayer/commands/tracker/TrackerStartCommand"
import {TrackerPauseCommand} from "../src/modules/logicLayer/commands/tracker/TrackerPauseCommand"
import {TrackerStopCommand} from "../src/modules/logicLayer/commands/tracker/TrackerStopCommand"
import {AppQuitCommand} from "../src/modules/logicLayer/commands/main/AppQuitCommand"
import SystemInactiveCommand from "../src/modules/logicLayer/commands/data_queue/SystemInactiveCommand";
import SystemActiveCommand from "../src/modules/logicLayer/commands/data_queue/SystemActiveCommand";
require("../src/modules/logicLayer/ipcInterface/ipcHandler.js")
export default class Main {
    static mainWindow: Electron.BrowserWindow | null;
    static application: Electron.App;
    static BrowserWindow: any;
    static tray: Tray;

    /**
     * If all windows are closed, then say they are closed.
     * @private
     */
    private static onWindowAllClosed() {
        console.log("All windows closed.");
    }

    /**
     * When the window is closed, set it to null.
     * @private
     */
    private static onClose() {
        Main.mainWindow = null;
    }

    /**
     *
     * @param url
     * @private
     */
    private static changeTo(url: String) {
        if (Main.mainWindow !== null) {
            Main.mainWindow.loadURL("http://localhost:3000" + url)
                .then(() => console.log("Successfully created window."))
                .catch(() => console.log("Failed to create window."))
        } else {
            throw new Error("Main window cannot be null when setting its address.");
        }
    }

    /**
     *
     * @private
     */
    private static async onReady() {
        Main.mainWindow = new Main.BrowserWindow({
            resizable: false,
            width: 1000,
            height: 600,
            webPreferences: {
                nodeIntegration: true,
                contextIsolation: false,
                nodeIntegrationInWorker: true
            }
        });
        if (Main.mainWindow !== null) {
            await Main.mainWindow.on('closed', Main.onClose)
            Main.mainWindow.loadURL("http://localhost:3000/")
                .then(() => console.log("Successfully created window."))
                .catch(() => console.log("Failed to create window."))
        } else {
            throw new Error("Main window cannot be null when ready.");
        }
    }

    private static makeTray() {
        setInterval(() => {
            let state = powerMonitor.getSystemIdleState(60)
            if (state === "idle" || state === "locked") {
                let cmd = new SystemInactiveCommand()
                cmd.execute()
            } else if (state === "active") {
                let cmd = new SystemActiveCommand()
                cmd.execute()
            } else {
                console.log`State unknown... ignoring.`
            }
        }, 1000)

        const schema : SchemaBuilder = SchemaBuilder.getInstance();
        schema.buildUp()
        console.log("app is running")
        if (process.platform === "darwin") {
            Main.tray = new Tray(__dirname + '/macOS.png')
        } else {
            Main.tray = new Tray(__dirname + '/windows.ico')
        }

        let contextMenu = Menu.buildFromTemplate([
            { label: "Start", click() { let cmd = new TrackerStartCommand()
                    cmd.execute() } },
            { label: "Pause", click() { let cmd = new TrackerPauseCommand()
                    cmd.execute() } },
            { label: "Stop", click() { let cmd = new TrackerStopCommand()
                    cmd.execute() } },
            { label: "Statistics", click() { Main.changeTo("/statistics") } },
            { label: "Settings", click() { Main.changeTo("/settings") } },
            { label: "Quit", click() { let cmd = new AppQuitCommand()
                    cmd.execute() } }
        ])

        Main.tray.setToolTip("Time Tracker")
        Main.tray.setContextMenu(contextMenu)
        Main.tray.setTitle("Time Tracker")
        Main.tray.on('double-click', ()=> {
            Main.changeTo("/")
        })
    }

    static async main(app: Electron.App, browserWindow: typeof BrowserWindow) {
        Main.BrowserWindow = browserWindow;
        Main.application = app;
        Main.application.on('window-all-closed', Main.onWindowAllClosed);
        Main.application.on('ready', () => {
            Main.onReady().then(Main.makeTray)
        })
    }
    
}

