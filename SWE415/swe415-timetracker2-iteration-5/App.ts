/**
 * Imports for the class.
 */
import {app, BrowserWindow } from "electron"; // import the handles for app and BrowserWindow.
import Main from './public/Main'; // import the Main class defined in "Main.ts".

/**
 * Create a new instance of main using the app handle and BrowserWindow.
 */
Main.main(app, BrowserWindow).then(r => console.log("DONE"));
