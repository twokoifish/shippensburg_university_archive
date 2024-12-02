'use strict';
import ApplicationTDG from "../../dal/gateways/application/ApplicationTDG"
import ApplicationRDG from "../../dal/gateways/application/ApplicationRDG"
import ApplicationDTO from "../../dal/gateways/application/ApplicationDTO"
const Application = require("../mapper/Application")


const fs = require('fs');
let isGenerated: boolean = false;
//create a default-settings.json and current-settings.json file and writes to it
export function generateDefaults() {
    isGenerated = true
    let apps: Array<App> = [];
    apps.push(new App('general - Discord', 'Discord.exe', false))
    apps.push(new App('Time Tracker', 'electron.exe', true))

    // create default Settings
    fs.writeFile("default-settings.json", JSON.stringify(apps, null, 2), (err: any) => {
        if (err) throw err;
    });

    // create current Settings (starts out the same as default settings)
    fs.writeFile("current-settings.json", JSON.stringify(apps, null, 2), (err: any) => {
        if (err) throw err;
    });

}

// read from a .json file and change the isBlacklisted value accordingly
export function resetDefaults() {
    let apps : Array<any> = [];
    if(!isGenerated) {
        generateDefaults()
    }
    fs.readFile("default-settings.json", (err: any, data: any)=> {
        if (err) throw err;
        apps = JSON.parse(data);

        apps.forEach(function (value) {
            let app:ApplicationRDG = new ApplicationRDG()
            let id = app.has(value.class, value.name)
            if ((id !== undefined) && (id >= 0)) {
                app.read(id)
                app.isBlacklisted = value.isBlacklisted
                app.update()
            }
            else {
                console.log(value.name + " is not in database")
            }
        });
    });


}


//reads the current settings form the database
//and saves them to a file
export function saveCurrentDefaults() {
    let currentApps : Array<App> = [];
    let DTOApps : Array<ApplicationDTO> = [];
    let Table:ApplicationTDG = new ApplicationTDG()

    DTOApps = Table.getAll()
    DTOApps.forEach(function (value) {
        currentApps.push(new App(value.windowName, value.windowClass, value.isBlacklisted))
    });
    // rewrite current settings
    fs.writeFile("current-settings.json", JSON.stringify(currentApps, null, 2), (err: any) => {
        if (err) throw err;
    });
}

// function that will reset to the current settings
export function resetToCurrentDefaults() {
    let appList : Array<any> = [];

    fs.readFile("current-settings.json", (err: any, data: any)=> {
        if (err) throw err;
        appList = JSON.parse(data);

        appList.forEach(function (value) {
            let currentApp:ApplicationRDG = new ApplicationRDG()
            let id = currentApp.has(value.class, value.name)
            if ((id !== undefined) && (id >= 0)) {
                currentApp.read(id)
                currentApp.isBlacklisted = value.isBlacklisted
                currentApp.update()
            }
            else {
                console.log(value.name + " is not in database")
            }
        });
    });

}

class App {
    name: string
    class: string
    isBlacklisted: boolean

    constructor(n: string, c: string, i: boolean) {
        this.name = n;
        this.class = c;
        this.isBlacklisted = i;
    }
}
