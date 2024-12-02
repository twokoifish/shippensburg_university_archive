import {Application} from "../mapper/Application";


import {AppGetAllCommand} from "../commands/application/AppGetAllCommand";
import {AppToggleTrackedCommand} from "../commands/application/AppToggleTrackedCommand";
import {AppGetWithTimeCommand} from "../commands/application/AppGetWithTimeCommand";
import {TimeDeleteByIDCommand} from "../commands/application/TimeDeleteByIDCommand";
import GetForegroundWindowCommand from "../commands/data_queue/GetForegroundWindowCommand";
import {AppWasModifiedCommand} from "../commands/application/AppWasModifiedCommand";
import {ResetDefaultCommand} from "../commands/settings/ResetDefaultCommand"
import {ResetCurrentCommand}  from "../commands/settings/ResetCurrentCommand"
import {SaveCurrentCommand}  from "../commands/settings/SaveCurrentCommand"
const { ipcMain } = require('electron')


ipcMain.on('AppsGetAll_Request', async(event, arg) => {

    try{
        let cmd = new AppGetAllCommand()

        const appsList = await cmd.execute()
        event.reply('AppsGetAll_Response', appsList)
    }catch (e) {
        console.log("couldn't get all apps with time " + e)
    }

})

ipcMain.on('AppToggleTracked_Request', async(event, id)=>{

    try{
        let cmd = new AppToggleTrackedCommand(id)
        await cmd.execute()
        event.reply('AppToggleTracked_Response', "complete")
    }catch (e) {
        console.log("couldn't get all apps with time " + e)
    }

})

ipcMain.on('AppsGetAllWithTime_Request', async(event, arg)=>{

    try{
        let cmd = new AppGetWithTimeCommand()
        // let appsList: Application[] = await cmd.execute()
        // console.log(appsList)
        // appsList.forEach((app)=>{
        //     console.log(app.times)
        // })
        let a = await cmd.execute()
        event.reply('AppsGetAllWithTime_Response', a)
    }catch (e) {
        console.log("couldn't get all apps with time " + e)

    }

})

ipcMain.on('LastActiveApp_Request', async(event, arg) => {
    console.log(arg)
    try {
        let cmd = new GetForegroundWindowCommand()
        let app = await cmd.execute()
        event.reply('LastActiveApp_Response', app);
    } catch (e) {
        console.log("Failed to retrieve last active application : " + e)
    }
})

ipcMain.on('AppWasModified_Request', async(event, appID) => {

    try {
        let cmd = new AppWasModifiedCommand(appID)
        cmd.execute()

    } catch (e) {
        console.log("Failed to retrieve last active application : " + e)
    }
})

ipcMain.on('TimeDeleteByID_Request', async(event, id) => {

    try{
        let cmd = new TimeDeleteByIDCommand(id)
        let app = await cmd.execute()
        event.reply('TimeDeleteByID_Response', "success")
    } catch (e) {
        console.log("Failed to delete times : " + e)
    }
})

ipcMain.on('ResetDefaultSettings_Request', async(event,arg) => {
    let cmd = new ResetDefaultCommand()
    cmd.execute()
    event.reply('ResetDefaultCommand_Response', "complete")
})

ipcMain.on('ResetCurrentSettings_Request', async(event,arg) => {
    let cmd = new ResetCurrentCommand()
    cmd.execute()
    event.reply('ResetCurrentCommand_Response', "complete")
})

ipcMain.on('SaveCurrentSettings_Request', async(event,arg) => {
    let cmd = new SaveCurrentCommand()
    cmd.execute()
    event.reply('SaveCurrentCommand_Response', "complete")
})




