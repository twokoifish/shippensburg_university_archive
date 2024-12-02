import {Command} from "../Command";
import {TrackerStopCommand} from "../tracker/TrackerStopCommand";
const { app } = require('electron')

export class AppQuitCommand implements Command{
  execute() {
    let cmd = new TrackerStopCommand()
    cmd.execute()
    app.quit()
  }
}
