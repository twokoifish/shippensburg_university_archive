import {Command} from "../Command";
import  * as SettingsManager from "../../settingsManager/SettingsManager"

export class SaveCurrentCommand implements Command {
    execute() {
        SettingsManager.saveCurrentDefaults()
        console.log("save current command done")
    }
}
