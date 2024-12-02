import {Command} from "../Command";
import  * as SettingsManager from "../../settingsManager/SettingsManager"

export class ResetCurrentCommand implements Command{
    execute() {
        SettingsManager.resetToCurrentDefaults()
        console.log("reset to current command done")
    }
}