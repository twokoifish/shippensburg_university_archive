import {Command} from "../Command";
import  * as SettingsManager from "../../settingsManager/SettingsManager"

export class ResetDefaultCommand implements Command{
    execute() {
        SettingsManager.resetDefaults()
        console.log("reset to default command done")
    }
}