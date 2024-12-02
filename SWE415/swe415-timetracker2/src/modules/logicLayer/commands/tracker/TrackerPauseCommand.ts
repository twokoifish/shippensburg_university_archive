import {Command} from "../Command";

import ApplicationTracker from "../../tracker/ApplicationTracker";

export class TrackerPauseCommand implements Command{
  execute() {
    let t = ApplicationTracker.getInstance()
    t.pauseTracking()
  }
}
