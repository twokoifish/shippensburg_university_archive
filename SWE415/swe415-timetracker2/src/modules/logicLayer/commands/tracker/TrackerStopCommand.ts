import {Command} from "../Command";

import ApplicationTracker from "../../tracker/ApplicationTracker";

export class TrackerStopCommand implements Command{
  execute() {
    let t = ApplicationTracker.getInstance()
    t.stopTracking()
  }
}
