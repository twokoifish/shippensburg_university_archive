import {Command} from "../Command";

import ApplicationTracker from "../../tracker/ApplicationTracker";

export class TrackerStartCommand implements Command{
  execute() {
    let t = ApplicationTracker.getInstance()
    t.startTracking()
    
  }
}
