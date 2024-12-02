import {Command} from "../Command";

const Tracker = require("../../tracker/Tracker")

export class TrackerMakeIdleCommand implements Command{
  execute() {
    let t = Tracker.getInstance()
    t.makeIdle()
  }
}

