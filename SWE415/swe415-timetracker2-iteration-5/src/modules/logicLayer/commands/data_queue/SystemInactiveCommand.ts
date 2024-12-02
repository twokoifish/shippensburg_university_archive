import {Command} from "../Command";
import DataQueue from "../../tracker/DataQueue";

export default class SystemInactiveCommand implements Command {
  
  execute(): void {
    let data_queue: DataQueue = DataQueue.getInstance();
    if (data_queue.getSystemActivity()) {
      data_queue.toggleSystemActivity();
    }
  }
  
}

