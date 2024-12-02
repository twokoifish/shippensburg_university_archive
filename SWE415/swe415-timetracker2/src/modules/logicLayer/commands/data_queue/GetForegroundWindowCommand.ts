import {Command} from "../Command";
import {DesktopWindow} from "app-tracker";
import DataQueue from "../../tracker/DataQueue";

/**
 * Create a new command that implements the Command interface.
 */
export default class GetForegroundWindowCommand implements Command {
  
  /**
   * Return the foreground window.
   */
  execute(): string {
    let data_queue: DataQueue = DataQueue.getInstance();
    return data_queue.getForegroundWindow().name;
  }
}