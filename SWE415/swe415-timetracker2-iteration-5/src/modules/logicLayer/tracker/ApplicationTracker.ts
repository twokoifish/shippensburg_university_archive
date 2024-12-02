import app_tracker from "app-tracker";
import DataQueue from "./DataQueue";
import { States } from "./States";

/**
 * Tracks active windows from the system.
 */
export default class ApplicationTracker {
  
  private static instance: ApplicationTracker;
  private static interval_ms: number;
  private static state: States;
  private static data_queue: DataQueue;
  
  /**
   * Initializes information for the class.
   * @private
   */
  private constructor() {
    ApplicationTracker.data_queue = DataQueue.getInstance();
    ApplicationTracker.interval_ms = 1000;
    ApplicationTracker.state = States.INACTIVE;
  }
  
  /**
   * Get the singleton instance of the Tracker.
   */
  public static getInstance(): ApplicationTracker {
    if (!ApplicationTracker.instance) {
      ApplicationTracker.instance = new ApplicationTracker();
    }
    return ApplicationTracker.instance;
  }
  
  /**
   * Get the windows from the system and process them.
   * @private
   */
  private static trackWindows(): void {
    switch (ApplicationTracker.state) {
      case States.ACTIVE:
        app_tracker.fetchDesktopWindows()
          .then((desktopWindows) => {
            ApplicationTracker.data_queue.addData(desktopWindows);
          })
          .catch((error) => {
            console.error(error);
          })
          .finally(() => {
            console.log("Tracker cycle complete.");
            setTimeout(ApplicationTracker.trackWindows, ApplicationTracker.interval_ms);
          });
        break;
      default:
        break;
    }
  }
  
  /**
   * Change the tracker to the start state.
   * @return boolean
   */
  public startTracking(): boolean {
    switch (ApplicationTracker.state) {
      case States.ACTIVE:
        return false;
      default:
        ApplicationTracker.state = States.ACTIVE;
        ApplicationTracker.data_queue.started();
        setTimeout(ApplicationTracker.trackWindows, ApplicationTracker.interval_ms);
        return true;
    }
  }
  
  /**
   * Change the tracker to the paused state.
   * @return boolean
   */
  public pauseTracking(): boolean {
    switch (ApplicationTracker.state) {
      case States.ACTIVE:
        ApplicationTracker.state = States.INACTIVE;
        ApplicationTracker.data_queue.stopped()
        return true;
      default:
        return false;
    }
  }
  
  /**
   * Change the tracker to the stopped state.
   * @return boolean
   */
  public stopTracking(): boolean {
    switch (ApplicationTracker.state) {
      case States.INACTIVE:
        return false;
      default:
        ApplicationTracker.state = States.INACTIVE;
        ApplicationTracker.data_queue.stopped()
        return true;
    }
  }
  
  /**
   * Change the interval for the tracker.
   * @param interval_ms
   */
  public changeInterval(interval_ms: number): boolean {
    if (interval_ms > 0) {
      ApplicationTracker.interval_ms = interval_ms;
      return true;
    } else {
      return false;
    }
  }
  
  /**
   * Get the current state of the tracker.
   */
  public getState(): States {
    return ApplicationTracker.state;
  }
  
}