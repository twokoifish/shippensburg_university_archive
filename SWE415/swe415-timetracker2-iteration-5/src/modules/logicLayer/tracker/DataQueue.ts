import {DesktopWindow} from "app-tracker";
import ApplicationRDG from "../../dal/gateways/application/ApplicationRDG";
import TimeRDG from "../../dal/gateways/time/TimeRDG";

export default class DataQueue {
  
  private static instance: DataQueue;
  private static interval: number;
  private static queue: DesktopWindow[][]
  private static isProcessing: boolean;
  private static foregroundWindow: DesktopWindow;
  private static hasActiveSystem: boolean;
  private static previousDataSet: DesktopWindow[];
  private static timeHandler: Map<number, number>;
  
  /**
   *
   * @private
   */
  private constructor() {
    DataQueue.queue = [];
    DataQueue.interval = 1000;
    DataQueue.isProcessing = false;
    DataQueue.hasActiveSystem = true;
    DataQueue.previousDataSet = [];
    DataQueue.timeHandler = new Map<number, number>();
  }
  
  /**
   * Get an instance of the data_queue.
   */
  public static getInstance(): DataQueue {
    if (!DataQueue.instance) {
      DataQueue.instance = new DataQueue();
    }
    return DataQueue.instance;
  }
  
  /**
   * Add data to the queue.
   * @param desktopWindows
   */
  public addData(desktopWindows: DesktopWindow[]) {
    if (typeof desktopWindows !== "undefined") {
      DataQueue.queue.push(desktopWindows);
      if (!DataQueue.isProcessing) {
        this.processData();
      }
    }
  }
  
  /**
   * Sieve out what items were added.
   */
  private getAddedApplications(data_set_a: DesktopWindow[], data_set_b: DesktopWindow[]): DesktopWindow[] {
    return data_set_a.filter((element) => {
      for (let i: number = 0; i < data_set_b.length; i++) {
        if (element.name === data_set_b[i].name && element.titleBar === data_set_b[i].titleBar && element.isFocus === data_set_b[i].isFocus) {
          return false;
        }
      }
      return true;
    });
  }
  
  /**
   * Sieve out what items were removed.
   */
  private getRemovedApplications(data_set_a: DesktopWindow[], data_set_b: DesktopWindow[]): DesktopWindow[] {
    return data_set_b.filter((element) => {
      for (let i: number = 0; i < data_set_a.length; i++) {
        if (element.name === data_set_a[i].name && element.titleBar === data_set_a[i].titleBar && element.isFocus === data_set_a[i].isFocus) {
          return false;
        }
      }
      return true;
    });
  }
  
  /**
   * Get the differences between to lists and return them in a set.
   * @param data_set_a the first set.
   * @param data_set_b the second set.
   */
  private getChangelog(data_set_a: DesktopWindow[], data_set_b: DesktopWindow[]): DesktopWindow[][] {
    let add_set: DesktopWindow[] = this.getAddedApplications(data_set_a, data_set_b);
    let remove_set: DesktopWindow[] = this.getRemovedApplications(data_set_a, data_set_b);
    return [add_set, remove_set];
  }
  
  /**
   * Wrapper for testing the changelog outside of the file.
   * @param data_set_a
   * @param data_set_b
   */
  public getChangelogWrapper(data_set_a: DesktopWindow[], data_set_b: DesktopWindow[]): DesktopWindow[][] {
    return this.getChangelog(data_set_a, data_set_b);
  }
  
  /**
   * Add items to the database.
   * @param desktop_windows the windows to add.
   * @private
   */
  private processAdditions(desktop_windows: DesktopWindow[]): void {
    desktop_windows.forEach((entry) => {
      let gateway: ApplicationRDG = new ApplicationRDG();
      let id: number = gateway.has(entry.name, entry.titleBar);
      if (id === -1) {
        gateway.create(entry);
        DataQueue.timeHandler.set(gateway.id, new Date().getTime());
      } else {
        gateway.read(id);
        if (!gateway.isBlacklisted) {
          console.log("I AM NOT BLACKLISTED -"+gateway.windowName);
          DataQueue.timeHandler.set(gateway.id, new Date().getTime());
        }
      }
    });
  }
  
  /**
   *
   * @param desktop_windows
   * @private
   */
  private processRemovals(desktop_windows: DesktopWindow[]): void {
    desktop_windows.forEach((entry) => {
      let gateway: ApplicationRDG = new ApplicationRDG();
      const id: number = gateway.has(entry.name, entry.titleBar);
      gateway.read(id);
      let start_time: number | undefined = DataQueue.timeHandler.get(id);
      console.log("Removing " + gateway.windowName);
      if (!gateway.isBlacklisted) {
        if (start_time !== undefined) {
          let time_gateway: TimeRDG = new TimeRDG();
          time_gateway.create(id, start_time, new Date().getTime(), !DataQueue.hasActiveSystem, entry.isFocus);
        } else {
          console.log("Failed to end time for " + entry.name + " with id of " + id);
        }
      }
    });
  }
  
  private focusChange(desktop_window: DesktopWindow): void {
    if (!DataQueue.foregroundWindow) {
      DataQueue.foregroundWindow = desktop_window;
    } else if (desktop_window.titleBar !== DataQueue.foregroundWindow.titleBar || desktop_window.name !== DataQueue.foregroundWindow.name) {
      // let gateway: ApplicationRDG = new ApplicationRDG();
      // const oldID: number = gateway.has(DataQueue.foregroundWindow.name, DataQueue.foregroundWindow.titleBar);
      // const newID: number = gateway.has(desktop_window.name, desktop_window.titleBar);
      //
      //
      //
      //
      //
      DataQueue.foregroundWindow = desktop_window;
    }
  }
  
  
  /**
   * Process the provided data in the queue.
   * @private
   */
  private processData(): void {
    let data_set: DesktopWindow[] | undefined = DataQueue.queue.pop();
    if (typeof data_set !== "undefined") {
      DataQueue.isProcessing = true;
      let result_set: DesktopWindow[][] = this.getChangelog(data_set, DataQueue.previousDataSet);
      if (result_set[1].length >= 1) {
        this.processRemovals(result_set[1]);
      }
      if (result_set[0].length >= 1) {
        this.processAdditions(result_set[0]);
      }
      let focus_set: DesktopWindow | undefined = data_set.filter((item) => item.isFocus).pop();
      if (typeof focus_set !== "undefined") {
        this.focusChange(focus_set);
      }
      DataQueue.previousDataSet = data_set;
      setTimeout(this.processData, DataQueue.interval);
    } else {
      DataQueue.isProcessing = false;
    }
  }
  
  /**
   * Return the foreground window. Will return a fake window if there is no foreground window.
   */
  public getForegroundWindow(): DesktopWindow {
    return (DataQueue.foregroundWindow)
      ? DataQueue.foregroundWindow
      : {
        titleBar: "None",
        name: "None",
        isFocus: false,
        id: 0,
        pid: 0,
        preventsSystemSleep: false,
        preventsScreenSleep: false
      };
  }
  
  /**
   * Toggle the activity status of the system for the processor.
   */
  public toggleSystemActivity(): void {
    DataQueue.hasActiveSystem = !DataQueue.hasActiveSystem
    DataQueue.timeHandler.forEach((value, key) => {
      let gateway: ApplicationRDG = new ApplicationRDG();
      gateway.read(key);
        let time_gateway: TimeRDG = new TimeRDG();
        if (gateway.windowName === DataQueue.foregroundWindow.titleBar && gateway.windowClass === DataQueue.foregroundWindow.name) {
          time_gateway.create(key, value, new Date().getTime(), DataQueue.hasActiveSystem, true);
        } else {
          time_gateway.create(key, value, new Date().getTime(), DataQueue.hasActiveSystem, false);
        }
        DataQueue.timeHandler.set(key, new Date().getTime());
    });
  }
  
  public started(): void {
    DataQueue.timeHandler.forEach((value, key) => {
      DataQueue.timeHandler.set(key, new Date().getTime());
    });
  }
  
  public stopped(): void {
    DataQueue.timeHandler.forEach((value, key) => {
      let gateway: ApplicationRDG = new ApplicationRDG();
      gateway.read(key);
      let time_gateway: TimeRDG = new TimeRDG();
      console.log("STOPPING " + gateway.windowName + " " + gateway.windowClass);
      if(!gateway.isBlacklisted){
        if (gateway.windowName === DataQueue.foregroundWindow.titleBar && gateway.windowClass === DataQueue.foregroundWindow.name) {
          time_gateway.create(key, value, new Date().getTime(), !DataQueue.hasActiveSystem, true);
        } else {
          time_gateway.create(key, value, new Date().getTime(), !DataQueue.hasActiveSystem, false);
        }
      }
    });
  }
  
  public getSystemActivity(): boolean {
    return DataQueue.hasActiveSystem;
  }
  
}