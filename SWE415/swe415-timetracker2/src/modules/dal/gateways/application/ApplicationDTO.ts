'use strict';

export default class ApplicationDTO {
  id : number
  windowClass : string
  windowName : string
  isBlacklisted : boolean
  isSettingsModified : boolean

  /**
   * Create an application data transfer object.
   *
   * @param id int, id number, refers to entry in applications table in database.
   * @param windowClass string, class of window, ie, msedge.exe.
   * @param windowName string, name of windowm,ie, google.
   * @param isBlacklisted boolean, whether or not the application is being tracked.
   * @param isSettingsModified boolean, whether or not the settings have been modified
   */
  constructor(id: number, windowClass: string , windowName: string , isBlacklisted: boolean, isSettingsModified : boolean) {
    this.id = id;
    this.windowClass = windowClass;
    this.windowName = windowName;
    this.isBlacklisted = isBlacklisted;
    this.isSettingsModified = isSettingsModified;
  }
}