// Imports
import ApplicationDTO from "./ApplicationDTO";
import SchemaBuilder from "../../database/schemaBuilder/SchemaBuilder";
import DatabaseManager from "../../database/manager/DatabaseManager";
import ApplicationMap from "../../../logicLayer/identityMap/ApplicationMap";
import {DesktopWindow} from "app-tracker";
import SQLite3 = require('better-sqlite3');
// This require() might be problematic
// const ApplicationMap = require("../../../logicLayer/identityMap/ApplicationMap");

// Get singletons
const schema : SchemaBuilder = SchemaBuilder.getInstance();
const dbm : DatabaseManager = DatabaseManager.getInstance();

/**
 * An ApplicationRow from database.
 */
interface ApplicationRow extends SQLite3.RunResult {
  id: number
  class: string
  name: string
  isBlacklisted: number
  isSettingsModified : number
}

export default class ApplicationRDG {
  id: number = 0
  windowClass: string = ""
  windowName: string = ""
  isBlacklisted: boolean = false;
  isSettingsModified: boolean = false;

  constructor(id ?: number) {
    if(id !== undefined)
      this.read(id)
  }

  /**
   * Generate an id given a DesktopWindow's title bar and name values.
   * @param promise DesktopWindow
   * @return number id
   */
  public getHash(promise : DesktopWindow): number {
    let hash : number = 0;
    for(let x = 0; x < promise.titleBar.length; x++) {
      let n = promise.titleBar.charCodeAt(x);
      hash = ((hash<<5)-hash)+n;
      hash = hash & hash;
      hash /= promise.titleBar.length;
    }
    for(let x = 0; x < promise.name.length; x++) {
      let n = promise.name.charCodeAt(x);
      hash = ((hash<<5)-hash)+n;
      hash = hash & hash;
      hash /= promise.name.length;
    }
    hash = Math.abs(Math.ceil(hash / (promise.name.length * promise.titleBar.length * 100)));
    hash +=( 2 * (promise.id + promise.pid));
    return hash;
  }

  /**
   * Insert an application into the database.
   * @param dw DesktopWindow
   */
  public create(dw : DesktopWindow): void {
    try {
      // Get connection, set up statement, run it
      let connection: SQLite3.Database = dbm.getConnection(); // Get connection to DB

      // SQLite doesn't do booleans, so convert each boolean to a number
      let insertApplication: string = // SQL Statement
          "INSERT INTO applications(id, class, name, isBlacklisted, isSettingsModified) VALUES (?, ?, ?, ?, ?);";
      let stmt: SQLite3.Statement = connection.prepare(insertApplication); // Prepare

      const out : ApplicationRow = stmt.run(this.getHash(dw), dw.name, dw.titleBar, 1, 0) as ApplicationRow; // Execute
      dbm.closeConnection(connection); // Close connection, don't need it anymore

      // Set local variables to reflect what we found
      this.id = this.getHash(dw);
      this.windowClass = dw.name;
      this.windowName = dw.titleBar;
      this.isBlacklisted = false;
      this.isSettingsModified = false;

      ApplicationMap.create(this.getDTO())

    } catch (e) {
      throw e // error is thrown when stmt.run fails
    }
  }

  /**
   * Read an application from the database.
   * @param id number of application to search the database for.
   */
  public read(id: number): void {
    try {
      // Get connection, set up statement, and run it.
      let connection: SQLite3.Database = dbm.getConnection();
      const readSQL: string = "SELECT * FROM applications WHERE id = ?;";
      let stmt: SQLite3.Statement = connection.prepare(readSQL);
      const out: ApplicationRow = stmt.get(id) as ApplicationRow;
      dbm.closeConnection(connection) // Close connection, don't need it anymore

      // Update local variables to reflect what we found
      if(out !== undefined) {
        this.id = id;
        this.windowClass = out.class;
        this.windowName = out.name;
        this.isBlacklisted = out.isBlacklisted === 1;
        this.isSettingsModified = out.isSettingsModified === 1;

        // Set variables in map, potentially updating it
        ApplicationMap.create(this.getDTO());
      } else {
        console.log(id + " was undefined")
      }

    } catch (e) {
      throw e; // error is thrown when stmt.run fails
    }
  }

  /**
   * Persist any changes to operating system, window class, window name, isBlacklist on
   * currently held application id.
   * @note should only update isBlacklist and isSettingsModified
   */
  public update(): void {
    try {
      // Get connection, set up statement, and run it
      let connection : SQLite3.Database = dbm.getConnection();
      const updateSQL : string = "UPDATE applications SET isBlacklisted = ?, isSettingsModified = ? WHERE id = ?;";
      let stmt : SQLite3.Statement = connection.prepare(updateSQL);

      stmt.run(this.isBlacklisted ? 1 : 0, this.isSettingsModified ? 1 : 0, this.id);

      dbm.closeConnection(connection); // Close connection, not needed anymore

      // Update ApplicationMap to reflect changes
      if(this.id !== undefined) {
        if (ApplicationMap.read(this.id) !== null) {
          ApplicationMap.update(this.id, this.getDTO());
        }
      }
    } catch (e) {
      throw e // error is thrown when stmt.run fails
    }
  }

  /**
   * Delete the currently loaded application from the database.
   */
  public delete() : void {
    try {
      // Delete app from map and all occurrences of app in Times table
      if(this.id !== undefined) {
        ApplicationMap.delete(this.id);
      }

      // Get connection, set up statement, and run it
      let connection : SQLite3.Database = dbm.getConnection();
      const deleteSQL : string = "DELETE FROM applications WHERE id = ?;"
      let stmt : SQLite3.Statement = connection.prepare(deleteSQL);
      stmt.run(this.id); // Execute delete sql statement on the id

      dbm.closeConnection(connection); // Close connection, not needed anymore

      // Set currently loaded application variables to undefined
      this.id = 0
      this.windowClass = ""
      this.windowName = ""
      this.isBlacklisted = false

    } catch (e) {
      throw e // error is thrown when stmt.run fails
    }
  }

  /**
   * Given an operating system, window class, and window name, determine if that
   * application already exists in the database.  If it does, return its id.
   * @param windowClass string, window class of the application
   * @param windowName string, window name of the application
   * @return id number if application was found, -1 if it was not found.
   */
  public has(windowClass : string, windowName : string) : number {
    let connection : SQLite3.Database = dbm.getConnection()
    let hasSQL : string = "SELECT * FROM applications WHERE class = ? AND name = ?;"
    let stmt : SQLite3.Statement = connection.prepare(hasSQL)
    const out : ApplicationRow = <ApplicationRow> stmt.get(windowClass, windowName)
    dbm.closeConnection(connection)

    return (out !== undefined ? out.id as number : -1)
  }

  /**
   * Given an ApplicationDTO, determine its whitelist status.
   * @param promise ApplicationDTO
   * @return boolean, whether or not the promise is whitelisted. If the application is not laoded,
   * return false by default.
   */
  public hasWhitelisted(promise : ApplicationDTO) : boolean {
    let connection : SQLite3.Database = dbm.getConnection()
    let hasSQL : string = "SELECT * FROM applications WHERE class = ? AND name = ?;"
    let stmt : SQLite3.Statement = connection.prepare(hasSQL);
    const out : ApplicationRow =  stmt.get(promise.windowClass, promise.windowName) as ApplicationRow;
    dbm.closeConnection(connection);

    return out.isBlacklisted === 1
  }

  /**
   * Create an ApplicationDTO from the currently held application.
   * @return ApplicationDTO of held application, or null if no application is loaded.
   */
  public getDTO() : ApplicationDTO {
    if(this.id === undefined && this.windowClass !== undefined && this.windowName !== undefined) {
      console.log("ApplicationRDG id: " + this.id + ": tried to 'getDTO' but no application is loaded.\n");
    }
    return new ApplicationDTO(this.id as number, this.windowClass as string, this.windowName as string, this.isBlacklisted as boolean, this.isSettingsModified as boolean);
  }

}
