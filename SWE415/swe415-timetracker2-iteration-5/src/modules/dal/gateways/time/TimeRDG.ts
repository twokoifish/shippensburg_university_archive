import DatabaseManager from "../../database/manager/DatabaseManager"
import TableManager from "../../database/schemaBuilder/SchemaBuilder"
import SQLite3 = require('better-sqlite3');
const dbm : DatabaseManager = DatabaseManager.getInstance()
const tm : TableManager =TableManager.getInstance()

interface TimeRow extends SQLite3.RunResult{
  id: number
  start: number
  end: number
  wasIdle: number
  wasForeground: number
}

export default class TimeRDG {

  _id: number | undefined
  _start: number | undefined
  _end: number | undefined
  _wasIdle: boolean | undefined
  _wasForeground: boolean | undefined

  /**
   *
   */
  constructor() {

  }

  /**
   *
   * @param id number
   * @param start number time app started tracking
   * @param end number time stopped tracking
   * @param wasIdle whether or not the user was idle during the tracking
   * @param wasForeground whether or not the application was in the foreground during tracking
   */
  create(id: number, start: number, end: number, wasIdle: boolean, wasForeground: boolean) : void {
    try {
      let connection: SQLite3.Database = dbm.getConnection()
      let bool : number = (wasIdle ? 1 : 0)
      let foreground: number = (wasForeground ? 1 : 0);
      let createSQL: string =
        "INSERT INTO times(id, start, end, wasIdle, wasForeground) VALUES (?, ?, ?, ?, ?);"
      let stmt : SQLite3.Statement = connection.prepare(createSQL)
      const out: TimeRow  = <TimeRow>stmt.run(id, start, end, bool, foreground)
      dbm.closeConnection(connection)

      this._id = out.id
      this._start = out.start
      this._end = out.end
      this._wasIdle = (out.wasIdle === 1)
      /** @TODO how does application map handle this? */
      this._wasForeground = (out.wasForeground === 1)
    } catch (e) {
      throw e
    }
  }

  /**
   *
   * @param id
   * @param startTime
   * @param endTime
   */
  read(id: number, startTime : number, endTime : number) : void {
    try {
      let connection: SQLite3.Database = dbm.getConnection();
      let readSQL: string = "SELECT * FROM times WHERE id = ? AND start = ? AND end = ?;";
      let stmt: SQLite3.Statement = connection.prepare(readSQL);
      const out: TimeRow | undefined = <TimeRow>stmt.get(id, startTime, endTime);

      dbm.closeConnection(connection);
      this._id = out.id;
      this._start = out.start;
      this._end = out.end;
      this._wasIdle = !!out.wasIdle;
      this._wasForeground = !!out.wasForeground;
    } catch (e) {
      throw e;
    }
  }

  /**
   *
   */
  update() : void {
    try {
      let connection: SQLite3.Database  = dbm.getConnection();
      const updateSQL: string = "UPDATE times SET start = ?, end = ?, wasIdle = ? WHERE id = ?;";
      let stmt: SQLite3.Statement = connection.prepare(updateSQL);
      let bool : number = (this.wasIdle ? 1 : 0);
      stmt.run(this.start, this.end, bool, this.id);
      dbm.closeConnection(connection)
    } catch (e) {
      throw e;
    }
  }

  /**
   *
   */
  delete() : void {
    try {
      let connection: SQLite3.Database  = dbm.getConnection();
      const deleteSQL: string = "DELETE FROM times WHERE id = ? AND start = ? AND end = ?;"
      let stmt: SQLite3.Statement = connection.prepare(deleteSQL);
      stmt.run(this.id, this.start, this.end);

      dbm.closeConnection(connection);

      this._id = undefined;
      this._start = undefined;
      this._end = undefined;
      this._wasIdle = undefined;
    } catch (e) {
      throw e;
    }
  }

  /**
   *
   * @returns {*}
   */
  get id() {
    return this._id
  }

  /**
   *
   * @returns {*}
   */
  get start() {
    return this._start
  }

  /**
   *
   * @param start
   */
  set start(start)  {
    this._start = start
  }

  /**
   *
   * @returns {*}
   */
  get end() {
    return this._end
  }

  /**
   *
   * @param end
   */
  set end(end) {
    this._end = end
  }

  /**
   *
   * @returns {*}
   */
  get wasIdle() {
    return this._wasIdle
  }

  /**
   *
   * @param wasIdle
   */
  set wasIdle(wasIdle) {
    this._wasIdle = wasIdle
  }

  get wasForeground(){
    return this._wasForeground
  }

  /**
   * 
   * @param wasForeground
   */
  set wasForeground(wasForeground){
    this._wasForeground = wasForeground
  }

}
