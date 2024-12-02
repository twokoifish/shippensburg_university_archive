import Database, {Statement} from "better-sqlite3";

import DatabaseManager from "../../database/manager/DatabaseManager"
import TimeDTO from "./TimeDTO"
import SQLite3 from 'better-sqlite3'
import Time from "../../../logicLayer/mapper/Time";

const dbm : DatabaseManager = DatabaseManager.getInstance();
/**
 * @TODO Convert to singleton and change imports
 */
interface TimeRow extends Database.RunResult{
  id: number
  start: number
  end: number
  sessionNumber: number
  wasIdle: boolean
  wasForeground : boolean
}

export class TimeTDG {

  private static instance : TimeTDG | null

  public static getInstance() : TimeTDG {
    if(!TimeTDG.instance) {
      TimeTDG.instance = new TimeTDG()
    }

    return TimeTDG.instance
  }
  /**
   *
   * @param id
   * @returns TimeDTO[]
   */
  getById(id: number):TimeDTO[] {
    const getSQL: string = "SELECT * FROM times WHERE id = ?;"
    return this.executeStatementID(getSQL, id)
  }

  /**
   *
   * @returns TimeDTO[]
   */
  getByWasIdle(): TimeDTO[] {
      const getSQL: string = "SELECT * FROM times WHERE wasIdle = 1;"
      return this.executeStatement(getSQL)
  }

  /**
   *
   * @returns TimeDTO[]
   */
  getByWasNotIdle():TimeDTO[] {
      const getSQL: string = "SELECT * FROM times WHERE wasIdle = 0;"
      return this.executeStatement(getSQL)
  }

  getByWasForeground(): TimeDTO[]{
    const getSQL: string = "SELECT * FROM times WHERE wasForeground = 1;"
    return this.executeStatement(getSQL)
  }

  getByWasNotForeground(): TimeDTO[] {
    const getSQL: string = "SELECT * FROM times WHERE wasForeground = 0;"
    return this.executeStatement(getSQL)
  }

  /**
   * may not work...
   * @param id
   */
  deleteByID(id:number){
    const getSQL:string = "DELETE FROM times WHERE id = ?;"
    try{
      let connection = dbm.getConnection()
      let stmt: SQLite3.Statement = connection.prepare(getSQL)
      stmt.run(id)
    }catch (e) {
      throw e
      //should this return empty list?s
    }
  }

  executeStatementID(getSQL: string, id: number):TimeDTO[]{
    try{
      let connection = dbm.getConnection()
      let stmt: SQLite3.Statement = connection.prepare(getSQL)
      let out: TimeRow[]
      out = <TimeRow[]> stmt.all(id)


      dbm.closeConnection(connection)
      let list: TimeDTO[] = []
      out.forEach((entry: TimeRow) => {
        list.push(new TimeDTO(entry.id, entry.start, entry.end, entry.sessionNumber, entry.wasIdle, entry.wasForeground));
      })
      return list
    } catch (e) {
      throw e
      //should this return empty list?s
    }
  }

  executeStatement(getSQL: string):TimeDTO[]{
    try{
      let connection = dbm.getConnection()
      let stmt: SQLite3.Statement = connection.prepare(getSQL)
      let out: TimeRow[]
      out = <TimeRow[]> stmt.all()

      dbm.closeConnection(connection)
      let list: TimeDTO[] = []
      out.forEach((entry: TimeRow) => {
        list.push(new TimeDTO(entry.id, entry.start, entry.end, entry.sessionNumber, entry.wasIdle, entry.wasForeground));
      })
      return list
    } catch (e) {
      throw e
      //should this return empty list?s
    }
  }
}
