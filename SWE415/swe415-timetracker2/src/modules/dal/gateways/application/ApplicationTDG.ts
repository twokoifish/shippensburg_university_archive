import ApplicationDTO from "./ApplicationDTO";
import SchemaBuilder from "../../database/schemaBuilder/SchemaBuilder";
import DatabaseManager from "../../database/manager/DatabaseManager";
import SQLite3 from 'better-sqlite3'
import ApplicationRDG from "./ApplicationRDG";

const schema : SchemaBuilder = SchemaBuilder.getInstance();
const dbm : DatabaseManager = DatabaseManager.getInstance();

interface ApplicationRow extends SQLite3.RunResult {
  id: number
  class : string
  name : string
  isBlacklisted : number
  isSettingsModified : number
}

export default class ApplicationTDG {
  private static instance : ApplicationTDG | null

  public static getInstance() : ApplicationTDG {
    if(!ApplicationTDG.instance) {
      ApplicationTDG.instance = new ApplicationTDG()
    }

    return ApplicationTDG.instance
  }

  getByClass(wclass : string) : ApplicationDTO[] {
    const getSQL : string = "SELECT * FROM applications WHERE class = ?;"
    return this.executeStatement(getSQL, undefined, wclass)
  }

  getByName(name : string) : ApplicationDTO[] {
    const getSQL : string = "SELECT * FROM applications WHERE name = ?;"
    return this.executeStatement(getSQL, undefined, name)
  }

  getBlacklisted() : ApplicationDTO[] {
    const getSQL : string = "SELECT * FROM applications WHERE isBlacklisted = 1;"
    return this.executeStatement(getSQL)
  }

  getWhitelisted() : ApplicationDTO[] {
    const getSQL : string = "SELECT * FROM applications WHERE isBlacklisted = 0;"
    return this.executeStatement(getSQL)
  }

  // getCurrentForeground() : ApplicationDTO {
  //   const getSQL : string = "SELECT * FROM applications WHERE isForeground = 1"
  //   return this.executeStatement(getSQL)[0]
  // }
  //
  // getPriorForeground() : ApplicationDTO[] {
  //   const getSQL : string = "SELECT * FROM applications WHERE isForeground = 0"
  //   return this.executeStatement(getSQL)
  // }

  getAll() : ApplicationDTO[] {
    const getSQL : string = "SELECT * FROM applications;"
    return this.executeStatement(getSQL)
  }

  executeStatement(getSQL: string, id?: number, str?: string): ApplicationDTO[] {
    try {
      let connection = dbm.getConnection()
      let stmt : SQLite3.Statement = connection.prepare(getSQL)
      let out : ApplicationRow[]

      if(id !== undefined) { // if id is valid ??
        out = stmt.all(id) as ApplicationRow[]
      } else if (str !== undefined) { // if id is undefined
        out =  stmt.all(str) as ApplicationRow[]
      } else { // if both are undefined
        out = stmt.all() as ApplicationRow[]
      }

      dbm.closeConnection(connection)

      let list: ApplicationDTO[] = []
      console.log(getSQL + "? = " + id + " ? = " + str)
      out.forEach((entry : ApplicationRow) => {
        let rdg : ApplicationRDG = new ApplicationRDG()
        rdg.read(entry.id)
        console.log(entry.id + " " +  entry.class + " " +  entry.name)
        list.push(new ApplicationDTO(entry.id, entry.class, entry.name, entry.isBlacklisted === 1, entry.isSettingsModified === 1));
      })
      console.log("END")
      return list
    } catch (e) {
      throw e
    }
  }
}