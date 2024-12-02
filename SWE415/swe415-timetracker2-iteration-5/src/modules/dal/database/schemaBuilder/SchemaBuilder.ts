import DatabaseManager from "../manager/DatabaseManager";
import SQLite3 = require('better-sqlite3')


const dbm: DatabaseManager = DatabaseManager.getInstance()

export default class SchemaBuilder {
  private static instance: SchemaBuilder | null

  public static getInstance(): SchemaBuilder {
    if (!SchemaBuilder.instance) {
      SchemaBuilder.instance = new SchemaBuilder()
    }

    return SchemaBuilder.instance
  }

  private isBuilt: boolean = false;

  public buildUp() {
    if (!this.isBuilt) {
      const createApplications =
        "CREATE TABLE IF NOT EXISTS applications(" +
        "id INTEGER NOT NULL PRIMARY KEY, " +
        "class TEXT NOT NULL, " +
        "name TEXT NOT NULL, " +
        "isBlacklisted INTEGER NOT NULL, " +
        "isSettingsModified INTEGER NOT NULL, " +
        "CONSTRAINT CHK_bl CHECK (isBlacklisted >= 0 AND isBlacklisted <= 1)," +
        "CONSTRAINT CHK_sm CHECK (isSettingsModified >= 0 AND isSettingsModified <= 1));"
      const createTimes =
        "CREATE TABLE IF NOT EXISTS times(" +
        "id INTEGER NOT NULL, " +
        "start INTEGER NOT NULL, " +
        "end INTEGER NOT NULL, " +
        "wasIdle INTEGER NOT NULL, " +
        "wasForeground INTEGER NOT NULL, " +
        "CONSTRAINT CHK_idle CHECK (wasIdle >= 0 AND wasIdle <= 1), " +
        "CONSTRAINT CHK_fg CHECK (wasForeground >= 0 AND wasForeground <= 1)," +
        "FOREIGN KEY(id) REFERENCES applications(id));"

      this.execute(createApplications, createTimes)
    }
  }

  /**
   * Drop the applications and times tables.
   */
  public tearDown(): void {
    if (this.isBuilt) {
      const dropTimes: string = "DROP TABLE IF EXISTS times;";
      const dropApplications: string = "DROP TABLE IF EXISTS applications;";
      this.execute(dropTimes, dropApplications);
    }
  }

  /**
   * Execute two SQL statements.
   * @param q1 string Statement 1 to execute.
   * @param q2 string Statement 2 to execute.
   * @private
   */
  private execute(q1: string, q2: string): void {
    try {
      let connection: SQLite3.Database = dbm.getConnection();
      let stmt1: SQLite3.Statement = connection.prepare(q1);
      stmt1.run();
      let stmt2: SQLite3.Statement = connection.prepare(q2);
      stmt2.run();
      dbm.closeConnection(connection);
      this.isBuilt = !this.isBuilt;
    } catch (e) {
      throw e;
    }

  }
}