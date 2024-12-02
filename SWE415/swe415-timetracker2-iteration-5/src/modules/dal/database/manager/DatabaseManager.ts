import SQLite3 = require('better-sqlite3')

/**
 * Provides access to the database.
 */
export default class DatabaseManager {

  private static instance : DatabaseManager;
  private static databasePath: string;

  /**
   * Private, sets the database path.
   * @private
   */
  private constructor() {
    DatabaseManager.databasePath = (__dirname + '/mongoose.db');
  }

  /**
   * Get access to the singleton instance of DatabaseManager.
   */
  public static getInstance(): DatabaseManager {
    if (!DatabaseManager.instance) {
      DatabaseManager.instance = new DatabaseManager();
    }
    return DatabaseManager.instance;
  }

  /**
   * Get a new database connection.
   */
  public getConnection() : SQLite3.Database {
    return new SQLite3(DatabaseManager.databasePath)
  }

  /**
   * Close the provided database connection.
   * @param connection the connection to be closed.
   */
  public closeConnection(connection: SQLite3.Database) {
    connection.close()
  }
}
