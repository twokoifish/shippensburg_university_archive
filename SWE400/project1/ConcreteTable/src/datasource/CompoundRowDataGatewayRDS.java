package datasource;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Row Data Gateway for Compound.
 * @author Chase and Joel.
 *
 */
public class CompoundRowDataGatewayRDS implements CompoundRowDataGateway{
  
  /**
   * Creates the table in the database. Drops the table if it already exists.
   * @throws DatabaseException
   */
	public static void createTable() throws DatabaseException{
		
		String drop = "DROP TABLE IF EXISTS Compound";
		String create = "CREATE TABLE Compound (" + 
				"compoundID INT NOT NULL AUTO_INCREMENT, " + 
				"name VARCHAR(30) NOT NULL, " +                      
				"inventory Double, " + 
				"UNIQUE(name), " +
				"PRIMARY KEY(compoundID)) ;";

		try
		{
			// drop table
			PreparedStatement stmt;
			stmt = DatabaseManager.getSingleton().getConnection().prepareStatement(drop);
			stmt.execute();
			stmt.close();

			// create table
			stmt = DatabaseManager.getSingleton().getConnection().prepareStatement(create);
			stmt.execute();
			stmt.close();
		} catch (SQLException e) {
			throw new DatabaseException("Unable to create compound table", e);
		}
	}
	
	/**
   * Only drop the table.
   * 
   * @return
   * @throws DatabaseException
   */
  public static void dropTable() throws DatabaseException {
    String drop = "DROP TABLE IF EXISTS Compound";
    try {
      // drop table
      PreparedStatement stmt;
      stmt = DatabaseManager.getSingleton().getConnection().prepareStatement(drop);
      stmt.execute();
      stmt.close();
    } catch (SQLException e) {
      throw new DatabaseException("Unable to drop Compound table", e);
    }
  }

	private Connection conn;
	
	private int compoundID;
	private String name;
	private double inventory;
	
	/**
   * Constructs Compound Row Data Gateway based off of existing row by ID.
   * @param id
   * @throws DatabaseException
   */
	public CompoundRowDataGatewayRDS(int id) throws DatabaseException {
		conn = DatabaseManager.getSingleton().getConnection();
		this.compoundID = id;
		findByID(id);
	}
	
	/**
   * Finds existing row by ID.
   * @param id
   * @throws DatabaseException
   */
	private void findByID(int id) throws DatabaseException {
		try {
			PreparedStatement stmt = conn.prepareStatement("SELECT * FROM Compound WHERE compoundID = " + id);
			ResultSet rs = stmt.executeQuery();
			rs.next();
			name = rs.getString("name");
			inventory = rs.getDouble("inventory");
		} catch (SQLException e) {
			throw new DatabaseException("Couldn't find Compound with that Id", e);
		}
		
	}
	
	/**
   * Constructs Compound Row Data Gateway based off of existing row by name.
   * @param name
   * @throws DatabaseException
   */
	public CompoundRowDataGatewayRDS(String name) throws DatabaseException{
		conn = DatabaseManager.getSingleton().getConnection();
		this.name = name;
		findByName(name);
	}
	
	/**
   * Finds existing row by Name.
   * @param name
   * @throws DatabaseException
   */
	private void findByName(String name) throws DatabaseException{
		try {
			PreparedStatement stmt = conn.prepareStatement("SELECT * FROM Compound WHERE name = '" + name + "'");
			ResultSet rs = stmt.executeQuery();
			rs.next();
			compoundID = rs.getInt("compoundID");
			inventory = rs.getDouble("inventory");
		} catch (SQLException e) {
			throw new DatabaseException("Couldn't find Compound with that name", e);
		}
	}
	
	/**
	 * Constructs new Base Row Data Gateway from given parameters.
	 * @param id
	 * @param name
	 * @param inventory
	 * @throws DatabaseException
	 */
	public CompoundRowDataGatewayRDS(String name, double inventory) throws DatabaseException {
		this.name = name;
		this.inventory = inventory;
		conn = DatabaseManager.getSingleton().getConnection();
		insert();
	}

  @Override
  public int getCompoundID() {
    return this.compoundID;
  }

  @Override
  public String getName() {
    return this.name;
  }

  @Override
  public double getInventory() {
    return this.inventory;
  }
  
  @Override
  public void setName(String n) {
   this.name = n;
  }

  @Override
  public void setInventory(double i) {
    this.inventory = i;
  }
  
  /**
   * Updates the information in the database to reflect changes made.
   * @return boolean
   */
  public boolean persist() {
    try {
      PreparedStatement stmt = conn.prepareStatement("UPDATE Compound SET"
          + " name = '" + name
          + "', inventory = '" + inventory
          + "' WHERE compoundID = " + compoundID);
      stmt.executeUpdate();
      return true;
    } catch (SQLException e) {
      new DatabaseException("Couldn't update Compound table");
      return false;
    }
  }
  
  /**
   * Deletes row from database.
   * @return boolean
   */
  public boolean delete() {
  	try {
  		PreparedStatement stmt1 = conn.prepareStatement("DELETE FROM CompoundMadeOf WHERE compoundID = " + compoundID);
  		stmt1.execute();
  		PreparedStatement stmt2 = conn.prepareStatement("DELETE FROM Compound WHERE compoundID = " + compoundID);
  		stmt2.execute();
  		return true;
  	} catch (SQLException e) {
  		new DatabaseException("could not delete Compound");
  		return false;
  	}
  }
  
  /**
   * Inserts new row into database.
   */
  private void insert() {
		try {
			PreparedStatement stmt = conn.prepareStatement("INSERT INTO Compound(name, inventory) VALUES ('" + name + "', '" + inventory + "');");
			stmt.execute();
			
			PreparedStatement stmt2 = conn.prepareStatement("SELECT LAST_INSERT_ID();");
      ResultSet rs = stmt2.executeQuery();
      rs.next();
      this.compoundID = rs.getInt("LAST_INSERT_ID()");
		} catch(SQLException e) {
			new DatabaseException("could not insert into compound table");
		}
	}
}
