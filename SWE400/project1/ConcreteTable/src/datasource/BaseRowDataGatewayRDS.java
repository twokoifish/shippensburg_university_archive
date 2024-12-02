package datasource;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
/**
 * Row Data Gateway for Base.
 * @author Chase
 *
 */
public class BaseRowDataGatewayRDS implements BaseRowDataGateway{
  //TODO make solute foreign key???
  /**
   * Creates the table in the database. Drops the table if it already exists.
   * @throws DatabaseException
   */
	public static void createTable() throws DatabaseException{
		String drop = "DROP TABLE IF EXISTS Base";
		String create = "CREATE TABLE Base (" + 
				"baseID INT NOT NULL AUTO_INCREMENT, " + 
				"name VARCHAR(30) NOT NULL, " +                      
				"inventory DOUBLE, " +
				"solute INT, " + 
				"soluteType VARCHAR(30)," +
				"UNIQUE(name), " +
				"PRIMARY KEY(baseID)) ;";
		
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
			throw new DatabaseException("Unable to create Base table", e);
		}
	}
	

	private Connection conn;
	
	private int baseID;
	private String name;
	private Double inventory;
	private int solute;
	private String soluteType;
	
	/**
	 * Constructs Base Row Data Gateway based off of existing row by ID.
	 * @param id
	 * @throws DatabaseException
	 */
	public BaseRowDataGatewayRDS(int id) throws DatabaseException {
		conn = DatabaseManager.getSingleton().getConnection();
		this.baseID = id;
		findByID(id);
	}
	
	/**
	 * Finds existing row by ID.
	 * @param id
	 * @throws DatabaseException
	 */
	private void findByID(int id) throws DatabaseException {
		try {
			PreparedStatement stmt = conn.prepareStatement("SELECT * FROM Base WHERE baseID = " + id);
			ResultSet rs = stmt.executeQuery();
			rs.next();
			name = rs.getString("name");
			inventory = rs.getDouble("inventory");
			solute = rs.getInt("solute");
			soluteType = rs.getString("soluteType");
		} catch (SQLException e) {
			throw new DatabaseException("Couldn't find Base with that name", e);
		}
	}
	
	/**
	 * Constructs Base Row Data Gateway based off of existing row by name.
	 * @param name
	 * @throws DatabaseException
	 */
	public BaseRowDataGatewayRDS(String name) throws DatabaseException{
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
			PreparedStatement stmt = conn.prepareStatement("SELECT * FROM Base WHERE name = '" + name + "'");
			ResultSet rs = stmt.executeQuery();
			rs.next();
			baseID = rs.getInt("baseID");
			inventory = rs.getDouble("inventory");
			solute = rs.getInt("solute");
			soluteType = rs.getString("soluteType");
		} catch (SQLException e) {
			throw new DatabaseException("Couldn't find Base with that name", e);
		}
	}
	
	/**
	 * Constructs new Base Row Data Gateway from given parameters.
	 * @param id
	 * @param name
	 * @param inventory
	 * @param solute
	 * @throws DatabaseException
	 */
	public BaseRowDataGatewayRDS(String name, double inventory, int solute, String soluteType) throws DatabaseException {
		this.name = name;
		this.inventory = inventory;
		this.solute = solute;
		this.soluteType = soluteType;
		conn = DatabaseManager.getSingleton().getConnection();
		insert();
	}

  @Override
  public int getBaseID() {
    return this.baseID;
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
  public int getSolute() {
    return this.solute;
  }
  
  public String getSoluteType() {
    return soluteType;
  }
  
  @Override
  public void setName(String name) {
    this.name = name;
  }

  @Override
  public void setInventory(double inventory) {
    this.inventory = inventory;
  }

  @Override
  public void setSolute(int solute) {
    this.solute = solute;
  }
  
  public void setSoluteType(String soluteType) {
    this.soluteType = soluteType;
  }
  
  /**
   * Updates the information in the database to reflect changes made.
   * @return boolean
   */
  public boolean persist() {
	  try {
  		PreparedStatement stmt = conn.prepareStatement("UPDATE Base SET"
  				+ " name = '" + name
  				+ "', inventory = " + inventory
  				+ ", solute = " + solute
  				+ " , soluteType = '" + soluteType
  				+ "' WHERE baseID = " + baseID);
  		stmt.executeUpdate();
  		return true;
	  } catch (SQLException e) {
	    e.printStackTrace();
  		new DatabaseException("Couldn't update Base table");
  		return false;
	  }
  }
  
  /**
   * Deletes row from database.
   * @return boolean
   */
  public boolean delete() {
    try {
      PreparedStatement stmt1 = conn.prepareStatement("DELETE FROM Base WHERE baseID = " + baseID);
      stmt1.execute();
      return true;
    } catch (SQLException e) {
      new DatabaseException("could not delete base");
      return false;
    }
  }
  
  /**
   * Inserts new row into database.
   */
  private void insert() {
		try {
			PreparedStatement stmt = conn.prepareStatement("INSERT INTO Base(name, inventory, solute, soluteType) VALUES ('" + name + "', '" + inventory + "', '" + solute + "','" + soluteType +"');");
			stmt.execute();
			
			PreparedStatement stmt2 = conn.prepareStatement("SELECT LAST_INSERT_ID();");
      ResultSet rs = stmt2.executeQuery();
      rs.next();
      this.baseID = rs.getInt("LAST_INSERT_ID()");
		} catch(SQLException e) {
			new DatabaseException("could not insert into base table");
		}
	}
}

  