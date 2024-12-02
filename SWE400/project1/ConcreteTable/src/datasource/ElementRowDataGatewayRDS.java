package datasource;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Row Data Gateway for Element.
 * @author 
 *
 */
public class ElementRowDataGatewayRDS implements ElementRowDataGateway{
	
  /**
   * Creates the table in the database. Drops the table if it already exists.
   * @throws DatabaseException
   */
	public static void createTable() throws DatabaseException{
		String drop = "DROP TABLE IF EXISTS Element";
		String create = "CREATE TABLE Element (" + 
				"elementID INT NOT NULL AUTO_INCREMENT, " + 
				"name VARCHAR(30) NOT NULL, " +                      
				"inventory Double, " +
				"atomicNumber INT NOT NULL, " +
				"atomicMass DOUBLE NOT NULL," + 
				"UNIQUE(name)," +
			  "PRIMARY KEY(elementID));";
		
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
			throw new DatabaseException("Unable to create Element table", e);
		}
		
	}
	
	/**
   * Only drop the table.
   * 
   * @throws DatabaseException
   */
  public static void dropTable() throws DatabaseException {
    String drop = "DROP TABLE IF EXISTS Element";
    try {
      // drop table
      PreparedStatement stmt;
      stmt = DatabaseManager.getSingleton().getConnection().prepareStatement(drop);
      stmt.execute();
      stmt.close();
    } catch (SQLException e) {
      e.printStackTrace();
      throw new DatabaseException("Unable to drop Element table", e);
    }
  }

	private Connection conn;
	
	private int elementID;
	private String name;
	private double inventory;
	private int atomicNumber;
	private double atomicMass;
	
	/**
   * Constructs Element Row Data Gateway based off of existing row by ID.
   * @param id
   * @throws DatabaseException
   */
	public ElementRowDataGatewayRDS(int id) throws DatabaseException {
		conn = DatabaseManager.getSingleton().getConnection();
		this.elementID = id;
		findByID(id);
	}
	
	/**
   * Finds existing row by ID.
   * @param id
   * @throws DatabaseException
   */
	private void findByID(int id) throws DatabaseException {
		try {
			PreparedStatement stmt = conn.prepareStatement("SELECT * FROM Element WHERE elementID = " + id);
			ResultSet rs = stmt.executeQuery();
			rs.next();
			name = rs.getString("name");
			inventory = rs.getDouble("inventory");
			atomicNumber = rs.getInt("atomicNumber");
			atomicMass = rs.getDouble("atomicMass");
		} catch (SQLException e) {
			throw new DatabaseException("Couldn't find element with that ID", e);
		}
	}
	
	/**
   * Constructs Element Row Data Gateway based off of existing row by name.
   * @param name
   * @throws DatabaseException
   */
	public ElementRowDataGatewayRDS(String name) throws DatabaseException{
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
			PreparedStatement stmt = conn.prepareStatement("SELECT * FROM Element WHERE name = '" + name + "'");
			ResultSet rs = stmt.executeQuery();
			rs.next();
			elementID = rs.getInt("elementID");
			inventory = rs.getDouble("inventory");
			atomicNumber = rs.getInt("atomicNumber");
			atomicMass = rs.getDouble("atomicMass");
		} catch (SQLException e) {
			throw new DatabaseException("Couldn't find element with that name", e);
		}
	}
	
	/**
	 * Constructs new Element Row Data Gateway from given parameters.
	 * @param id
	 * @param name
	 * @param inventory
	 * @param atomicNumber
	 * @param atomicMass
	 * @throws DatabaseException
	 */
	public ElementRowDataGatewayRDS(String name, double inventory, int atomicNumber, double atomicMass) throws DatabaseException {
		this.name = name;
		this.inventory = inventory;
		this.atomicNumber = atomicNumber;
		this.atomicMass = atomicMass;
		conn = DatabaseManager.getSingleton().getConnection();
		insert();
	}
	
  @Override
  public int getElementID() {
    return this.elementID;
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
  public int getAtomicNumber() {
    return this.atomicNumber;
  }

  @Override
  public double getAtomicMass() {
    return this.atomicMass;
  }

  @Override
  public void setName(String n) {
    this.name = n;
  }

  @Override
  public void setInventory(double i) {
    this.inventory = i;
  }

  @Override
  public void setAtomicNumber(int i) {
    this.atomicNumber = i;
    
  }

  @Override
  public void setAtomicMass(double d) {
    this.atomicMass = d;
    
  }
  
  /**
   * Updates the information in the database to reflect changes made.
   * @return boolean
   */
  public boolean persist() {
	  try {
		PreparedStatement stmt = conn.prepareStatement("UPDATE Element SET"
				+ " name = '" + name
				+ "', inventory = '" + inventory
				+ "', atomicNumber = " + atomicNumber
				+ ", atomicMass = " + atomicMass
				+ " WHERE elementID = " + elementID);
		
		stmt.executeUpdate();
		return true;
	} catch (SQLException e) {
		new DatabaseException("could't update element table");
		return false;
	}
  }

  /**
   * Deletes row from database.
   * @return boolean
   */
  public boolean delete() {
	  try {
			PreparedStatement stmt1 = conn.prepareStatement("DELETE FROM CompoundMadeOf WHERE elementID = " + elementID);
			stmt1.execute();
			PreparedStatement stmt2 = conn.prepareStatement("DELETE FROM Element WHERE elementID = " + elementID);
			stmt2.execute();
			return true;
		} catch (SQLException e) {
			new DatabaseException("could not delete Element");
			e.printStackTrace();
			return false;
		}
  }
  
  /**
   * Inserts new row into database.
   */
  private void insert() {
		try {
			PreparedStatement stmt = conn.prepareStatement("INSERT INTO Element(name, inventory, atomicNumber, atomicMass) VALUES ('" + name + "', '" + inventory + "', " + atomicNumber + ", " + atomicMass + ");");
			stmt.execute();
			
			PreparedStatement stmt2 = conn.prepareStatement("SELECT LAST_INSERT_ID();");
      ResultSet rs = stmt2.executeQuery();
      rs.next();
      this.elementID = rs.getInt("LAST_INSERT_ID()");
		} catch(SQLException e) {
			new DatabaseException("could not insert into Element table");
		}
	}
}
