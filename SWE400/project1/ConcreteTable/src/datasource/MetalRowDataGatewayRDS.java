package datasource;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Row Data Gateway for Metal.
 * @author Chase
 *
 */
public class MetalRowDataGatewayRDS implements MetalRowDataGateway{
	
  /*
	 * Creates Table
	 * 
	 * @throws DatabaseException if it can't create or drop table.
	 */
	public static void createTable() throws DatabaseException
	{
		String drop = "DROP TABLE IF EXISTS Metal";
		String create = "CREATE TABLE Metal (" + 
				"metalID INT NOT NULL AUTO_INCREMENT, " + 
				"name VARCHAR(30) NOT NULL, " +                     
				"inventory Double, " +
				"atomicNumber INT NOT NULL, " +
				"atomicMass DOUBLE NOT NULL, " +
				"acidAmount DOUBLE NOT NULL, " +
				"dissolvedBy INT, " + 
				"UNIQUE(name), " +
				"PRIMARY KEY(metalID), " +
				"FOREIGN KEY(dissolvedBy) REFERENCES Acid(acidID)); ";

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
		} catch (SQLException e)
		{
			throw new DatabaseException("Unable to create Metal table", e);
		}
	}
	
	/**
   * Only drop the table.
   * @throws DatabaseException
   */
	 public static void dropTable() throws DatabaseException {
	   String drop = "DROP TABLE IF EXISTS Metal";
	   try {
	      // drop table
	      PreparedStatement stmt;
	      stmt = DatabaseManager.getSingleton().getConnection().prepareStatement(drop);
	      stmt.execute();
	      stmt.close();
	    } catch (SQLException e)
      {
        throw new DatabaseException("Unable to drop Metal table", e);
      }
	 }

	private Connection conn;
	
	private int metalID;
	private String name;
	private double inventory;
	private int atomicNumber;
	private double atomicMass;
	private double acidAmount;
	private int dissolvedBy;
	
	/**
   * Constructs Metal Row Data Gateway based off of existing row by ID.
   * @param id
   * @throws DatabaseException
   */
	public MetalRowDataGatewayRDS(int id) throws DatabaseException {
		conn = DatabaseManager.getSingleton().getConnection();
		this.metalID = id;
		findByID(id);
	}
	
	/**
   * Finds existing row by ID.
   * @param id
   * @throws DatabaseException
   */
	private void findByID(int id) throws DatabaseException{
		try {
			PreparedStatement stmt = conn.prepareStatement("SELECT * FROM Metal WHERE metalID = " + id);
			ResultSet rs = stmt.executeQuery();
			rs.next();
			name = rs.getString("name");
			inventory = rs.getDouble("inventory");
			atomicNumber = rs.getInt("atomicNumber");
			atomicMass = rs.getDouble("atomicMass");
			acidAmount = rs.getDouble("acidAmount");
			dissolvedBy = rs.getInt("dissolvedBy");
		} catch (SQLException e) {
		  e.printStackTrace();
			throw new DatabaseException("Couldn't find metal with that ID", e);
		}
	}
	
	/**
   * Constructs Metal Row Data Gateway based off of existing row by name.
   * @param name
   * @throws DatabaseException
   */
	public MetalRowDataGatewayRDS(String name) throws DatabaseException{
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
			PreparedStatement stmt = conn.prepareStatement("SELECT * FROM Metal WHERE name = '" + name + "'");
			ResultSet rs = stmt.executeQuery();
			rs.next();
			metalID = rs.getInt("metalID");
			inventory = rs.getDouble("inventory");
			atomicNumber = rs.getInt("atomicNumber");
			atomicMass = rs.getDouble("atomicMass");
			acidAmount = rs.getDouble("acidAmount");
			dissolvedBy = rs.getInt("dissolvedBy");
		} catch (SQLException e) {
			throw new DatabaseException("Couldn't find metal with that name", e);
		}
	}
	
	/**
	 * Constructs new Metal Row Data Gateway from given parameters.
	 * @param id
	 * @param name
	 * @param inventory
	 * @param atomicNumber
	 * @param atomicMass
	 * @param dissolvedBy
	 * @throws DatabaseException
	 */
	public MetalRowDataGatewayRDS(String name, double inventory, int atomicNumber, double atomicMass, double acidAmount, int dissolvedBy) throws DatabaseException {
	  conn = DatabaseManager.getSingleton().getConnection();
		this.name = name;
		this.inventory = inventory;
		this.atomicNumber = atomicNumber;
		this.atomicMass = atomicMass;
		this.acidAmount = acidAmount;
		this.dissolvedBy = dissolvedBy;
		insert();
	}

  @Override
  public int getMetalID() {
    return this.metalID;
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
  
  public double getAcidAmount() {
    return this.acidAmount;
  }

  @Override
  public int getDissolvedBy() {
    return this.dissolvedBy;
  }

  @Override
  public void setName(String s) {
    this.name = s;
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
  
  public void setAcidAmount(double d) {
    this.acidAmount = d;
  }
  @Override
  public void setDissolvedBy(int i) {
    this.dissolvedBy = i;
  }
  
  /**
   * Updates the information in the database to reflect changes made.
   * @return boolean
   */
  public boolean persist() {
	  try {
		PreparedStatement stmt = conn.prepareStatement("UPDATE Metal SET"
				+ " name = '" + name
				+ "', inventory = '" + inventory
				+ "', atomicNumber = " + atomicNumber
				+ ", atomicMass = " + atomicMass
				+ ", acidAmount = " + acidAmount
				+ ", dissolvedBy = " + dissolvedBy
				+ " WHERE metalID = " + metalID);
		
		stmt.executeUpdate();
		return true;
	} catch (SQLException e) {
		new DatabaseException("could't update metal table");
		return false;
	}
  }

  /**
   * Deletes row from database.
   * @return boolean
   */
  public boolean delete() {
	  try {
		  PreparedStatement stmt = conn.prepareStatement("DELETE FROM Metal WHERE metalID = " + metalID + ";");  
		  stmt.execute();
		  return true;
	  } catch (SQLException e) {
			new DatabaseException("couldn't delete metal");
			return false;
	  }
  }
  
  /**
   * Inserts new row into database.
   */
  private void insert() {
		try {
		  if(this.dissolvedBy == -1) {
		    PreparedStatement stmt = conn.prepareStatement("INSERT INTO Metal (name, inventory, atomicNumber, atomicMass, acidAmount, dissolvedBy) VALUES ('" + name + "', '" + inventory + "', " + atomicNumber + ", " + atomicMass + ", "  + acidAmount + ", null);");
	      stmt.execute();
		  }
		  else {
  			PreparedStatement stmt = conn.prepareStatement("INSERT INTO Metal (name, inventory, atomicNumber, atomicMass, acidAmount, dissolvedBy) VALUES ('" + name + "', '" + inventory + "', " + atomicNumber + ", " + atomicMass + ", "  + acidAmount + ", "+ dissolvedBy +");");
  			stmt.execute();
		  }
			
			PreparedStatement stmt2 = conn.prepareStatement("SELECT LAST_INSERT_ID();");
      ResultSet rs = stmt2.executeQuery();
      rs.next();
      this.metalID = rs.getInt("LAST_INSERT_ID()");
		} catch(SQLException e) {
		  e.printStackTrace();
			new DatabaseException("could not insert into Metal table");
		}
	}
}
