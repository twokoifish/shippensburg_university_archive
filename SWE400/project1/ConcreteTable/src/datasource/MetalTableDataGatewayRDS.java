package datasource;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import datadto.MetalDTO;

public class MetalTableDataGatewayRDS {
  /**
   * Creates Table
   * 
   * @throws DatabaseException if it can't create or drop table.
   */
  public static void createTable() throws DatabaseException {
    String drop = "DROP TABLE IF EXISTS Metal";
    String create = "CREATE TABLE Metal (" + 
        "metalID INT NOT NULL AUTO_INCREMENT, " + 
        "name VARCHAR(30) NOT NULL, " + 
        "inventory Double, " + 
        "atomicNumber INT NOT NULL, " + 
        "atomicMass DOUBLE NOT NULL, "+ 
        "acidAmount DOUBLE NOT NULL, " + 
        "dissolvedBy INT, " + 
        "UNIQUE(name), " + 
        "PRIMARY KEY(metalID), " +
        "FOREIGN KEY(dissolvedBy) REFERENCES Acid(acidID)); ";

    try {
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
      throw new DatabaseException("Unable to create Metal table", e);
    }
  }

  /**
   * Only drop the table.
   * 
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
    } catch (SQLException e) {
      throw new DatabaseException("Unable to drop Metal table", e);
    }
  }
  private static double lowInventory = 20.0;
  /**
   * Converts a result set that is assumed to contain a collection of acids from
   * the database.
   * 
   * @param rs the result set, contains a collection of acids.
   * @return a list of AcidDTOs.
   */
  private static List<MetalDTO> toDTOList(ResultSet rs) {
    List<MetalDTO> metalDTOs = new ArrayList<MetalDTO>();
    try {
      while (rs.next()) {
        int metalID = rs.getInt("metalID");
        String name = rs.getString("name");
        double inventory = rs.getDouble("inventory");
        int atomicNumber = rs.getInt("atomicNumber");
        double atomicMass = rs.getDouble("atomicMass");
        double acidAmount = rs.getDouble("acidAmount");
        int dissolvedBy = rs.getInt("dissolvedBy");
        MetalDTO a = new MetalDTO(metalID, name, inventory, atomicNumber, atomicMass, acidAmount, dissolvedBy);
        metalDTOs.add(a);
        
      }
      return metalDTOs;
    } catch (SQLException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return metalDTOs;
  }

  /**
   * Gets all Metals.
   * 
   * @return A list of all rows in Metal.
   */
  public static List<MetalDTO> getAll() {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection().prepareStatement("SELECT * FROM Metal;");
      stmt.execute();
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      new DatabaseException("could not get all");
    }
    return null;
  }
  
  public static List<MetalDTO> filterByDissovedBy(int id){
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE dissolvedBy = " + id);
      stmt.execute();
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);
    } catch(SQLException | DatabaseException e) {
      e.printStackTrace();
      new DatabaseException("couldn't find by dissolvedBy");
    }
    return null;
  }
  
  /**
   * Filters the metal table by a String.
   * 
   * @param nameLike String that is used in filter.
   * @return List of filtered MetalDTOs.
   */
  public static List<MetalDTO> filterByNameLike(String nameLike) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE name LIKE '%" + nameLike + "%'");
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters the metal table by inventory.
   * 
   * @param inventory Given amount.
   * @return List of filtered AcidDTOs.
   */
  public static List<MetalDTO> filterByInventory(double inventory) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE inventory = " + inventory);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters the metal table by a range of inventory.
   * 
   * @param min Lower limit for filter.
   * @param max Upper limit for filter.
   * @return List of filtered MetalDTOs.
   */
  public static List<MetalDTO> filterByInventoryBetween(double min, double max) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE inventory BETWEEN " + min + " AND " + max);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters by the atomic number.
   * 
   * @param atomicNumber number for filter.
   * @return List of filtered MetalDTOs.
   */
  public static List<MetalDTO> filterByAtomicNumber(int atomicNumber) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE atomicNumber = " + atomicNumber);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters by the atomic mass.
   * 
   * @param atomicNumber number for filter.
   * @return List of filtered MetalDTOs.
   */
  public static List<MetalDTO> filterByAtomicMass(double atomicMass) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE atomicMass = " + atomicMass);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
  
  /**
   * Filters by the acid amount.
   * 
   * @param acidAmount number for filter.
   * @return List of filtered MetalDTOs.
   */
  public static List<MetalDTO> filterByAcidAmount(double acidAmount) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE acidAmount = " + acidAmount);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters by a range of atomic mass.
   * 
   * @param min range.
   * @param max range.
   * @return List of filtered MetalDTOs.
   */
  public static List<MetalDTO> filterByAtomicMassBetween(double min, double max) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE atomicMass BETWEEN " + min + " AND " + max);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
  
  /**
   * Filters by a range of atomic number.
   * 
   * @param min range.
   * @param max range.
   * @return List of filtered MetalDTOs.
   */
  public static List<MetalDTO> filterByAtomicNumberBetween(int min, int max) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE atomicNumber BETWEEN " + min + " AND " + max);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
  
  /**
   * Filters by a range of acid amount.
   * 
   * @param min range.
   * @param max range.
   * @return List of filtered MetalDTOs.
   */
  public static List<MetalDTO> filterByAcidAmountBetween(double min, double max) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE acidAmount BETWEEN " + min + " AND " + max);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
  
  public static List<MetalDTO> filterByPartOfCompound(int compoundID) {
    List<MetalDTO> metalDTOs = new ArrayList<MetalDTO>();
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT metalID FROM CompoundMadeOf WHERE compoundID = " + compoundID);
      ResultSet rs = stmt.executeQuery();
   
      while (rs.next()) {
        PreparedStatement stmt2 = DatabaseManager.getSingleton().getConnection()
            .prepareStatement("SELECT * FROM Metal WHERE metalID = " + rs.getInt("metalID"));
        ResultSet rs2 = stmt2.executeQuery();
        rs2.next();
        int metalID = rs2.getInt("metalID");
        String name = rs2.getString("name");
        double inventory = rs2.getDouble("inventory");
        int atomicNumber = rs2.getInt("atomicNumber");
        double atomicMass = rs2.getDouble("atomicMass");
        double acidAmount = rs2.getDouble("acidAmount");
        int dissolvedBy = rs2.getInt("dissolvedBy");
        MetalDTO a = new MetalDTO(metalID, name, inventory, atomicNumber, atomicMass, acidAmount, dissolvedBy);
        metalDTOs.add(a);

      }
      return metalDTOs;

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      //e.printStackTrace();
    }
    return metalDTOs;
  }
  
  public static List<MetalDTO> filterByLowInventory() {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Metal WHERE Metal.inventory <= " + lowInventory);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
  
}
