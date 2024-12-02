package datasource;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import datadto.CompoundDTO;

public abstract class CompoundTableDataGatewayRDS {
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

  private static double lowInventory = 20.0;
  
  /**
   * Converts a result set that is assumed to contain a collection of acids from
   * the database.
   * 
   * @param rs the result set, contains a collection of bases.
   * @return a list of BaseDTOs.
   */
  private static List<CompoundDTO> toDTOList(ResultSet rs) {
    List<CompoundDTO> compoundDTOs = new ArrayList<CompoundDTO>();
    try {
      while (rs.next()) {
        int compoundID = rs.getInt("compoundID");
        String name = rs.getString("name");
        double inventory = rs.getDouble("inventory");
        CompoundDTO a = new CompoundDTO(compoundID, name, inventory);
        compoundDTOs.add(a);
        
      }
      return compoundDTOs;
    } catch (SQLException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return compoundDTOs;
  }
  
  /**
   * Gets all Compounds.
   * 
   * @return A list of all rows in Compound.
   */
  public static List<CompoundDTO> getAll() {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection().prepareStatement("SELECT * FROM Compound;");
      stmt.execute();
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      new DatabaseException("could not get all");
    }
    return null;
  }

 /**
  * Filters the compound table by a String.
  * 
  * @param wildCard String that is used in filter.
  * @return List of filtered CompoundDTOs.
  */
 public static List<CompoundDTO> filterByNameLike(String wildCard) {
   try {
     PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
         .prepareStatement("SELECT * FROM Compound WHERE name LIKE '%" + wildCard + "%'");
     ResultSet rs = stmt.executeQuery();
     return toDTOList(rs);

   } catch (SQLException | DatabaseException e) {
     // TODO Auto-generated catch block
     e.printStackTrace();
   }
   return null;
 }

 /**
  * Filters the Compound table by inventory.
  * 
  * @param inventory Given amount.
  * @return List of filtered CompoundDTOs.
  */
 public static List<CompoundDTO> filterByInventory(double inventory) {
   try {
     PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
         .prepareStatement("SELECT * FROM Compound WHERE inventory = " + inventory);
     ResultSet rs = stmt.executeQuery();
     return toDTOList(rs);

   } catch (SQLException | DatabaseException e) {
     // TODO Auto-generated catch block
     e.printStackTrace();
   }
   return null;
 }
 
 /**
  * Filters the Compound table by inventory.
  * 
  * @param inventory Given amount.
  * @return List of filtered CompoundDTOs.
  */
 public static List<CompoundDTO> filterByLowInventory() {
   try {
     PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
         .prepareStatement("SELECT * FROM Compound WHERE inventory <= " + lowInventory);
     ResultSet rs = stmt.executeQuery();
     return toDTOList(rs);

   } catch (SQLException | DatabaseException e) {
     // TODO Auto-generated catch block
     e.printStackTrace();
   }
   return null;
 }

 /**
  * Filters the Compound table by a range of inventory.
  * 
  * @param min Lower limit for filter.
  * @param max Upper limit for filter.
  * @return List of filtered CompoundDTOs.
  */
 public static List<CompoundDTO> filterByInventoryBetween(double min, double max) {
   try {
     PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
         .prepareStatement("SELECT * FROM Compound WHERE inventory BETWEEN " + min + " AND " + max);
     ResultSet rs = stmt.executeQuery();
     return toDTOList(rs);

   } catch (SQLException | DatabaseException e) {
     // TODO Auto-generated catch block
     e.printStackTrace();
   }
   return null;
 }
 
 public static List<CompoundDTO> filterByMadeOf(int elementID) {
   try {
     PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
         .prepareStatement("SELECT compoundID FROM CompoundMadeOf WHERE elementID = " + elementID);

     ResultSet rs = stmt.executeQuery();
     List<CompoundDTO> compoundDTOs = new ArrayList<CompoundDTO>();
     while (rs.next()) {
       PreparedStatement stmt2 = DatabaseManager.getSingleton().getConnection()
           .prepareStatement("SELECT * FROM Compound WHERE compoundID = " + rs.getInt("compoundID"));
       ResultSet rs2 = stmt2.executeQuery();
       rs2.next();
       int compoundID = rs2.getInt("compoundID");
       String name = rs2.getString("name");
       double inventory = rs2.getDouble("inventory");
       CompoundDTO a = new CompoundDTO(compoundID, name, inventory);
       compoundDTOs.add(a);

     }
     return compoundDTOs;

   } catch (SQLException | DatabaseException e) {
     // TODO Auto-generated catch block
     e.printStackTrace();
   }
   return null;
 }
}
