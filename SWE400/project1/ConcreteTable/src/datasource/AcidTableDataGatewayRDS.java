package datasource;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import datadto.AcidDTO;

/**
 * Table Data Gateway for the Acid table
 * 
 * @author jeol
 *
 */
public abstract class AcidTableDataGatewayRDS {
  // TODO maybe make singleton.

  /**
   * Creates table in database.
   * 
   * @throws DatabaseException
   */
  public static void createTable() throws DatabaseException {
    String drop = "DROP TABLE IF EXISTS Acid";
    String create = "CREATE TABLE Acid (" + "acidID INT NOT NULL AUTO_INCREMENT, " + "name VARCHAR(30) NOT NULL, "
        + "inventory Double, " + "solute INT, " + "UNIQUE(name), " + "PRIMARY KEY(acidID) );";

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
      throw new DatabaseException("Unable to create Acid table", e);
    }
  }

  /**
   * Only drop the table.
   * 
   * @throws DatabaseException
   */
  public static void dropTable() throws DatabaseException {
    String drop = "DROP TABLE IF EXISTS Acid";
    try {
      // drop table
      PreparedStatement stmt;
      stmt = DatabaseManager.getSingleton().getConnection().prepareStatement(drop);
      stmt.execute();
      stmt.close();
    } catch (SQLException e) {
      throw new DatabaseException("Unable to drop Acid table", e);
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
  private static List<AcidDTO> toDTOList(ResultSet rs) {
    List<AcidDTO> acidDTOs = new ArrayList<AcidDTO>();
    try {
      while (rs.next()) {
        int acidID = rs.getInt("acidID");
        String name = rs.getString("name");
        int solute = rs.getInt("solute");
        double inventory = rs.getDouble("inventory");
        String soluteType = rs.getString("soluteType");
        AcidDTO a = new AcidDTO(acidID, name, inventory, solute, soluteType);
        acidDTOs.add(a);
       
      }
      return acidDTOs;
    } catch (SQLException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return acidDTOs;
  }

  /**
   * Gets all Acids.
   * 
   * @return A list of all rows in Acid.
   */
  public static List<AcidDTO> getAll() {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection().prepareStatement("SELECT * FROM Acid;");
      stmt.execute();
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      new DatabaseException("could not get all");
    }
    return null;
  }

  /**
   * Filters the acid table by a String.
   * 
   * @param wildCard String that is used in filter.
   * @return List of filtered AcidDTOs.
   */
  public static List<AcidDTO> filterByNameLike(String wildCard) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Acid WHERE name LIKE '%" + wildCard + "%'");
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters the acid table by inventory.
   * 
   * @param inventory Given amount.
   * @return List of filtered AcidDTOs.
   */
  public static List<AcidDTO> filterByInventory(double inventory) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Acid WHERE inventory = " + inventory);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
  
  /**
   * Filters the acid table by inventory.
   * 
   * @param inventory Given amount.
   * @return List of filtered AcidDTOs.
   */
  public static List<AcidDTO> filterByLowInventory() {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Acid WHERE inventory <= " + lowInventory);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters the acid table by a range of inventory.
   * 
   * @param min Lower limit for filter.
   * @param max Upper limit for filter.
   * @return List of filtered AcidDTOs.
   */
  public static List<AcidDTO> filterByInventoryBetween(double min, double max) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Acid WHERE inventory BETWEEN " + min + " AND " + max);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters by a given solute.
   * 
   * @param chemicalID ID of the solute.
   * @return List of filtered AcidDTOs.
   */
  public static List<AcidDTO> filterBySolute(int chemicalID) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Acid WHERE solute = " + chemicalID);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
}
