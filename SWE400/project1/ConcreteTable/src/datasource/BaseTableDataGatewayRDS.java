package datasource;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import datadto.BaseDTO;

public abstract class BaseTableDataGatewayRDS {
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
   * @return
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
   * @param rs the result set, contains a collection of bases.
   * @return a list of BaseDTOs.
   */
  private static List<BaseDTO> toDTOList(ResultSet rs) {
    List<BaseDTO> baseDTOs = new ArrayList<BaseDTO>();
    try {
      while (rs.next()) {
        int baseID = rs.getInt("baseID");
        String name = rs.getString("name");
        int solute = rs.getInt("solute");
        String soluteType = rs.getString("soluteType");
        double inventory = rs.getDouble("inventory");
        
        BaseDTO a = new BaseDTO(baseID, name, inventory, solute, soluteType);
        baseDTOs.add(a);
        
      }
      return baseDTOs;
    } catch (SQLException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return baseDTOs;
  }

  /**
   * Gets all Bases.
   * 
   * @return A list of all rows in Base.
   */
  public static List<BaseDTO> getAll() {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection().prepareStatement("SELECT * FROM Base;");
      stmt.execute();
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      new DatabaseException("could not get all");
    }
    return null;
  }

  /**
   * Filters the base table by a String.
   * 
   * @param wildCard String that is used in filter.
   * @return List of filtered BaseDTOs.
   */
  public static List<BaseDTO> filterByNameLike(String wildCard) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Base WHERE name LIKE '%" + wildCard + "%'");
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters the Base table by inventory.
   * 
   * @param inventory Given amount.
   * @return List of filtered BaseDTOs.
   */
  public static List<BaseDTO> filterByInventory(double inventory) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Base WHERE inventory = " + inventory);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters the Base table by a range of inventory.
   * 
   * @param min Lower limit for filter.
   * @param max Upper limit for filter.
   * @return List of filtered BaseDTOs.
   */
  public static List<BaseDTO> filterByInventoryBetween(double min, double max) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Base WHERE inventory BETWEEN " + min + " AND " + max);
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
   * @return List of filtered BaseDTOs.
   */
  public static List<BaseDTO> filterBySolute(int chemicalID) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Base WHERE solute = " + chemicalID);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
  
  public static List<BaseDTO> filterByLowInventory() {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Base WHERE Base.inventory <= " + lowInventory);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
}
