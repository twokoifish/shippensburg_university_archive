package datasource;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import datadto.ElementDTO;

public class ElementTableDataGatewayRDS {
  /**
   * Creates the table in the database. Drops the table if it already exists.
   * 
   * @throws DatabaseException
   */
  public static void createTable() throws DatabaseException {
    String drop = "DROP TABLE IF EXISTS Element";
    String create = "CREATE TABLE Element (" + "elementID INT NOT NULL AUTO_INCREMENT, " + "name VARCHAR(30) NOT NULL, "
        + "inventory Double, " + "atomicNumber INT NOT NULL, " + "atomicMass DOUBLE NOT NULL," + "UNIQUE(name),"
        + "PRIMARY KEY(elementID));";

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
      throw new DatabaseException("Unable to drop Element table", e);
    }
  }

  private static double lowInventory = 20.0;

  private static List<ElementDTO> toDTOList(ResultSet rs) {
    List<ElementDTO> elementDTOs = new ArrayList<ElementDTO>();
    try {
      while (rs.next()) {
        int elementID = rs.getInt("elementID");
        String name = rs.getString("name");
        double inventory = rs.getDouble("inventory");
        int atomicNumber = rs.getInt("atomicNumber");
        double atomicMass = rs.getDouble("atomicMass");
        ElementDTO a = new ElementDTO(elementID, name, inventory, atomicNumber, atomicMass);
        elementDTOs.add(a);
      }
      return elementDTOs;
    } catch (SQLException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return elementDTOs;
  }

  /**
   * Gets all Elements.
   * 
   * @return A list of all rows in Element.
   */
  public static List<ElementDTO> getAll() {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Element;");
      stmt.execute();
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      new DatabaseException("could not get all");
    }
    return null;
  }

  /**
   * Filters the element table by a String.
   * 
   * @param nameLike String that is used in filter.
   * @return List of filtered ElementDTOs.
   */
  public static List<ElementDTO> filterByNameLike(String nameLike) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Element WHERE name LIKE '%" + nameLike + "%'");
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
  public static List<ElementDTO> filterByInventory(double inventory) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Element WHERE inventory = " + inventory);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Filters the element table by a range of inventory.
   * 
   * @param min Lower limit for filter.
   * @param max Upper limit for filter.
   * @return List of filtered ElementDTOs.
   */
  public static List<ElementDTO> filterByInventoryBetween(double min, double max) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Element WHERE inventory BETWEEN " + min + " AND " + max);
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
   * @return List of filtered ElementDTOs.
   */
  public static List<ElementDTO> filterByAtomicNumber(int atomicNumber) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Element WHERE atomicNumber = " + atomicNumber);
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
   * @return List of filtered ElementDTOs.
   */
  public static List<ElementDTO> filterByAtomicNumberBetween(int min, int max) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Element WHERE atomicNumber BETWEEN " + min + " AND " + max);
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
   * @return List of filtered ElementDTOs.
   */
  public static List<ElementDTO> filterByAtomicMass(double atomicMass) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Element WHERE atomicMass = " + atomicMass);
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
   * @return List of filtered ElementDTOs.
   */
  public static List<ElementDTO> filterByAtomicMassBetween(double min, double max) {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Element WHERE atomicMass BETWEEN " + min + " AND " + max);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  public static List<ElementDTO> filterByPartOfCompound(int compoundID) {
    List<ElementDTO> elementDTOs = new ArrayList<ElementDTO>();
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT elementID FROM CompoundMadeOf WHERE compoundID = " + compoundID);

      ResultSet rs = stmt.executeQuery();
      elementDTOs = new ArrayList<ElementDTO>();
      while (rs.next()) {
        PreparedStatement stmt2 = DatabaseManager.getSingleton().getConnection()
            .prepareStatement("SELECT * FROM Element WHERE elementID = " + rs.getInt("elementID"));
        ResultSet rs2 = stmt2.executeQuery();
        rs2.next();
        int elementID = rs2.getInt("elementID");
        String name = rs2.getString("name");
        double inventory = rs2.getDouble("inventory");
        int atomicNumber = rs2.getInt("atomicNumber");
        double atomicMass = rs2.getDouble("atomicMass");
        ElementDTO a = new ElementDTO(elementID, name, inventory, atomicNumber, atomicMass);
        elementDTOs.add(a);

      }
      return elementDTOs;

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return elementDTOs;
  }

  public static List<ElementDTO> filterByLowInventory() {
    try {
      PreparedStatement stmt = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Element WHERE Element.inventory <= " + lowInventory);
      ResultSet rs = stmt.executeQuery();
      return toDTOList(rs);

    } catch (SQLException | DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }
}
