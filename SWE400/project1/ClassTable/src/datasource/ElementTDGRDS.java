package datasource;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import database.DatabaseException;
import database.DatabaseManager;

/**
 * ElementTDG used to acces the Element table.
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class ElementTDGRDS implements ElementTDG {
  String sql = "SELECT * FROM Element INNER JOIN Chemical WHERE (Element.elementId = Chemical.chemicalId)";
  private static ElementTDGRDS singleton;

  /**
   * Get singleton instance
   * 
   * @return singleton
   */
  public static ElementTDGRDS getSingleton() {
    if (singleton == null) {
      singleton = new ElementTDGRDS();
    }
    return singleton;
  }

  /**
   * Get all elements in the table
   */
  public ElementTDGRDS getAllElements() {
    sql = "SELECT * FROM Element INNER JOIN Chemical WHERE (Element.elementId = Chemical.chemicalId)";
    return getSingleton();
  }

  /**
   * Get all elements with a similar name
   */
  public ElementTDGRDS filterByName(String name) {
    sql += " AND (Chemical.name LIKE '%" + name + "%') ";
    return getSingleton();
  }

  /**
   * Get all elements with a specific inventory amount
   */
  public ElementTDGRDS filterByInventory(double inventory) {
    sql += " AND (Chemical.inventory = " + inventory + ")";
    return getSingleton();
  }

  /**
   * Get all elements with a specific inventory range
   */
  public ElementTDGRDS filterByInventoryRange(double high, double low) {
    sql += " AND (Chemical.inventory BETWEEN " + low + " AND " + high + ")";
    return getSingleton();
  }

  /**
   * Get all elements with a specific atomic mass
   */
  public ElementTDGRDS filterByAtomicMass(double atomicMass) {
    sql += " AND (Element.atomicMass = " + atomicMass + ")";
    return getSingleton();
  }

  /**
   * Get all elements with a specific atomic mass range
   */
  public ElementTDGRDS filterByAtomicMassRange(double high, double low) {
    sql += " AND (Element.atomicMass BETWEEN " + low + " AND " + high + ")";
    return getSingleton();
  }

  /**
   * Get all elements with a specific atomic number
   */
  public ElementTDGRDS filterByAtomicNumber(int atomicNumber) {
    sql += " AND (Element.atomicNumber = " + atomicNumber + ")";
    return getSingleton();
  }

  /**
   * Get all elements with a specific atomic number range
   */
  public ElementTDGRDS filterByAtomicNumberRange(int high, int low) {
    sql += " AND (Element.atomicNumber BETWEEN " + low + " AND " + high + ")";
    return getSingleton();
  }

  /**
   * Execute SQL query and put results into dtos.
   * 
   * @return List of element DTOs that meet the SQL query's specifications
   * @throws DatabaseException when things go wrong (Syntax) 
   */
  public List<ElementDTO> executeQuery() throws DatabaseException {
    List<ElementDTO> listDTO = new ArrayList<>();
    try {
      PreparedStatement statement = DatabaseManager.getSingleton().getConnection().prepareStatement(this.sql + ";");
      ResultSet results = statement.executeQuery();

      while (results.next()) {
        int id = results.getInt("chemicalId");
        int atomicNum = results.getInt("atomicNumber");
        double atomicMass = results.getDouble("atomicMass");
        String name = results.getString("name");
        double inventory = results.getDouble("inventory");
        ElementDTO element = new ElementDTO(id, atomicNum, atomicMass, name, inventory);
        listDTO.add(element);
      }
    } catch (SQLException e) {
      throw new DatabaseException("Failed to execute query.", e);
    }
    sql = "SELECT * FROM Element INNER JOIN Chemical WHERE (Element.elementId = Chemical.chemicalId)";
    return listDTO;
  }

}
