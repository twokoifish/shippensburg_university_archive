package datasource;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import database.DatabaseException;
import database.DatabaseManager;

/**
 * MetalTDGRDS used to acces the Metal table.
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class MetalTDGRDS implements MetalTDG {
  String sql = "SELECT * FROM Metal INNER JOIN Chemical ON Metal.metalId = Chemical.chemicalId "
      + "INNER JOIN Element ON Element.elementId = Metal.metalId ";
  private static MetalTDGRDS singleton;

  /**
   * Get singleton
   * @return singleton
   */
  public static MetalTDGRDS getSingleton() {
    if (singleton == null) {
      singleton = new MetalTDGRDS();
    }
    return singleton;
  }

  /**
   * Execute the SQL Query
   * @return list of metal DTOs from query
   * @throws DatabaseException
   */
  public List<MetalDTO> executeQuery() throws DatabaseException {
    List<MetalDTO> listDTO = new ArrayList<>();
    try {
      PreparedStatement statement = DatabaseManager.getSingleton().getConnection().prepareStatement(this.sql + ";");
      ResultSet results = statement.executeQuery();

      while (results.next()) {
        int id = results.getInt("metalId");
        int dissolvedBy = results.getInt("dissolvedBy");
        int atomicNum = results.getInt("atomicNumber");
        double moles = results.getDouble("moles");
        double atomicMass = results.getDouble("atomicMass");
        String name = results.getString("name");
        double inventory = results.getDouble("inventory");
        MetalDTO metal = new MetalDTO(id, dissolvedBy, atomicNum, atomicMass, moles, name, inventory);
        listDTO.add(metal);
      }
    } catch (SQLException e) {
      throw new DatabaseException("Failed to execute query.", e);
    }

    sql = "SELECT * FROM Metal INNER JOIN Chemical ON Metal.metalId = Chemical.chemicalId "
        + "INNER JOIN Element ON Element.elementId = Metal.metalId ";

    return listDTO;
  }

  /**
   * Get all metals
   */
  public MetalTDGRDS getAllMetals() {
    sql = "SELECT * FROM Metal INNER JOIN Chemical ON Metal.metalId = Chemical.chemicalId "
        + "INNER JOIN Element ON Element.elementId = Metal.metalId ";
    return getSingleton();
  }

  /**
   * Get all metals with a similar name
   */
  public MetalTDGRDS filterByName(String name) {
    sql += " AND (Chemical.name LIKE '%" + name + "%')";
    return getSingleton();
  }

  /**
   * Get all metals with a specific inventory amount
   */
  public MetalTDGRDS filterByInventory(double inventory) {
    sql += " AND (Chemical.inventory = " + inventory + ")";
    return getSingleton();
  }

  /**
   * Get all metals with a specific inventory range
   */
  public MetalTDGRDS filterByInventoryRange(double high, double low) {
    sql += " AND (Chemical.inventory BETWEEN " + low + " AND " + high + ")";
    return getSingleton();
  }

  /**
   * Get all metals with a specific atomic mass
   */
  public MetalTDGRDS filterByAtomicMass(double atomicMass) {
    sql += " AND (Element.atomicMass = " + atomicMass + ")";
    return getSingleton();
  }

  /**
   * Get all metals with a specific atomic mass range
   */
  public MetalTDGRDS filterByAtomicMassRange(double high, double low) {
    sql += " AND (Element.atomicMass BETWEEN " + low + " AND " + high + ")";
    return getSingleton();
  }

  /**
   * Get all metals with a specific atomic number
   */
  public MetalTDGRDS filterByAtomicNumber(int atomicNumber) {
    sql += " AND (Element.atomicNumber = " + atomicNumber + ")";
    return getSingleton();
  }

  /**
   * Get all metals with a specific atomic number range
   */
  public MetalTDGRDS filterByAtomicNumberRange(int high, int low) {
    sql += " AND (Element.atomicNumber BETWEEN '" + low + "' AND '" + high + "')";
    return getSingleton();
  }

  /**
   * Get all metals that are dissolved by a specific acid id
   */
  public MetalTDGRDS filterByDissolvedBy(int dissolvedBy) {
    sql += " AND (Metal.dissolvedBy = " + dissolvedBy + ")";
    return getSingleton();
  }

  /**
   * Get all metals that have a specific num of moles
   */
  public MetalTDGRDS filterByMoles(double moles) {
    sql += " AND (Metal.moles = " + moles + ")";
    return getSingleton();
  }

  /***
   * Get all metals that have a specific number of moles range
   */
  public MetalTDGRDS filterByMolesRange(double high, double low) {
    sql += " AND (Metal.moles BETWEEN " + low + " AND " + high + ")";
    return getSingleton();
  }

}
