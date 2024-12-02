package datasource;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import database.DatabaseException;
import database.DatabaseManager;

/**
 * MetalRDGRDS used to access a row of the Metal table.
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class MetalRDGRDS implements MetalRDG {
  MetalDTO metal;

  /**
   * Empty constructor
   */
  public MetalRDGRDS() {
    
  }
  
  /**
   * Constructor to find an existing metal
   * 
   * @param id to find
   */
  public MetalRDGRDS(int id) {
    String sql = "SELECT * FROM Metal INNER JOIN Chemical ON chemicalId = Metal.metalId  INNER JOIN Element ON elementId = Metal.metalId AND Metal.metalId = "
        + id + ";";
    try {
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(sql);
      rs.next();

      metal = new MetalDTO(id, rs.getInt("dissolvedBy"), rs.getInt("atomicNumber"), rs.getDouble("atomicMass"),
          rs.getDouble("moles"), rs.getString("name"), rs.getDouble("inventory"));

    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("No entry with id " + id);
    }
  }

  /**
   * Constructor to create a metal
   * 
   * @param id            int
   * @param dissolvedById int
   * @param atomicNumber  int
   * @param atomicMass    double
   * @param moles         double
   * @param name          String
   * @param inventory     double
   */
  public MetalRDGRDS(int dissolvedById, int atomicNumber, double atomicMass, double moles, String name,
      double inventory) {

    try {
      PreparedStatement insertChemical = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("INSERT INTO Chemical (name, inventory)" + "VALUES (?, ?);");
      insertChemical.setString(1, name);
      insertChemical.setDouble(2, inventory);

      String fetchId = ("SELECT LAST_INSERT_ID();");
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(fetchId);
      rs.next();
      int id = rs.getInt("LAST_INSERT_ID()");

      PreparedStatement insertElement = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("INSERT INTO Element (elementId, atomicNumber, atomicMass)" + "VALUES (?, ?, ?);");
      insertElement.setInt(1, id);
      insertElement.setInt(2, atomicNumber);
      insertElement.setDouble(3, atomicMass);

      PreparedStatement insert = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("INSERT INTO Metal (metalId, dissolvedBy, moles)" + "VALUES (?, ?, ?);");
      insert.setInt(1, id);
      insert.setInt(2, dissolvedById);
      insert.setDouble(3, moles);

      insertChemical.execute();
      insertElement.execute();
      insert.execute();

      metal = new MetalDTO(id, dissolvedById, atomicNumber, atomicMass, moles, name, inventory);
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Failed to insert");
    }
  }

  /**
   * Delete a metal from the table.
   */
  @Override
  public void delete() {
    String sqlMetal = "DELETE FROM Metal WHERE metalId = " + metal.getMetalId() + ";";
    String sqlChem = "DELETE FROM Chemical WHERE chemicalId = " + metal.getMetalId() + ";";
    String sqlElement = "DELETE FROM Element WHERE elementId = " + metal.getMetalId() + ";";
    try {

      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      statement.executeUpdate(sqlMetal);
      statement.executeUpdate(sqlChem);
      statement.executeUpdate(sqlElement);

      metal = null;
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Problem deleting Metal with id " + metal.getMetalId());
    }

  }

  /**
   * Updates metal with current values in metalDTO.
   */
  @Override
  public void update() {
    try {
      PreparedStatement updateMetal = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("UPDATE Metal SET dissolvedById = ?, moles = ? WHERE elementId = ?;");
      updateMetal.setInt(1, metal.getDissolvedById());
      updateMetal.setDouble(2, metal.getMoles());
      updateMetal.setInt(3, metal.getMetalId());

      PreparedStatement updateElement = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("UPDATE Element SET atomicNumber = ?, atomicMass = ? WHERE elementId = ?;");
      updateMetal.setInt(1, metal.getAtomicNumber());
      updateMetal.setDouble(2, metal.getAtomicMass());
      updateMetal.setInt(3, metal.getMetalId());

      PreparedStatement updateChemical = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("UPDATE Chemical SET name = ?, inventory = ? WHERE chemicalId = ?;");
      updateChemical.setString(1, metal.getName());
      updateChemical.setDouble(2, metal.getInventory());
      updateChemical.setInt(3, metal.getMetalId());

      updateMetal.execute();
      updateElement.execute();
      updateChemical.execute();
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Failed to update");
    }
  }

  /**
   * Finds all the metals dissolved by an Acid
   * 
   * @param dissolvedById an Acid
   * @return list of MetalRowDataGatewayRDS that contain the metals dissolved by
   *         the given acid
   */
  public List<MetalRDGRDS> findSet(int dissolvedById) {
    List<MetalRDGRDS> results = new ArrayList<>();
    try {
      String sql = "SELECT * FROM Metal WHERE dissolvedBy = " + dissolvedById + ";";
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(sql);
      while (rs.next()) {
        MetalRDGRDS metalRDS = new MetalRDGRDS(rs.getInt("metalId"));
        results.add(metalRDS);
      }
    } catch (SQLException | DatabaseException e) {

    }
    return results;
  }

  /**
   * Set dissolvedById
   * 
   * @param dissolvedById
   */
  public void setDissolvedById(int dissolvedById) {
    metal.setDissolvedById(dissolvedById);
  }

  /**
   * Set name
   * 
   * @param name
   */
  public void setName(String name) {
    metal.setName(name);
  }

  /**
   * Set inventory
   * 
   * @param inventory
   */
  public void setInventory(double inventory) {
    metal.setInventory(inventory);
  }

  /**
   * Set moles
   * 
   * @param moles
   */
  public void setMoles(double moles) {
    metal.setMoles(moles);
  }

  /**
   * Set atomic number
   * 
   * @param atomicNumber
   */
  public void setAtomicNumber(int atomicNumber) {
    metal.setAtomicNumber(atomicNumber);
  }

  /**
   * Set atomic mass
   * 
   * @param atomicMass
   */
  public void setAtomicMass(double atomicMass) {
    metal.setAtomicMass(atomicMass);
  }

  /**
   * Get metalDTO
   * 
   * @return MetalDTO
   */
  public MetalDTO getMetal() {
    return metal;
  }
}
