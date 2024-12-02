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
 * BaseRDGRDS used to access rows in the Base table.
 * 
 * @author Isabella Boone, Kim O'Neill
 */
public class BaseRDGRDS implements BaseRDG {
  BaseDTO base;

  /**
   * Empty constructor
   */
  public BaseRDGRDS() {
    
  }
  
  /**
   * Constructor BaseRowDataGateway, search for existing Base via id
   * 
   * @param id
   */
  public BaseRDGRDS(int id) throws SQLException, DatabaseException {
    String select = "SELECT * FROM Base INNER JOIN Chemical WHERE Base.baseId = Chemical.chemicalId AND Chemical.chemicalId = "
        + id + ";";
    
    // Get acid information
    Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
    ResultSet rs = statement.executeQuery(select);
    rs.next(); // Get result

    base = new BaseDTO(id, rs.getInt("solute"), rs.getString("name"), rs.getDouble("inventory"), rs.getInt("soluteType"));

  }

  /**
   * AcidRowDataGateway constructor for creating a new base
   * 
   * @param id
   * @param solute
   * @param name
   * @param inhabits
   */
  public BaseRDGRDS(int solute, String name, double inventory, int soluteType) {

    try {
      // Insert chemical
      PreparedStatement insertChemical = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("INSERT INTO Chemical (name, inventory, type)" + "VALUES (?, ?, ?);");
      insertChemical.setString(1, name);
      insertChemical.setDouble(2, inventory);

      // Insert Base
      PreparedStatement insertBase = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("INSERT INTO Base (baseId, solute)" + "VALUES (LAST_INSERT_ID(), ?);");
      if(solute > 0 ) {
        insertBase.setInt(1, solute); // set solute id
        insertChemical.setInt(3, soluteType); // set solute id
      } else {
        insertBase.setInt(1, java.sql.Types.INTEGER);
        insertChemical.setInt(3, java.sql.Types.INTEGER);
      }
      insertChemical.execute(); // Insert chemical
      insertBase.execute(); // Insert base

      String fetchId = ("SELECT LAST_INSERT_ID();");
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(fetchId);
      rs.next();

      base = new BaseDTO(rs.getInt("LAST_INSERT_ID()"), solute, name, inventory, soluteType);
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Failed to insert base through constructor");
    }
  }

  /**
   * Delete a base from both chemical and base tables.
   */
  public void delete() {
    String deleteChemical = "DELETE FROM Chemical WHERE ChemicalId = " + base.getBaseId() + ";",
        deleteBase = "DELETE FROM Base WHERE baseId = " + base.getBaseId() + ";";

    try {
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();

      statement.executeUpdate(deleteBase);
      statement.executeUpdate(deleteChemical);

      base = null;
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Error deleting base " + base.getBaseId());
    }
  }

  /**
   * Update the database.
   */
  public void update() {
    String updateChemicalSQL = "UPDATE Chemical SET chemicalId = ?, name = ?, inventory = ? WHERE chemicalID = "
        + base.getBaseId() + ";",
        updateBaseSQL = "UPDATE Base SET baseId = ?, solute = ?, soluteType = ? WHERE baseId = " + base.getBaseId() + ";";

    try {
      // Chemical
      PreparedStatement chem = DatabaseManager.getSingleton().getConnection().prepareStatement(updateChemicalSQL);
      chem.setInt(1, base.getBaseId());
      chem.setString(2, base.getName());
      chem.setDouble(3, base.getInventory());

      chem.execute();

      // Base
      PreparedStatement acid = DatabaseManager.getSingleton().getConnection().prepareStatement(updateBaseSQL);
      acid.setInt(1, base.getBaseId());
      acid.setInt(2, base.getSoluteId());
      acid.setInt(3, base.getSoluteType());

      acid.execute();

    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Failed to update base");
    }
  }

  /**
   * Find all bases neutralized by a specific solute id.
   */
  public List<BaseRDGRDS> findSet(int solute) {
    List<BaseRDGRDS> results = new ArrayList<>();

    try {
      String sql = "SELECT * FROM Base WHERE solute = " + solute + ";";
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(sql);

      while (rs.next()) {
        int sol = rs.getInt("baseId");
        BaseRDGRDS id = new BaseRDGRDS(sol);
        results.add(id);
      }

    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
    }
    return results;
  }

  /**
   * Set solute.
   */
  @Override
  public void setSolute(int newSolute) {
    base.setSoluteId(newSolute);
  }

  /**
   * Set name.
   */
  @Override
  public void setName(String newName) {
    base.setName(newName);
  }

  /**
   * Set inhabits.
   */
  @Override
  public void setInventory(double inventory) {
    base.setInventory(inventory);
  }

  public BaseDTO getBase() {
    return base;
  }

  @Override
  public void setSoluteType(int soluteType) {
    base.setSoluteType(soluteType);
  }

}
