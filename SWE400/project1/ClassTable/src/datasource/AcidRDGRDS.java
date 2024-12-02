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
 * AcidRowDataGatewayRDS used to access rows in the Acid table.
 * 
 * @author Isabella Boone, Kim O'Neill
 */
public class AcidRDGRDS implements AcidRDG {
  private AcidDTO acid;
  
  /**
   * Empty constructor
   */
  public AcidRDGRDS() {
    
  }
  
  /**
   * Constructor used to find an existing Acid.
   * @param id of the acid to find
   */
  public AcidRDGRDS(int id) throws SQLException, DatabaseException {
    // Statements to find existing acid/chemical and collect their information.
    String select = "SELECT * FROM Acid INNER JOIN Chemical WHERE chemicalId = acidId AND acidId = " + id + ";";

    // Get acid information
    Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
    ResultSet rs = statement.executeQuery(select);
    rs.next(); // Get result
    if(rs.getInt("solute") < 0) {
      acid = new AcidDTO(id, -1, rs.getString("name"), rs.getDouble("inventory"), -1);
    } else {
      acid = new AcidDTO(id, rs.getInt("solute"), rs.getString("name"), rs.getDouble("inventory"), rs.getInt("type"));
    }
    
  }
  
  /**
   * AcidRowDataGateway constructor for creating a new acid.
   * @param id of acid to insert
   * @param solute of acid to insert
   * @param name of acid to insert
   * @param solutetype of acid to insert
   */
  public AcidRDGRDS(int solute, String name, double inventory, int soluteType) {
    
    try {
      // Insert chemical 
      PreparedStatement insertChemical = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("INSERT INTO Chemical (name, inventory, type) VALUES (?, ?, ?);");
      insertChemical.setString(1, name);
      insertChemical.setDouble(2, inventory);
      
      // Insert Acid
      PreparedStatement insertAcid = DatabaseManager.getSingleton().getConnection()
        .prepareStatement("INSERT INTO Acid (acidId, solute) VALUES (LAST_INSERT_ID(), ?);");
      if(solute > 0) {
        insertAcid.setInt(1, solute); // set solute id
        insertChemical.setInt(3, soluteType); // solute type
      } else {
        insertAcid.setInt(1, java.sql.Types.INTEGER);
        insertChemical.setInt(3, java.sql.Types.INTEGER);
      }
      
      insertChemical.execute(); // Insert chemical
      insertAcid.execute();  // Insert acid
      
      String fetchId = ("SELECT LAST_INSERT_ID();");
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(fetchId);
      rs.next();
      
      acid = new AcidDTO(rs.getInt("LAST_INSERT_ID()"), solute, name, inventory, soluteType);
      
    } catch(SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Failed to insert acid through constructor");
    }
  }
	
	/**
	 * Delete an acid from both acid and chemical tables
	 */
  @Override
  public void delete() {
    String deleteChemical = "DELETE FROM Chemical WHERE ChemicalId = " + acid.getAcidId() + ";",
        deleteAcid = "DELETE FROM Acid WHERE AcidId = " + acid.getAcidId() + ";";
    
    try {
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      
      statement.executeUpdate("SET FOREIGN_KEY_CHECKS = 0;");
      statement.executeUpdate(deleteAcid);
      statement.executeUpdate(deleteChemical);
      statement.executeUpdate("SET FOREIGN_KEY_CHECKS = 1;");
      
      acid = null;
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Error deleting acid " + acid.getAcidId());
    }
    
    acid = null;
  }

  /**
   * Update the database.
   */
  @Override
  public void update() {
    String updateChemicalSQL = "UPDATE Chemical SET chemicalId = ?, name = ?, inventory = ? WHERE chemicalID = " + acid.getAcidId() + ";";
    String updateAcidSQL = "UPDATE Acid SET acidId = ?, solute = ?, soluteType = ? WHERE acidId = " + acid.getAcidId() + ";";
    
    try {
      // Chemical
      PreparedStatement chem = DatabaseManager.getSingleton().getConnection().prepareStatement(updateChemicalSQL);
      chem.setInt(1, acid.getAcidId());
      chem.setString(2, acid.getName());
      chem.setDouble(3, acid.getInventory());

      chem.execute();
      
      // Acid
      PreparedStatement acid = DatabaseManager.getSingleton().getConnection().prepareStatement(updateAcidSQL);
      acid.setInt(1, this.acid.getAcidId());
      acid.setInt(2, this.acid.getSoluteId());
      acid.setInt(3, this.acid.getSoluteType());
      
      acid.execute();
      
      
    } catch(SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Failed to update acid");
    }

  }
  
  /**
   * Find a set of ids that are dissolved by the same solute
   * @param solute
   * @return
   */
  public List<AcidRDGRDS> findSet(int solute) {
    List<AcidRDGRDS> results = new ArrayList<>();
    try {
      String sql = "SELECT * FROM Acid WHERE solute = "+ solute + ";";
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(sql);
      
      while(rs.next()) {
        int sol = rs.getInt("acidId");
        AcidRDGRDS id = new AcidRDGRDS(sol);
        results.add(id);
      }
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();

    }
    return results;
  }

  /**
   * Set solute
   */
  @Override
  public void setSolute(int newSolute) {
    acid.setSoluteId(newSolute);
  }
  
  public void setSoluteType(int soluteType) {
    acid.setSoluteType(soluteType);
  }

  /**
   * Set name
   */
  @Override
  public void setName(String newName) {
    acid.setName(newName);
  }

  /**
   * Set inhabits
   */
  @Override
  public void setInventory(double inventory) {
    acid.setInventory(inventory);
  }
  
  /**
   * Get acid
   * @return AcidDTO acid 
   */
  public AcidDTO getAcid() {
    return acid;
  }

}
