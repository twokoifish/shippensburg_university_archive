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
 * Table data gateway for accessing Chemical table.
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class ChemicalTDGRDS implements ChemicalTDG {
  String sql = "SELECT * FROM Chemical";
  
  private static ChemicalTDGRDS singleton;

  public ChemicalTDGRDS() {
    sql = "SELECT * FROM Chemical";
  }

  public static ChemicalTDGRDS getSingleton() {
    if (singleton == null) {
      singleton = new ChemicalTDGRDS();
    }
    return singleton;
  }

  public ChemicalTDGRDS filterByName(String name) {
    if(!(sql.contains("WHERE"))) {
      sql += " WHERE (name LIKE '%" + name + "%')";
    } else {
      sql += " AND (name LIKE '%" + name + "%')";
    }
    
    return getSingleton();
  }

  @Override
  public ChemicalTDGRDS filterByInventory(double inventory) {
    if(!(sql.contains("WHERE"))) {
      sql += " WHERE (inventory = '" + inventory + "')";
    } else {
      sql += " AND (inventory = '" + inventory + "')";
    }

    return getSingleton();
  }

  @Override
  public ChemicalTDGRDS filterByInventoryRange(double high, double low) {
    if(!(sql.contains("WHERE"))) {
      sql += " WHERE (inventory BETWEEN " + low + " AND " + high + ")";
    } else {
      sql += " AND (inventory BETWEEN " + low + " AND " + high + ")";
    }

    return getSingleton();
  }

  @Override
  public List<ChemicalDTO> executeQuery() throws DatabaseException {
    List<ChemicalDTO> listDTO = new ArrayList<>();
    try {
      PreparedStatement statement = DatabaseManager.getSingleton().getConnection().prepareStatement(this.sql + ";");
      try {
        ResultSet results = statement.executeQuery();

        while (results.next()) {
          
          int baseId = results.getInt("chemicalId");
          String name = results.getString("name");
          double inventory = results.getDouble("inventory");
          ChemicalDTO base = new ChemicalDTO(baseId, name, inventory);
          listDTO.add(base);
        }
        
        sql = "SELECT * FROM Chemical";
      } catch (SQLException e) {
        throw new DatabaseException("Failed to convert query to DTO.", e);
      }
    } catch (SQLException e) {
      throw new DatabaseException("Failed to execute query.", e);
    }
    return listDTO;
  }

  @Override
  public ChemicalTDGRDS getAllChemicals() {
    sql = "SELECT * FROM Chemical";
    return getSingleton();
  }

  public static void delete(int i) {
    String deleteChemical = "DELETE FROM Chemical WHERE ChemicalId = " + i + ";";
    
    try {
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      
      statement.executeUpdate(deleteChemical);
      
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Error deleting chemical " + i);
    }
  }
  
  public static void create(int id, String name, double inventory) {
    try {
      PreparedStatement insertChemical = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("INSERT INTO Chemical (chemicalId, name, inventory) VALUES (?,?,?);");

      insertChemical.setInt(1, id);
      insertChemical.setString(2, name);
      insertChemical.setDouble(3, inventory);

      insertChemical.execute();

    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  public ChemicalTDGRDS filterLowInventory(double inventoryNeeded) {
    try {
      PreparedStatement insertChemical = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("SELECT * FROM Chemical WHERE inventory < " + inventoryNeeded + ";");

      insertChemical.execute();

    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
    }
    return getSingleton();
  }

}
