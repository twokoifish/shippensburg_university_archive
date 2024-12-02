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
 * Table data gateway used to acces the Compound table.
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class CompoundTDGRDS implements CompoundTDG {
  String sql = "SELECT * FROM Compound INNER JOIN Chemical WHERE Compound.CompoundId = Chemical.chemicalId ";

  private static CompoundTDGRDS singleton;

  /**
   * Singleton
   * 
   * @return
   */
  public static CompoundTDGRDS getSingleton() {
    if (singleton == null) {
      singleton = new CompoundTDGRDS();
    }
    return singleton;
  }

  /**
   * Get all compounds from the compound table
   */
  public CompoundTDGRDS getAllCompounds() {
    sql = "SELECT * FROM Compound INNER JOIN Chemical ";
    return getSingleton();
  }

  /**
   * Get all compounds with a similar name
   */
  public CompoundTDGRDS filterByName(String name) {
    System.out.println(sql);
    return getSingleton();
  }

  /**
   * Get all compounds with a specific inventory amount
   */
  public CompoundTDGRDS filterByInventory(double inventory) {
    sql += " AND (Chemical.inventory = " + inventory + ") ";
    return getSingleton();
  }

  /**
   * Get all compounds with a specific inventory range
   */
  public CompoundTDGRDS filterByInventoryRange(double high, double low) {
    sql += " AND (Chemical.inventory BETWEEN " + low + " AND " + high + ") ";
    return getSingleton();
  }

  /**
   * Get all compounds that hold a specific element
   */
  public CompoundTDGRDS filterByElements(int elementId) {
    sql += " AND Compound.elementId = " + elementId + ")";
    return getSingleton();
  }

  /** 
   * Get all compounds with a specific compound id.
   */
  public CompoundTDGRDS filterByCompoundId(int compoundId) {
    sql += "AND Compound.compoundId = " + compoundId + ") ";
    return getSingleton();
  }
  
  /**
   * Execute 'SQL' query and get its results. 
   */
  @Override
  public List<CompoundDTO> executeQuery() throws DatabaseException {
    List<CompoundDTO> listDTO = new ArrayList<>();
    sql+= ";";
    try {
      PreparedStatement statement = DatabaseManager.getSingleton().getConnection().prepareStatement(this.sql);

      ResultSet results = statement.executeQuery();

      while (results.next()) {
        listDTO.add(getDTO(results.getInt("compoundId")));
      }

    } catch (Exception e) {
      throw new DatabaseException("Failed to execute query.", e);
    }
    sql = "SELECT * FROM Compound INNER JOIN Chemical WHERE Compound.CompoundId = Chemical.chemicalId ";
    return listDTO;
  }
  
  /**
   * Convert an element id to a dto
   * 
   * @param id to get dto of
   * @return dto
   */
  private ElementDTO elementIdToDTO(int id) {
    ElementRDG element = new ElementRDGRDS(id);
    return element.getElement();
  }

  /**
   * Get compoundDTO from an id
   * 
   * @param id to get compoundDTO of
   * @return compoundDTO
   * @throws Exception
   */
  private CompoundDTO getDTO(int id) throws Exception {
    try {
      String sql = "SELECT * FROM Compound INNER JOIN Chemical WHERE Chemical.chemicalId"
          + " = Compound.compoundId AND Compound.compoundId = " + id + ";";
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(sql);

      // Get all elements connected to compound
      List<ElementDTO> elements = new ArrayList<>();
      String name = null;
      double inventory = -1 ;
      while (rs.next()) {
        // shhhh i know this is dumb.......
        name = rs.getString("name");
        inventory = rs.getDouble("inventory");
        
        elements.add(elementIdToDTO(rs.getInt("elementId")));
      }

      
      return (new CompoundDTO(id, elements, name, inventory));

    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      throw new Exception("Failed to read" + id, e);
    }
  }

}
