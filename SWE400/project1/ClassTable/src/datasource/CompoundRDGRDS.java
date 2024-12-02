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
 * CompoundRDGRDS, used to access rows of the compound table.
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class CompoundRDGRDS implements CompoundRDG {
  CompoundDTO compound;

  /**
   * Empty constructor
   */
  public CompoundRDGRDS() {
    
  }
  
  /**
   * Initialize the RDG by reading in a compound.
   * 
   * @param id to read
   */
  public CompoundRDGRDS(int id) {
    try {
      compound = read(id);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Initialize the RDG by creating a compound.
   * 
   * @param madeOf    List of elements that the compound is made of.
   * @param name      of the compound.
   * @param inventory of the compound
   */
  public CompoundRDGRDS(List<Integer> madeOf, String name, double inventory) {
    try {
      compound = create(madeOf, name, inventory);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
  
  /**
   * Function to insert a compound into the database with its raw values.
   * 
   * @param madeOf    list of element ids (ints) that the compound is made of
   * @param name      of the compound
   * @param inventory of the compound
   * @return CompoundDTO of the compound inserted into the database.
   */
  public CompoundDTO create(List<Integer> madeOf, String name, double inventory) throws Exception {
    try {
      List<ElementDTO> elements = new ArrayList<>();
      // Insert compound
      PreparedStatement insertChemical = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("INSERT INTO Chemical (name, inventory) VALUES (?, ?);");
      insertChemical.setString(1, name);
      insertChemical.setDouble(2, inventory);

      insertChemical.execute();

      String fetchId = ("SELECT LAST_INSERT_ID();");
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(fetchId);
      rs.next();

      int id = rs.getInt("LAST_INSERT_ID()");

      // Insert all compounds
      for (int i = 0; i < madeOf.size(); i++) {
        PreparedStatement insert = DatabaseManager.getSingleton().getConnection()
            .prepareStatement("INSERT INTO Compound(compoundId, elementId) VALUES (?, ?);");

        insert.setInt(1, id);
        insert.setInt(2, madeOf.get(i));

        insert.execute();

        // List of elements
        elements.add(elementIdToDTO(madeOf.get(i)));
      }

      compound = (new CompoundDTO(id, elements, name, inventory));

      return compound;

    } catch (SQLException | DatabaseException e) {
      throw new Exception("CompoundRDGRDS", e);
    }
  }

  /**
   * Insert a compound into the database with a compoundDTO.
   * 
   * @param compound to insert
   * @throws Exception when things go wrong.
   */
  public void create(CompoundDTO compound) throws Exception {
    try {
      // Insert compound
      PreparedStatement insertChemical = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("INSERT INTO Chemical (chemicalId, name, inventory) VALUES (?, ?, ?);");
      insertChemical.setInt(1, compound.getCompoundId());
      insertChemical.setString(2, compound.getName());
      insertChemical.setDouble(3, compound.getInventory());

      insertChemical.execute();

      // Insert all compounds
      for (int i = 0; i < compound.getElements().size(); i++) {
        PreparedStatement insert = DatabaseManager.getSingleton().getConnection()
            .prepareStatement("INSERT INTO Compound(compoundId, elementId) VALUES (?, ?);");

        insert.setInt(1, compound.getCompoundId());
        insert.setInt(2, compound.getElements().get(i).getElementId());
        insert.execute();
      }
    } catch (SQLException | DatabaseException e) {
      throw new Exception("CompoundRDGRDS", e);
    }
  }

  /**
   * Convert an element id to an ElementDTO
   * 
   * @param id of element
   * @return ElementDTO
   */
  private ElementDTO elementIdToDTO(int id) {
    
    ElementRDG element = new ElementRDGRDS(id);
    return element.getElement();
  }

  /**
   * Read in a compound though the id.
   * 
   * @param id int
   * @return CompoundDTO of id
   */
  public CompoundDTO read(int id) throws Exception {
    try {
      String sql = "SELECT * FROM Compound INNER JOIN Chemical WHERE Compound.compoundId = Chemical.chemicalId AND Compound.compoundId = "
          + id + ";";
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(sql);
      String name = null;
      double inv = 0;
      // Get all elements connected to compound
      List<ElementDTO> elements = new ArrayList<>();
      while (rs.next()) {
        elements.add(elementIdToDTO(rs.getInt("elementId")));
        name = rs.getString("name");
        inv = rs.getDouble("inventory");
      }

      compound = (new CompoundDTO(id, elements, name, inv));

      return compound;

    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      throw new Exception("Failed to read" + id, e);
    }
  }

  /**
   * Update a compound in the database. For this many to many relationship, we
   * delete all occurrences of the compound id and then re-add the compound to the
   * database.
   * 
   * @param compound to update to
   */
  public CompoundDTO update(CompoundDTO compound) {
    // Note: This might get slow when we get a compound with a LOT of elements
    try {
      delete(compound.getCompoundId());
      create(compound);
    } catch (Exception e) {
      e.printStackTrace();
    }
    return this.compound;
  }

  /**
   * Delete a compound from the database.
   */
  public void delete(int id) {
    try {
      PreparedStatement sql = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("DELETE FROM Compound WHERE compoundId = " + id + ";");
      sql.execute();
      compound = null;
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
    }
  }

  /**
   * Get the compound that the RDG is currently holding.
   */
  public CompoundDTO getCompound() {
    return compound;
  }

}
