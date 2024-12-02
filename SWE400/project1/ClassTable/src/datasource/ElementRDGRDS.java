package datasource;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import database.DatabaseException;
import database.DatabaseManager;

/**
 * Element Row Data Gateway used to access a row of the Element table
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class ElementRDGRDS implements ElementRDG {
  ElementDTO element;

  /**
   * Empty constructor
   */
  public ElementRDGRDS() {

  }

  /**
   * Constructor to search for an element
   * 
   * @param id
   */
  public ElementRDGRDS(int id) {
    String select = "SELECT * FROM Element INNER JOIN Chemical ON Chemical.chemicalId = Element.elementId WHERE Element.elementId = "
        + id + ";";

    try {
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(select);
      rs.next();

      element = new ElementDTO(id, rs.getInt("atomicNumber"), rs.getDouble("atomicMass"), rs.getString("name"),
          rs.getDouble("inventory"));

      return;
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("couldn't find element with id " + id);
    }
  }

  /**
   * Constructor to create an element
   * 
   * @param atomicNum
   * @param atomicMass
   * @param name
   * @param inhabits
   */
  public ElementRDGRDS(int atomicNum, double atomicMass, String name, double inventory) {
    try {
      PreparedStatement insertChemical = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("INSERT INTO Chemical (name, inventory) VALUES (?, ?);");
      insertChemical.setString(1, name);
      insertChemical.setDouble(2, inventory);

      PreparedStatement insert = DatabaseManager.getSingleton().getConnection().prepareStatement(
          "INSERT INTO Element (elementId, atomicNumber, atomicMass) VALUES (LAST_INSERT_ID(), ?, ?);");
      insert.setInt(1, atomicNum);
      insert.setDouble(2, atomicMass);

      insertChemical.execute();
      insert.execute();

      String fetchId = ("SELECT LAST_INSERT_ID();");
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(fetchId);
      rs.next();

      element = new ElementDTO(rs.getInt("LAST_INSERT_ID()"), atomicNum, atomicMass, name, inventory);

    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Failed to insert");
    }
  }

  /**
   * Deletes an Element held by the RDGRDS
   */
  @Override
  public void delete() {
    String sqlElement = "DELETE FROM Element WHERE elementId = " + element.getElementId() + ";";
    String sqlChem = "DELETE FROM Chemical WHERE chemicalId = " + element.getElementId() + ";";
    try {

      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      statement.executeUpdate(sqlElement);
      statement.executeUpdate(sqlChem);
      element = null;
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Problem deleting Element with id " + element.getElementId());
    }

  }

  /**
   * Updates an element with the values in the ElementDTO.
   */
  @Override
  public void update() {
    try {
      PreparedStatement updateElement = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("UPDATE Element SET atomicNumber = ?, atomicMass = ? WHERE elementId = ?;");
      updateElement.setInt(1, element.getAtomicNumber());
      updateElement.setDouble(2, element.getAtomicMass());
      updateElement.setInt(3, element.getElementId());

      PreparedStatement updateChemical = DatabaseManager.getSingleton().getConnection()
          .prepareStatement("UPDATE Chemical SET name = ?, inventory = ? WHERE chemicalId = ?;");
      updateChemical.setString(1, element.getName());
      updateChemical.setDouble(2, element.getInventory());
      updateChemical.setInt(3, element.getElementId());

      updateElement.execute();
      updateChemical.execute();
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      System.out.println("Failed to update");
    }

  }

  /**
   * Finds entry by atomic number
   * 
   * @param atomicNum to search for
   * @return ElementDTO with specified atomicNum
   */
  public ElementDTO findByAtomicNumber(int atomicNum) {
    int id;
    double atomicMass, inventory;
    String name;
    String sqlElement = "SELECT * FROM Element WHERE atomicNumber = " + atomicNum + ";";
    try {
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(sqlElement);
      rs.next();
      id = rs.getInt("elementId");
      atomicMass = rs.getDouble("atomicMass");

      String sqlChem = "SELECT * FROM Chemical INNER JOIN Element ON Chemical.chemicalId = " + id + ";";
      rs = statement.executeQuery(sqlChem);
      rs.next();

      name = rs.getString("name");
      inventory = rs.getDouble("inventory");

      return new ElementDTO(id, atomicNum, atomicMass, name, inventory);

    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * Find entry with a specific atomic mass
   * 
   * @param atomicMass to search for
   * @return ElementTDO
   */
  @Override
  public ElementDTO findByAtomicMass(double atomicMass) {
    int id, atomicNum;
    double inventory;
    String name;

    String sqlElement = "SELECT * FROM Element WHERE atomicMass = " + atomicMass + ";";
    try {

      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      ResultSet rs = statement.executeQuery(sqlElement);
      rs.next();
      id = rs.getInt("elementId");
      atomicNum = rs.getInt("atomicNumber");

      String sqlChem = "SELECT * FROM Chemical INNER JOIN Element ON Chemical.chemicalId = " + id + ";";
      rs = statement.executeQuery(sqlChem);
      rs.next();
      name = rs.getString("name");
      inventory = rs.getDouble("inventory");

      return new ElementDTO(id, atomicNum, atomicMass, name, inventory);

    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * Set atomic number
   */
  @Override
  public void setAtomicNumber(int atomicNumber) {
    element.setAtomicNumber(atomicNumber);
  }

  /**
   * Set atomic mass
   */
  @Override
  public void setAtomicMass(double atomicMass) {
    element.setAtomicMass(atomicMass);
  }

  /**
   * Set name
   */
  @Override
  public void setName(String name) {
    element.setName(name);
  }

  /**
   * Set name
   */
  @Override
  public void setInventory(double inventory) {
    element.setInventory(inventory);
  }

  /**
   * Get element currently held by the RDGRDS
   */
  public ElementDTO getElement() {
    return element;
  }

}
