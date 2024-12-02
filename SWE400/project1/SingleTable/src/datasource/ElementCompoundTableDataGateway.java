package datasource;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import dataDTO.ChemicalDTO;
import dataDTO.ElementCompoundDTO;

/**
 * The RDS version of the gateway for CompoundMadeFromElement.
 * 
 * @author andrewjanuszko
 */
public class ElementCompoundTableDataGateway implements ElementCompoundTableDataGatewayInterface {
  
  private static ElementCompoundTableDataGateway singletonInstance;
  
  /**
   * 
   */
  private ElementCompoundTableDataGateway() {
    
  }
  
  /**
   * 
   * @return
   */
  public static synchronized ElementCompoundTableDataGateway getSingletonInstance() {
    if (singletonInstance == null) {
      singletonInstance = new ElementCompoundTableDataGateway();
    }
    return singletonInstance;
  }
  
  /**
   * 
   * @return
   * @throws DatabaseException
   */
  private static Connection getConnection() throws DatabaseException {
    try {
      return DatabaseManager.getSingleton().getConnection();
    } catch (DatabaseException e) {
      throw new DatabaseException("Failed to get connection to database.", e);
    }
  }
  
  /**
   * 
   * @throws DatabaseException
   */
  public void createTable() throws DatabaseException {
    final String createTable = "CREATE TABLE IF NOT EXISTS ElementCompound("
        + "compoundID INTEGER NOT NULL, "
        + "elementID INTEGER NOT NULL, "
        + "FOREIGN KEY (compoundID) REFERENCES Chemical(id), "
        + "FOREIGN KEY (elementID) REFERENCES Chemical(id), "
        + "UNIQUE (compoundID, elementID));";
    PreparedStatement statement;
    try {
      statement = getConnection().prepareStatement(createTable);
      statement.execute();
    } catch (SQLException | DatabaseException e) {
      throw new DatabaseException("Failed to create table 'ElementCompound'.", e);
    }
  }
  
  /**
   * 
   * @throws DatabaseException
   */
  public void dropTable() throws DatabaseException {
    final String dropTable = "DROP TABLE IF EXISTS ElementCompound;";
    PreparedStatement statement;
    try {
      statement = getConnection().prepareStatement(dropTable);
      statement.execute();
    } catch (SQLException | DatabaseException e) {
      throw new DatabaseException("Failed to drop table 'ElementCompound'.", e);
    }
  }
  
  /**
   * 
   */
  @Override
  public void create(int compoundID, int elementID) throws DatabaseException {
    final String create = "INSERT INTO ElementCompound VALUES (?, ?);";
    PreparedStatement statement;
    try {
      statement = getConnection().prepareStatement(create);
      statement.setInt(1, compoundID);
      statement.setInt(2, elementID);
      statement.execute();
    } catch (SQLException | DatabaseException e) {
      throw new DatabaseException("Failed to insert into table 'ElementCompound'.", e);
    }
  }

  /**
   * 
   */
  @Override
  public void update(int oldCompoundID, int oldElementID, int newCompoundID, int newElementID)
      throws DatabaseException {
    final String update = "UPDATE ElementCompound SET compoundID = ?, elementID = ? WHERE compoundID = ? AND elementID = ?;";
    PreparedStatement statement;
    try {
      statement = getConnection().prepareStatement(update);
      statement.setInt(1, newCompoundID);
      statement.setInt(2, newElementID);
      statement.setInt(3, oldCompoundID);
      statement.setInt(4, oldElementID);
      statement.execute();
    } catch (SQLException | DatabaseException e) {
      throw new DatabaseException("Failed to update in table 'ElementCompound'.", e);
    }
  }

  /**
   * 
   */
  @Override
  public void delete(int compoundID, int elementID) throws DatabaseException {
    final String delete = "DELETE FROM ElementCompound WHERE compoundID = ? AND elementID = ?;";
    PreparedStatement statement;
    try {
      statement = getConnection().prepareStatement(delete);
      statement.setInt(1, compoundID);
      statement.setInt(2, elementID);
      statement.execute();
    } catch (SQLException | DatabaseException e) {
      throw new DatabaseException("Failed to delete from table 'ElementCompound'.", e);
    }
  }

  /**
   * 
   */
  @Override
  public ElementCompoundDTO readElementsFromCompound(int compoundID) throws DatabaseException {
    final String read = "SELECT * FROM Chemical WHERE Chemical.id IN (SELECT elementID FROM ElementCompound WHERE ElementCompound.compoundID = ?);";
    PreparedStatement statement;
    try {
      statement = getConnection().prepareStatement(read);
      statement.setInt(1, compoundID);
      List<ChemicalDTO> relations = convertQueryToDTO(statement);
      return new ElementCompoundDTO(compoundID, relations);
    } catch (SQLException | DatabaseException e) {
      throw new DatabaseException("Failed to get all Elements in Compound '" + compoundID + "'.", e);
    }
  }

  /**
   * 
   */
  @Override
  public ElementCompoundDTO readCompoundsWithElement(int elementID) throws DatabaseException {
    final String read = "SELECT * FROM Chemical WHERE Chemical.id IN (SELECT compoundID FROM ElementCompound WHERE ElementCompound.elementID = ?);";
    PreparedStatement statement;
    try {
      statement = getConnection().prepareStatement(read);
      statement.setInt(1, elementID);
      List<ChemicalDTO> relations = convertQueryToDTO(statement);
      return new ElementCompoundDTO(elementID, relations);
    } catch (SQLException | DatabaseException e) {
      throw new DatabaseException("Failed to get all Compounds with Element '" + elementID + "'.", e);
    }
  }
  
  /**
   * 
   * @param statement
   * @return
   * @throws DatabaseException
   */
  private List<ChemicalDTO> convertQueryToDTO(PreparedStatement statement) throws DatabaseException {
    List<ChemicalDTO> chemicals = new ArrayList<>();
    try {
      ResultSet results = statement.executeQuery();
      while (results.next()) {
        int id = results.getInt("id");
        int type = results.getInt("type");
        String name = results.getString("name");
        double inventory = results.getDouble("inventory");
        int atomicNumber = results.getInt("atomicNumber");
        double atomicMass = results.getDouble("atomicMass");
        int dissolvedBy = results.getInt("dissolvedBy");
        double acidAmount = results.getDouble("acidAmount");
        int solute = results.getInt("solute");
        ChemicalDTO add = new ChemicalDTO(id, type, name, inventory, atomicNumber, atomicMass, dissolvedBy, acidAmount, solute);
        chemicals.add(add);
      }
      return chemicals;
    } catch (SQLException e) {
      throw new DatabaseException("Failed to convert PreparedStatement to List<ChemicalDTO>.", e);
    }
  }

}
