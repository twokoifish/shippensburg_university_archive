package datasource;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;

import dataENUM.ChemicalEnum;

/**
 * The RDS version of the gateway for Chemical.
 * 
 * @author andrewjanuszko
 */
public class ChemicalRowDataGateway implements ChemicalRowDataGatewayInterface {

  private int id;
  private int type;
  private String name;
  private double inventory;
  private int atomicNumber;
  private double atomicMass;
  private int dissolvedBy;
  private double acidAmount;
  private int solute;

  /**
   * Constructor for finding Chemicals by ID.
   * 
   * @param id the ID of the Chemical.
   * @throws DatabaseException when things go wrong.
   */
  public ChemicalRowDataGateway(int id) throws DatabaseException {
    try {
      final String loadSQL = "SELECT * FROM Chemical WHERE Chemical.id = ?;";
      PreparedStatement statement = DatabaseManager.getSingleton().getConnection().prepareStatement(loadSQL);
      statement.setInt(1, id);
      ResultSet result = statement.executeQuery();
      result.next();
      loadInstanceVariables(id, result.getInt("type"), result.getString("name"), result.getDouble("inventory"),
          result.getInt("atomicNumber"), result.getDouble("atomicMass"), result.getInt("dissolvedBy"),
          result.getDouble("acidAmount"), result.getInt("solute"));
    } catch (SQLException e) {
      throw new DatabaseException("Could not find Chemical with ID " + id + ".", e);
    }
  }

  /**
   * Insert a Chemical into the database then store it locally.
   * 
   * @param type         the type of the Chemical.
   * @param name         the name of the Chemical.
   * @param inventory    the amount of acidAmount in storage.
   * @param atomicNumber the atomic number of the Element.
   * @param atomicMass   the atomic mass of the Element.
   * @param dissolvedBy  the acid that dissolves a Metal.
   * @param acidAmount        the amount of acid needed to dissovle a Metal.
   * @param solute       the chemical solutes.
   * @throws DatabaseException when things go wrong.
   */
  public ChemicalRowDataGateway(int type, String name, double inventory, int atomicNumber, double atomicMass,
      int dissolvedBy, double acidAmount, int solute) throws DatabaseException {
    try {
      if (name.contains(" ") && (type == ChemicalEnum.ELEMENT.getIntValue() || type == ChemicalEnum.METAL.getIntValue())) {
        throw new SQLException("Elements and Metals can only have one-word names. If it is a isotope use a hyphen.");
      }
      String createSQL = "INSERT INTO Chemical (type, name, inventory, atomicNumber, atomicMass, dissolvedBy, acidAmount, solute)"
          + " VALUES (?, ?, ?, ?, ?, ?, ?, ?);";
      PreparedStatement statement = DatabaseManager.getSingleton().getConnection().prepareStatement(createSQL,
          PreparedStatement.RETURN_GENERATED_KEYS);
      loadPreparedStatement(statement, type, name, inventory, atomicNumber, atomicMass, dissolvedBy, acidAmount, solute);
      statement.executeUpdate();
      ResultSet result = statement.getGeneratedKeys();
      result.next();
      loadInstanceVariables(result.getInt(1), type, name, inventory, atomicNumber, atomicMass, dissolvedBy, acidAmount,
          solute);
    } catch (SQLException e) {
      throw new DatabaseException("Failed to insert into 'Chemical' table.", e);
    }
  }

  /**
   * Creates a table in the database.
   * 
   * @throws DatabaseException when things go wrong.
   */
  public static void createTable() throws DatabaseException {
    try {
      final String createTableSQL = "CREATE TABLE IF NOT EXISTS Chemical("
                                  + "id INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY, " 
                                  + "type INTEGER NOT NULL, "
                                  + "name VARCHAR(20) NOT NULL UNIQUE, " 
                                  + "inventory DOUBLE NOT NULL, " 
                                  + "atomicNumber INTEGER, "
                                  + "atomicMass DOUBLE, " 
                                  + "dissolvedBy INTEGER, " 
                                  + "acidAmount DOUBLE, " 
                                  + "solute INTEGER, "
                                  + "CHECK (inventory >= 0 AND atomicNumber <= atomicMass));";
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      statement.executeUpdate("SET FOREIGN_KEY_CHECKS = 1;");
      statement.executeUpdate(createTableSQL);
    } catch (SQLException e) {
      throw new DatabaseException("Failed to create 'Chemical' table.", e);
    }
  }

  /**
   * Drops a table in the database.
   * 
   * @throws DatabaseException when things go wrong.
   */
  public static void dropTable() throws DatabaseException {
    try {
      final String dropTableSQL = "DROP TABLE IF EXISTS Chemical, CompoundMadeFromElement;";
      Statement statement = DatabaseManager.getSingleton().getConnection().createStatement();
      statement.executeUpdate("SET FOREIGN_KEY_CHECKS = 0;");
      statement.executeUpdate(dropTableSQL);
    } catch (SQLException e) {
      throw new DatabaseException("Failed to drop 'Chemical', 'CompoundMadeFromElement' table.", e);
    }
  }

  /**
   * @see datasource.ChemicalRowDataGateway#delete(void).
   */
  @Override
  public void delete() throws DatabaseException {
    try {
      final String deleteSQL = "DELETE FROM Chemical WHERE Chemical.id = ?;";
      PreparedStatement statement = DatabaseManager.getSingleton().getConnection().prepareStatement(deleteSQL);
      statement.setInt(1, id);
      statement.execute();
    } catch (SQLException e) {
      throw new DatabaseException("Failed to delete Chemical with ID " + id + ".", e);
    }
  }

  /**
   * @see datasource.ChemicalRowDataGateway#update(void).
   */
  @Override
  public void update() throws DatabaseException {
    try {
      final String updateSQL = "UPDATE Chemical SET Chemical.type = ?, Chemical.name = ?, Chemical.inventory = ?, Chemical.atomicNumber = ?, Chemical.atomicMass = ?, Chemical.dissolvedBy = ?, Chemical.acidAmount = ?, Chemical.solute = ? WHERE Chemical.id = " + id + ";";
      PreparedStatement statement = DatabaseManager.getSingleton().getConnection().prepareStatement(updateSQL);
      loadPreparedStatement(statement, this.type, this.name, this.inventory, this.atomicNumber, this.atomicMass,
          this.dissolvedBy, this.acidAmount, this.solute);
      statement.execute();
    } catch (SQLException e) {
      throw new DatabaseException("Failed to update Chemical with ID " + id + ".", e);
    }
  }

  /**
   * Loads values into a PreparedStatement.
   * 
   * @param type         the type of the Chemical.
   * @param name         the name of the Chemical.
   * @param inventory    the amount of acidAmount in storage.
   * @param atomicNumber the atomic number of the Element.
   * @param atomicMass   the atomic mass of the Element.
   * @param dissolvedBy  the acid that dissolves a Metal.
   * @param acidAmount        the amount of acid needed to dissovle a Metal.
   * @param solute       the chemical solutes.
   * @throws DatabaseException when things go wrong.
   */
  private void loadPreparedStatement(PreparedStatement statement, int type, String name, double inventory,
      int atomicNumber, double atomicMass, int dissolvedBy, double acidAmount, int solute) throws DatabaseException {
    try {
      statement.setInt(1, type);
      statement.setString(2, name);
      statement.setDouble(3, inventory);
      statement.setInt(4, atomicNumber);
      statement.setDouble(5, atomicMass);
      
      if (dissolvedBy <= 0) {
        statement.setNull(6, Types.INTEGER);
      } else {
        statement.setInt(6, dissolvedBy);
      }
      
      statement.setDouble(7, acidAmount);
      
      if (solute <= 0) {
        statement.setNull(8, Types.INTEGER);
      } else {
        statement.setInt(8, solute);
      }
    } catch (SQLException e) {
      throw new DatabaseException("Failed to load prepared statment.", e);
    }
  }

  /**
   * Loads values into local instance variables.
   * 
   * @param id   the ID of the Chemical.
   * @param type         the type of the Chemical.
   * @param name         the name of the Chemical.
   * @param inventory    the amount of acidAmount in storage.
   * @param atomicNumber the atomic number of the Element.
   * @param atomicMass   the atomic mass of the Element.
   * @param dissolvedBy  the acid that dissolves a Metal.
   * @param acidAmount        the amount of acid needed to dissovle a Metal.
   * @param solute       the chemical solutes.
   * @throws DatabaseException when things go wrong.
   */
  private void loadInstanceVariables(int id, int type, String name, double inventory, int atomicNumber,
      double atomicMass, int dissolvedBy, double acidAmount, int solute) {
    setID(id);
    setType(type);
    setName(name);
    setInventory(inventory);
    setAtomicNumber(atomicNumber);
    setAtomicMass(atomicMass);
    setDissolvedBy(dissolvedBy);
    setAcidAmount(acidAmount);
    setSolute(solute);
  }

  /**
   * Returns the ID of a chemical.
   * 
   * @return the ID.
   */
  @Override
  public int getID() {
    return this.id;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#getType().
   */
  @Override
  public int getType() {
    return this.type;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#getName().
   */
  @Override
  public String getName() {
    return this.name;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#getInventory().
   */
  @Override
  public double getInventory() {
    return this.inventory;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#getAtomicNumber().
   */
  @Override
  public int getAtomicNumber() {
    return this.atomicNumber;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#getAtomicMass().
   */
  @Override
  public double getAtomicMass() {
    return this.atomicMass;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#getDissolvedBy().
   */
  @Override
  public int getDissolvedBy() {
    return this.dissolvedBy;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#getMoles().
   */
  @Override
  public double getAcidAmount() {
    return this.acidAmount;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#getSolute().
   */
  @Override
  public int getSolute() {
    return this.solute;
  }

  /**
   * Sets the ID of the chemical.
   * 
   * @param id the ID of the Chemical.
   */
  private void setID(int id) {
    this.id = id;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#setType(int).
   */
  @Override
  public void setType(int type) {
    this.type = type;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#setName(String).
   */
  @Override
  public void setName(String name) {
    this.name = name;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#setInventory(String).
   */
  @Override
  public void setInventory(double inventory) {
    this.inventory = inventory;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#setAtomicNumber(int).
   */
  @Override
  public void setAtomicNumber(int atomicNumber) {
    this.atomicNumber = atomicNumber;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#setAtomicMass(double).
   */
  @Override
  public void setAtomicMass(double atomicMass) {
    this.atomicMass = atomicMass;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#setDissolvedBy(int).
   */
  @Override
  public void setDissolvedBy(int dissolvedBy) {
    this.dissolvedBy = dissolvedBy;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#setMoles(double);
   */
  @Override
  public void setAcidAmount(double acidAmount) {
    this.acidAmount = acidAmount;
  }

  /**
   * @see datasource.ChemicalRowDataGateway#setSolute(int).
   */
  @Override
  public void setSolute(int solute) {
    this.solute = solute;
  }

}