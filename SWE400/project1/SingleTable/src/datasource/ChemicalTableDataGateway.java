package datasource;

import dataDTO.ChemicalDTO;
import dataENUM.ChemicalEnum;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * Implements a ChemicalTableDataGateway.
 * 
 * @author andrewjanuszko
 */
public class ChemicalTableDataGateway implements ChemicalTableDataGatewayInterface {

  private static ChemicalTableDataGatewayInterface singletonInstance = null;

  private static String querySQL = "";

  /**
   * Retrieves a Singleton instance of ChemicalTableDataGateway. Creates a new one
   * if it does not exist.
   * 
   * @return a Singleton instance of ChemicalTableDataGateway.
   */
  public static synchronized ChemicalTableDataGatewayInterface getSingletonInstance() {
    if (singletonInstance == null) {
      singletonInstance = new ChemicalTableDataGateway();
    }
    return singletonInstance;
  }
  
  private ChemicalTableDataGateway() {
    
  }

  /**
   * Converts a query to a list of ChemicalDTOs.
   * 
   * @param statement the query on the database.
   * @return a list of ChemicalDTOs.
   * @throws DatabaseException when things go wrong.
   */
  private List<ChemicalDTO> convertToDTO(PreparedStatement statement) throws DatabaseException {
    List<ChemicalDTO> listDTO = new ArrayList<>();
    try {
      ResultSet results = statement.executeQuery();
      querySQL = "";
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
        ChemicalDTO chemical = new ChemicalDTO(id, type, name, inventory, atomicNumber, atomicMass, dissolvedBy,
            acidAmount, solute);
        listDTO.add(chemical);
      }
    } catch (SQLException e) {
      throw new DatabaseException("Failed to convert query to DTO.", e);
    }
    return listDTO;
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getAll().
   */
  @Override
  public ChemicalTableDataGatewayInterface getAll() {
    querySQL += "SELECT * FROM Chemical WHERE (Chemical.type <> 0)";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getElements().
   */
  @Override
  public ChemicalTableDataGatewayInterface getElements() {
    querySQL += "SELECT * FROM Chemical WHERE (Chemical.type = " + ChemicalEnum.ELEMENT.getIntValue() + " OR Chemical.type = " + ChemicalEnum.METAL.getIntValue() + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getMetals().
   */
  @Override
  public ChemicalTableDataGatewayInterface getMetals() {
    querySQL += "SELECT * FROM Chemical WHERE (Chemical.type = " + ChemicalEnum.METAL.getIntValue() + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getCompounds().
   */
  @Override
  public ChemicalTableDataGatewayInterface getCompounds() {
    querySQL += "SELECT * FROM Chemical WHERE (Chemical.type = " + ChemicalEnum.COMPOUND.getIntValue() + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getBases().
   */
  @Override
  public ChemicalTableDataGatewayInterface getBases() {
    querySQL += "SELECT * FROM Chemical WHERE (Chemical.type = " + ChemicalEnum.BASE.getIntValue() + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getAcids().
   */
  @Override
  public ChemicalTableDataGatewayInterface getAcids() {
    querySQL += "SELECT * FROM Chemical WHERE (Chemical.type = " + ChemicalEnum.ACID.getIntValue() + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#filterByNameLike(String nameLike).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterByNameLike(String nameLike) {
    querySQL += " AND (Chemical.name LIKE '%" + nameLike + "%')";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#filterByInventory(double inventory).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterByInventory(double inventory) {
    querySQL += " AND (Chemical.inventory = " + inventory + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#filterByInventoryBetween(double min, double max).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterByInventoryBetween(double min, double max) {
    querySQL += " AND (Chemical.inventory BETWEEN " + min + " AND " + max + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#filterByAtomicNumber(int atomicNumber).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterByAtomicNumber(int atomicNumber) {
    querySQL += " AND (Chemical.atomicNumber = " + atomicNumber + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#filterByAtomicNumberBetween(int min, int max).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterByAtomicNumberBetween(int min, int max) {
    querySQL += " AND (Chemical.atomicNumber BETWEEN " + min + " AND " + max + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay# filterByAtomicMass(double atomicMass).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterByAtomicMass(double atomicMass) {
    querySQL += " AND (Chemical.atomicMass = " + atomicMass + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#filterByAtomicMassBetween(double min, double max).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterByAtomicMassBetween(double min, double max) {
    querySQL += " AND (Chemical.atomicMass BETWEEN " + min + " AND " + max + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#filterByDissolvedBy(int acidID).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterByDissolvedBy(int acidID) {
    querySQL += " AND (Chemical.dissolvedBy = " + acidID + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#filterByAcidAmount(double acidAmount).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterByAcidAmount(double acidAmount) {
    querySQL += " AND (Chemical.acidAmount = " + acidAmount + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#filterByAcidAmountBetween(double min, double max).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterByAcidAmountBetween(double min, double max) {
    querySQL += " AND (Chemical.acidAmount BETWEEN " + min + " AND " + max + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#filterBySolute(int soluteID).
   */
  @Override
  public ChemicalTableDataGatewayInterface filterBySolute(int soluteID) {
    querySQL += " AND (Chemical.solute = " + soluteID + ")";
    return getSingletonInstance();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#executeQuery().
   */
  @Override
  public List<ChemicalDTO> executeQuery() throws DatabaseException {
    try {
      PreparedStatement statement = DatabaseManager.getSingleton().getConnection().prepareStatement(querySQL + ";");
      return convertToDTO(statement);
    } catch (SQLException e) {
      throw new DatabaseException("Failed to execute query.", e);
    }
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getAllWithLowInventory().
   */
  @Override
  public List<ChemicalDTO> getAllWithLowInventory() throws DatabaseException {
    List<ChemicalDTO> lowChemicals = new ArrayList<>();
    lowChemicals.addAll(getElementsWithLowInventory());
    lowChemicals.addAll(getBasesWithLowInventory());
    lowChemicals.addAll(getAcidsWithLowInventory());
    return lowChemicals;
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getElementsWithLowInventory().
   */
  @Override
  public List<ChemicalDTO> getElementsWithLowInventory() throws DatabaseException {
    return getElements().filterByInventoryBetween(0, 20).executeQuery();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getMetalsWithLowInventory().
   */
  @Override
  public List<ChemicalDTO> getMetalsWithLowInventory() throws DatabaseException {
    return getMetals().filterByInventoryBetween(0, 20).executeQuery();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getBasesWithLowInventory().
   */
  @Override
  public List<ChemicalDTO> getBasesWithLowInventory() throws DatabaseException {
    return getBases().filterByInventoryBetween(0, 40).executeQuery();
  }

  /**
   * @see datasource.ChemicalTableDataGateWay#getAcidsWithLowInventory().
   */
  @Override
  public List<ChemicalDTO> getAcidsWithLowInventory() throws DatabaseException {
    List<ChemicalDTO> acids = getAcids().executeQuery();
    List<ChemicalDTO> lowAcids = new ArrayList<ChemicalDTO>();
    for (ChemicalDTO acid : acids) {
      int acidAmountNeeded = 0;
      List<ChemicalDTO> metals = getMetals().filterByDissolvedBy(acid.getID()).executeQuery();
      for (ChemicalDTO metal : metals) {
        acidAmountNeeded += metal.getAcidAmount();
      }
      if (acid.getInventory() >= acidAmountNeeded) {
        lowAcids.add(acid);
      }
    }
    return lowAcids;
  }

}