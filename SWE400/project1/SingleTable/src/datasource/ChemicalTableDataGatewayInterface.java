package datasource;

import dataDTO.ChemicalDTO;
import java.util.List;

/**
 * Table Data Gateway for Chemical.
 * 
 * @author andrewjanuszko
 */
public interface ChemicalTableDataGatewayInterface {

  /**
   * Get all Chemicals.
   * 
   * @return all Chemicals.
   */

  public ChemicalTableDataGatewayInterface getAll();

  /**
   * Get all Elements.
   * 
   * @return all Elements.
   */
  public ChemicalTableDataGatewayInterface getElements();

  /**
   * Get all Metals.
   * 
   * @return all Metals.
   */
  public ChemicalTableDataGatewayInterface getMetals();

  /**
   * Get all Compounds.
   * 
   * @return all Compounds.
   */
  public ChemicalTableDataGatewayInterface getCompounds();

  /**
   * Get all Bases.
   * 
   * @return all Bases.
   */
  public ChemicalTableDataGatewayInterface getBases();

  /**
   * Get all Acids.
   * 
   * @return all Acids.
   */
  public ChemicalTableDataGatewayInterface getAcids();

  /**
   * Get all Chemicals with a name like String.
   * 
   * @param nameLike the name we are looking for.
   * @return all Chemicals with a name like String.
   */
  public ChemicalTableDataGatewayInterface filterByNameLike(String nameLike);

  /**
   * Get all Chemicals with a given inventoryValue.
   * 
   * @param inventoryValue the amount of a chemical in stock.
   * @return all chemical with a given inventory
   */
  public ChemicalTableDataGatewayInterface filterByInventory(double inventoryValue);

  /**
   * Get all Chemicals with an inventoryValue between the given range.
   * 
   * @param min the minimum amount of a chemical.
   * @param max the maximum amount of a chemical.
   * @return all chemical in a given inventory range
   */
  public ChemicalTableDataGatewayInterface filterByInventoryBetween(double min, double max);

  /**
   * Get all Chemicals with an atomicNumberValue.
   * 
   * @param atomicNumberValue the atomic number you are looking for.
   * @return all chemical with a given atomic number
   */
  public ChemicalTableDataGatewayInterface filterByAtomicNumber(int atomicNumberValue);

  /**
   * Get all Chemicals with an atomic number between the given range.
   * 
   * @param min the minimum amount for the atomic number range.
   * @param max the maximum amount the atomic number range.
   * @return all chemical in a given atomic number range
   */
  public ChemicalTableDataGatewayInterface filterByAtomicNumberBetween(int min, int max);

  /**
   * Get all Chemicals with a given atomicMassValue.
   * 
   * @param atomicMassValue the atomic mass you are searching by.
   * @return
   */
  public ChemicalTableDataGatewayInterface filterByAtomicMass(double atomicMassValue);

  /**
   * Get all Chemicals with an atomic mass between the given range.
   * 
   * @param min the minimum amount for the atomic mass range.
   * @param max the maximum amount the atomic mass range.
   * @return all chemical in a given atomic mass reange.
   */
  public ChemicalTableDataGatewayInterface filterByAtomicMassBetween(double min, double max);

  /**
   * Get all chemical with a given dissolvedByID.
   * 
   * @param dissolvedByID the id of an acid the dissolves the a metal.
   * @return all chemical that are dissolved by a the given id.
   */
  public ChemicalTableDataGatewayInterface filterByDissolvedBy(int dissolvedByID);

  /**
   * Get all chemicals with a specific acid amount.
   * 
   * @param acidAmount a specific acid amount.
   * @return all chemicals with a specific acid amount.
   */
  public ChemicalTableDataGatewayInterface filterByAcidAmount(double acidAmount);

  /**
   * Get all chemicals in a given acid amount range.
   * 
   * @param min the minimum amount for the acid amount range.
   * @param max the maximum amount for the acid amount range.
   * @return all chemical in the given acid amount range.
   */
  public ChemicalTableDataGatewayInterface filterByAcidAmountBetween(double min, double max);

  /**
   * Get all chemicals with given soluteId.
   * 
   * @param soluteID the id of a solute.
   * @return all chemicals with the given soluteId.
   */
  public ChemicalTableDataGatewayInterface filterBySolute(int soluteID);

  /**
   * Runs the query
   * 
   * @return the results of the constructed query
   * @throws DatabaseException
   */
  public List<ChemicalDTO> executeQuery() throws DatabaseException;

  /**
   * Gets all the chemicals that are low inventory.
   * 
   * @return
   * @throws DatabaseException
   */
  public List<ChemicalDTO> getAllWithLowInventory() throws DatabaseException;

  /**
   * Gets all the elements that are low inventory.
   * 
   * @return
   * @throws DatabaseException
   */
  public List<ChemicalDTO> getElementsWithLowInventory() throws DatabaseException;

  /**
   * Gets all the metals that are low inventory
   * 
   * @return
   * @throws DatabaseException
   */
  public List<ChemicalDTO> getMetalsWithLowInventory() throws DatabaseException;

  /**
   * Gets all the bases that are low inventory
   * 
   * @return
   * @throws DatabaseException
   */
  public List<ChemicalDTO> getBasesWithLowInventory() throws DatabaseException;

  /**
   * Gets all the acids that are low inventory
   * 
   * @return
   * @throws DatabaseException
   */
  public List<ChemicalDTO> getAcidsWithLowInventory() throws DatabaseException;

}
