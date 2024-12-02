package model;

import java.util.List;

/**
 * Interface for a Metal mapper.
 * 
 * @author andrewjanuszko & morgan williams-burrell
 *
 */
public interface MetalDataMapperInterface {

  /**
   * Create a Metal.
   * 
   * @param name         the name of the Metal.
   * @param inventory    the inventory of the Metal.
   * @param atomicNumber the atomic number of the Metal.
   * @param atomicMass   the atomic mass of the Metal.
   * @param acidAmount   the amount of acid needed to dissolve the Metal.
   * @return a Metal.
   * @throws DomainModelException when things go wrong.
   */
  public Metal create(String name, double inventory, int atomicNumber, double atomicMass, double acidAmount)
      throws DomainModelException;

  /**
   * Read a Metal from a Gateway.
   * 
   * @param id the ID of the Metal.
   * @return a Metal from a Gateway.
   * @throws DomainModelException when things go wrong.
   */
  public Metal read(int id) throws DomainModelException;

  /**
   * Update a Metal from a Gateway.
   * 
   * @param metal the Metal to update.
   * @throws DomainModelException when things go wrong.
   */
  public void update(Metal metal) throws DomainModelException;

  /**
   * Delete a Metal from a Gateway.
   * 
   * @param metal the Metal to delete.
   * @throws DomainModelException when things go wrong.
   */
  public void delete(Metal metal) throws DomainModelException;

  /**
   * Get all Metals.
   * 
   * @return all Metals.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> getAll() throws DomainModelException;

  /**
   * Get all Metals that have a partial name match.
   * 
   * @param wildCard the name to partial match.
   * @return all Metals that have a partial name match.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByNameLike(String nameLike) throws DomainModelException;

  /**
   * Get all Metals that have a specific inventory value.
   * 
   * @param inventory the inventory value.
   * @return all Metals that have a specific inventory value.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByInventory(double inventory) throws DomainModelException;

  /**
   * Get all Metals that have an inventory in a given range.
   * 
   * @param min the minimum value.
   * @param max the maximum value.
   * @return all Metals that have an inventory in a given range.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByInventoryBetween(double min, double max) throws DomainModelException;

  /**
   * Get all Metals with a given atomic number.
   * 
   * @param atomicNumber the atomic number of the Metals.
   * @return all Metals with a given atomic number.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByAtomicNumber(int atomicNumber) throws DomainModelException;

  /**
   * 
   * @param min
   * @param max
   * @return
   * @throws DomainModelException
   */
  public List<Metal> filterByAtomicNumberBetween(int min, int max) throws DomainModelException;

  /**
   * Get all Metals with a specific atomic mass.
   * 
   * @param atomicMass the atomic mass.
   * @return all Metals with a specific atomic mass.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByAtomicMass(double atomicMass) throws DomainModelException;

  /**
   * Get all Metals with an atomic mass in a given range.
   * 
   * @param min the minimum value.
   * @param max the maximum value.
   * @return all Metals with an atomic mass in a given range.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByAtomicMassBetween(double min, double max) throws DomainModelException;

  /**
   * Get all Metals with a specific acid required value.
   * 
   * @param acidRequired the amount of acid required.
   * @return all Metals with a specific acid required value.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByAcidAmount(double acidAmount) throws DomainModelException;

  /**
   * Get all Metals with an acid required value in a range.
   * 
   * @param min the minimum value.
   * @param max the maximum value.
   * @return all Metals with an acid required value in a range.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByAcidAmountBetween(double min, double max) throws DomainModelException;

  /**
   * Get all Metals dissolved by an Acid.
   * 
   * @param acidID the Acid.
   * @return all Metals dissolved by an Acid.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByDissolvedBy(int acidID) throws DomainModelException;

  /**
   * Get all Metals in a given Compound.
   * 
   * @param compoundID the Compound.
   * @return all Metals in a given Compound.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByPartOfCompound(int compoundID) throws DomainModelException;

  /**
   * Get all Metals with low inventory.
   * 
   * @return all Metals with low inventory.
   * @throws DomainModelException when things go wrong.
   */
  public List<Metal> filterByLowInventory() throws DomainModelException;

}
