package model;

import java.util.List;

/**
 * Interface for a Compound mapper.
 * 
 * @author andrewjanuszko & morgan williams-burrell
 *
 */
public interface CompoundDataMapperInterface {

  /**
   * Create a Compound.
   * 
   * @param name      the name of the Compound.
   * @param inventory the inventory of the Compound.
   * @param madeOf    the Elements in the Compound.
   * @return a Compound.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public Compound create(String name, double inventory, List<Element> madeOf) throws DomainModelException;

  /**
   * Read a Compound from the Gateway.
   * 
   * @param id the ID of the Compound.
   * @return a Compound from the Gateway.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public Compound read(int id) throws DomainModelException;

  /**
   * Update a Compound from the Gateway.
   * 
   * @param compound the Compound to update.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public void update(Compound compound) throws DomainModelException;

  /**
   * Delete a Compound from the Gateway.
   * 
   * @param compound the Compound to delete.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public void delete(Compound compound) throws DomainModelException;

  /**
   * Get all Compounds.
   * 
   * @return all Compounds.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public List<Compound> getAll() throws DomainModelException;

  /**
   * Get all Compounds that match a partial name.
   * 
   * @param wildCard the name to partial match.
   * @return all Compounds that match a partial name.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public List<Compound> filterByNameLike(String nameLike) throws DomainModelException;

  /**
   * Get all Compounds with a specific inventory value.
   * 
   * @param inventory the inventory value.
   * @return all Compounds with a specific inventory value.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public List<Compound> filterByInventory(double inventory) throws DomainModelException;

  /**
   * Get all Compounds with inventory in a range.
   * 
   * @param min the minimum value.
   * @param max the maximum value.
   * @return all Compounds with inventory in a range.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public List<Compound> filterByInventoryBetween(double min, double max) throws DomainModelException;

  /**
   * Get all compounds that contain an Element.
   * 
   * @param elementID the Element ID.
   * @return all compounds that contain an Element.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public List<Compound> filterByMadeOf(int elementID) throws DomainModelException;

  /**
   * Get all Compounds with low inventory.
   * 
   * @return all Compounds with low inventory.
   * @throws DomainModelException when things go wrong.
   */
  public List<Compound> filterByLowInventory() throws DomainModelException;

}
