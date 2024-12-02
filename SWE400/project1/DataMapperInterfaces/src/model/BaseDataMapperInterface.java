package model;

import java.util.List;

/**
 * Interface for an Base mapper.
 * 
 * @author andrewjanuszko & morgan williams-burrell
 *
 */
public interface BaseDataMapperInterface {

  /**
   * Creates a Base.
   * 
   * @param name      the name of the Base.
   * @param inventory the inventory of the Base.
   * @param solute    the chemical that dissolves in the Base.
   * @return a Base.
   * @throws DomainModelException when things go wrong.
   */
  public Base create(String name, double inventory, Chemical solute) throws DomainModelException;

  /**
   * Read a Base from the Gateways.
   * 
   * @param id the ID of the Base.
   * @return a Base from the Gateways.
   * @throws DomainModelException when things go wrong.
   */
  public Base read(int id) throws DomainModelException;

  /**
   * Update a Base from the Gateways.
   * 
   * @param base the Base to update.
   * @throws DomainModelException when things go wrong.
   */
  public void update(Base base) throws DomainModelException;

  /**
   * Delete a Base from the Gateways.
   * 
   * @param base the Base to delete.
   * @throws DomainModelException when things go wrong.
   */
  public void delete(Base base) throws DomainModelException;

  /**
   * Get all Bases.
   * 
   * @return all Bases.
   * @throws DomainModelException when things go wrong.
   */
  public List<Base> getAll() throws DomainModelException;

  /**
   * Get all Bases with a partial name match.
   * 
   * @param wildCard the name to partial match.
   * @return all Bases with a partial name match.
   * @throws DomainModelException when things go wrong.
   */
  public List<Base> filterByNameLike(String nameLike) throws DomainModelException;

  /**
   * Get all Bases with a specific inventory value.
   * 
   * @param inventory the specific inventory value.
   * @return all Bases with a specific inventory value.
   * @throws DomainModelException when things go wrong.
   */
  public List<Base> filterByInventory(double inventory) throws DomainModelException;

  /**
   * Get all Bases with a inventory value in a range.
   * 
   * @param min the minimum inventory value.
   * @param max the maximum inventory value.
   * @return all Bases with a inventory value in a range.
   * @throws DomainModelException when things go wrong.
   */
  public List<Base> filterByInventoryBetween(double min, double max) throws DomainModelException;

  /**
   * Get all Bases that have the provided solute.
   * 
   * @param chemicalID the ID of the solute.
   * @return all Bases that have the provided solute.
   * @throws DomainModelException when things go wrong.
   */
  public List<Base> filterBySolute(int chemicalID) throws DomainModelException;

  /**
   * Get all Bases with low inventory.
   * 
   * @return all Bases with low inventory.
   * @throws DomainModelException when things go wrong.
   */
  public List<Base> filterByLowInventory() throws DomainModelException;

}
