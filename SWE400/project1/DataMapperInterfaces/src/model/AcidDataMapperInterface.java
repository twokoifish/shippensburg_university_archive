package model;

import java.util.List;

/**
 * Interface for an Acid mapper.
 * 
 * @author andrewjanuszko & morgan williams-burrell
 *
 */
public interface AcidDataMapperInterface {

  /**
   * Creates an Acid.
   * 
   * @param name      the name of the Acid.
   * @param inventory the inventory of the Acid.
   * @param dissolves the Metals that the Acid dissolves.
   * @param solute    the chemical that dissolves in the Acid.
   * @return an Acid.
   * @throws DomainModelException when things go wrong.
   */
  public Acid create(String name, double inventory, List<Metal> dissolves, Chemical solute) throws DomainModelException;

  /**
   * Reads an Acid from the Gateways.
   * 
   * @param id the ID of the Acid.
   * @return the Acid.
   * @throws DomainModelException when things go wrong.
   */
  public Acid read(int id) throws DomainModelException;

  /**
   * Updates an Acid in the Gateway.
   * 
   * @param acid the Acid to update.
   * @throws DomainModelException when things go wrong.
   */
  public void update(Acid acid) throws DomainModelException;

  /**
   * Delete an Acid in the Gateway.
   * 
   * @param acid the Acid to delete.
   * @throws DomainModelException when things go wrong.
   */
  public void delete(Acid acid) throws DomainModelException;

  /**
   * Get all Acids.
   * 
   * @return all Acids.
   * @throws DomainModelException when things go wrong.
   */
  public List<Acid> getAll() throws DomainModelException;

  /**
   * Get all Acids that have a partial match.
   * 
   * @param wildCard the name to partial match.
   * @return all Acids that have a partial match.
   * @throws DomainModelException when things go wrong.
   */
  public List<Acid> filterByNameLike(String nameLike) throws DomainModelException;

  /**
   * Get all Acids with a set inventory value.
   * 
   * @param inventory the inventory of the Acid.
   * @return all Acids with a set inventory value.
   * @throws DomainModelException when things go wrong.
   */
  public List<Acid> filterByInventory(double inventory) throws DomainModelException;

  /**
   * Get all Acids with inventory in the provided range.
   * 
   * @param min the minimum inventory value.
   * @param max the maximum inventory value.
   * @return all Acids with inventory in the provided range.
   * @throws DomainModelException when things go wrong.
   */
  public List<Acid> filterByInventoryBetween(double min, double max) throws DomainModelException;

  /**
   * Get all Acids that have the provided solute.
   * 
   * @param chemicalID the ID of the solute.
   * @return all Acids that have the provided solute.
   * @throws DomainModelException when things go wrong.
   */
  public List<Acid> filterBySolute(int chemicalID) throws DomainModelException;

  /**
   * Get all Acids with low inventory.
   * 
   * @return all Acids with low inventory.
   * @throws DomainModelException when things go wrong.
   */
  public List<Acid> filterByLowInventory() throws DomainModelException;

}
