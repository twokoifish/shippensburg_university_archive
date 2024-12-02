package model;

import java.util.List;

public interface ChemicalDataMapperInterface {
  
  /**
   * Read a Chemical from the database. 
   * @param id the id to search.
   * @return the Chemical.
   * @throws DomainModelException when things go wrong.
   */
  public Chemical read(int id) throws DomainModelException;

  /**
   * Get all Chemicals.
   * 
   * @return all Chemicals.
   * @throws DomainModelException when things go wrong.
   */
  public List<Chemical> getAll() throws DomainModelException;

  /**
   * Get all Chemicals that have a partial match.
   * 
   * @param wildCard the name to partial match.
   * @return all Chemicals that have a partial match.
   * @throws DomainModelException when things go wrong.
   */
  public List<Chemical> filterByNameLike(String nameLike) throws DomainModelException;

  /**
   * Get all Chemicals with a set inventory value.
   * 
   * @param inventory the inventory of the Acid.
   * @return all Chemicals with a set inventory value.
   * @throws DomainModelException when things go wrong.
   */
  public List<Chemical> filterByInventory(double inventory) throws DomainModelException;

  /**
   * Get all Chemicals with inventory in the provided range.
   * 
   * @param min the minimum inventory value.
   * @param max the maximum inventory value.
   * @return all Chemicals with inventory in the provided range.
   * @throws DomainModelException when things go wrong.
   */
  public List<Chemical> filterByInventoryBetween(double min, double max) throws DomainModelException;

  /**
   * Get all Chemicals with low inventory.
   * 
   * @return all Chemicals with low inventory.
   * @throws DomainModelException when things go wrong.
   */
  public List<Chemical> filterByLowInventory() throws DomainModelException;

}
