package model;

import java.util.List;

/**
 * Interface for an Element mapper.
 * 
 * @author andrewjanuszko & morgan williams-burrell
 *
 */
public interface ElementDataMapperInterface {

  /**
   * Creates an Element.
   * 
   * @param name         the name of the Element.
   * @param inventory    the inventory of the Element.
   * @param atomicNumber the atomic number of the Element.
   * @param atomicMass   the atomic mass of the Element.
   * @return an Element.
   * @throws DomainModelException when things go wrong.
   */
  public Element create(String name, double inventory, int atomicNumber, double atomicMass) throws DomainModelException;

  /**
   * Load an Element from the Gateway.
   * 
   * @param id the ID of the Element.
   * @return an Element from the Gateway.
   * @throws DomainModelException when things go wrong.
   */
  public Element read(int id) throws DomainModelException;

  /**
   * Update an Element from the Gateway.
   * 
   * @param element the Element to update.
   * @throws DomainModelException when things go wrong.
   */
  public void update(Element element) throws DomainModelException;

  /**
   * Delete an Element from the Gateway.
   * 
   * @param element the Element to delete.
   * @throws DomainModelException when things go wrong.
   */
  public void delete(Element element) throws DomainModelException;

  /**
   * Get all Elements.
   * 
   * @return all Elements.
   * @throws DomainModelException when things go wrong.
   */
  public List<Element> getAll() throws DomainModelException;

  /**
   * Get all Elements that have a partial name match.
   * 
   * @param wildCard the name to partial match.
   * @return all Elements that have a partial name match.
   * @throws DomainModelException when things go wrong.
   */
  public List<Element> filterByNameLike(String nameLike) throws DomainModelException;

  /**
   * Get all Elements that have a specific inventory value.
   * 
   * @param inventory the specific inventory value.
   * @return all Elements that have a specific inventory value.
   * @throws DomainModelException when things go wrong.
   */
  public List<Element> filterByInventory(double inventory) throws DomainModelException;

  /**
   * Get all Elements that have an inventory value in a range.
   * 
   * @param min the minimum value.
   * @param max the maximum value.
   * @return all Elements that have an inventory value in a range.
   * @throws DomainModelException when things go wrong.
   */
  public List<Element> filterByInventoryBetween(double min, double max) throws DomainModelException;

  /**
   * Get all Elements with a specific atomic number.
   * 
   * @param atomicNumber the atomic number of the Element.
   * @return all Elements with a specific atomic number.
   * @throws DomainModelException when things go wrong.
   */
  public List<Element> filterByAtomicNumber(int atomicNumber) throws DomainModelException;

  /**
   * Get all Elements with a specific atomic number.
   * 
   * @param atomicNumber the atomic number of the Element.
   * @return all Elements with a specific atomic number.
   * @throws DomainModelException when things go wrong.
   */
  public List<Element> filterByAtomicNumberBetween(int min, int max) throws DomainModelException;

  /**
   * Get all Elements with a specific atomic mass.
   * 
   * @param atomicMass the atomic mass of the Element.
   * @return all Elements with a specific atomic mass.
   * @throws DomainModelException when things go wrong.
   */
  public List<Element> filterByAtomicMass(double atomicMass) throws DomainModelException;

  /**
   * Get all Elements with an atomic mass in a range.
   * 
   * @param min the minimum value.
   * @param max the maximum value.
   * @return all Elements with an atomic mass in a range.
   * @throws DomainModelException when things go wrong.
   */
  public List<Element> filterByAtomicMassBetween(double min, double max) throws DomainModelException;

  /**
   * Get all Elements that are in a Compound.
   * 
   * @param compoundID the Compound to check.
   * @return all Elements that are in a Compound.
   * @throws DomainModelException when things go wrong.
   */
  public List<Element> filterByPartOfCompound(int compoundID) throws DomainModelException;

  /**
   * Get all Elements with low inventory.
   * 
   * @return all Elements with low inventory.
   * @throws DomainModelException when things go wrong.
   */
  public List<Element> filterByLowInventory() throws DomainModelException;

}
