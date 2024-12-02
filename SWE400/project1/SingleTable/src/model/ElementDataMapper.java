package model;

import java.util.ArrayList;
import java.util.List;

import dataDTO.ChemicalDTO;
import dataENUM.ChemicalEnum;
import datasource.ChemicalRowDataGateway;
import datasource.ChemicalTableDataGateway;
import datasource.DatabaseException;
import datasource.ElementCompoundTableDataGateway;

/**
 * A mapper for Element objects.
 * 
 * @author andrewjanuszko
 *
 */
public class ElementDataMapper implements ElementDataMapperInterface {

  /**
   * An empty Constructor for ElementDataMapper.
   */
  public ElementDataMapper() {
    // EMPTY
  }

  /**
   * @see model.ElementDataMapperInterface#create(String, double, int, double).
   */
  @Override
  public Element create(String name, double inventory, int atomicNumber, double atomicMass)
      throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(ChemicalEnum.ELEMENT.getIntValue(), name, inventory,
          atomicNumber, atomicMass, 0, 0, 0);
      return new Element(row.getID(), name, inventory, atomicNumber, atomicMass);
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to create an Element.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#read(int).
   */
  @Override
  public Element read(int id) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(id);
      if (row.getType() != ChemicalEnum.ELEMENT.getIntValue() && row.getType() != ChemicalEnum.METAL.getIntValue()) {
        throw new DatabaseException("ID '" + id + "' does not belong to an Element.");
      }
      return new Element(row.getID(), row.getName(), row.getInventory(), row.getAtomicNumber(), row.getAtomicMass());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to find an Element with ID '" + id + "'.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#update(Element).
   */
  @Override
  public void update(Element element) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(element.getID());
      row.setName(element.getName());
      row.setInventory(element.getInventory());
      row.setAtomicNumber(element.getAtomicNumber());
      row.setAtomicMass(element.getAtomicMass());
      row.update();
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to update an Element with ID '" + element.getID() + "'.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#delete(Element).
   */
  @Override
  public void delete(Element element) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(element.getID());
      row.delete();
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to delete an Element with ID '" + element.getID() + "'.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#getAll().
   */
  @Override
  public List<Element> getAll() throws DomainModelException {
    try {
      List<ChemicalDTO> chemicals = ChemicalTableDataGateway.getSingletonInstance().getElements().executeQuery();
      return convertToElement(chemicals);
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Elements.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#filterByWildCardName(String).
   */
  @Override
  public List<Element> filterByNameLike(String nameLike) throws DomainModelException {
    try {
      return convertToElement(
          ChemicalTableDataGateway.getSingletonInstance().getElements().filterByNameLike(nameLike).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Elements with a partial name match of '" + nameLike + "'.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#filterByInventory(double).
   */
  @Override
  public List<Element> filterByInventory(double inventory) throws DomainModelException {
    try {
      return convertToElement(
          ChemicalTableDataGateway.getSingletonInstance().getElements().filterByInventory(inventory).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Elements with an inventory value of '" + inventory + "'.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#filterByInventoryRange(double, double).
   */
  @Override
  public List<Element> filterByInventoryBetween(double min, double max) throws DomainModelException {
    try {
      return convertToElement(ChemicalTableDataGateway.getSingletonInstance().getElements()
          .filterByInventoryBetween(min, max).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException(
          "Failed to get all Elements with inventory between '" + min + "' < x < '" + max + "'.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#filterByAtomicNumber(int).
   */
  @Override
  public List<Element> filterByAtomicNumber(int atomicNumber) throws DomainModelException {
    try {
      return convertToElement(ChemicalTableDataGateway.getSingletonInstance().getElements()
          .filterByAtomicNumber(atomicNumber).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Elements with atomic number of '" + atomicNumber + "'.", e);
    }
  }

  /**
   * 
   */
  @Override
  public List<Element> filterByAtomicNumberBetween(int min, int max) throws DomainModelException {
    try {
      return convertToElement(ChemicalTableDataGateway.getSingletonInstance().getElements()
          .filterByAtomicNumberBetween(min, max).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException(
          "Failed to get all Element with atomic number between '" + min + "' < x < '" + max + "'.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#filterByAtomicMass(double).
   */
  @Override
  public List<Element> filterByAtomicMass(double atomicMass) throws DomainModelException {
    try {
      return convertToElement(
          ChemicalTableDataGateway.getSingletonInstance().getElements().filterByAtomicMass(atomicMass).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Elements with atomic mass of '" + atomicMass + "'.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#filterByAtomicMassRange(double,
   *      double).
   */
  @Override
  public List<Element> filterByAtomicMassBetween(double min, double max) throws DomainModelException {
    try {
      return convertToElement(ChemicalTableDataGateway.getSingletonInstance().getElements()
          .filterByAtomicMassBetween(min, max).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException(
          "Failed to get all Element with atomic mass between '" + min + "' < x < '" + max + "'.", e);
    }
  }

  /**
   * @see model.ElementDataMapperInterface#filterByPartOfCompound(int).
   */
  @Override
  public List<Element> filterByPartOfCompound(int compoundID) throws DomainModelException {
    try {
      return convertToElement(
          ElementCompoundTableDataGateway.getSingletonInstance().readElementsFromCompound(compoundID).getRelations());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get Elements in Compound with ID '" + compoundID + "'.", e);
    }
  }

  /**
   * Converts ChemicalDTOs to a List of Elements.
   * 
   * @param chemicals the ChemicalDTOs.
   * @return a List of Elements.
   * @throws DomainModelException when things go wrong.
   */
  private List<Element> convertToElement(List<ChemicalDTO> chemicals) throws DomainModelException {
    List<Element> elements = new ArrayList<>();
    for (ChemicalDTO chemical : chemicals) {
      elements.add(new Element(chemical.getID(), chemical.getName(), chemical.getInventory(),
          chemical.getAtomicNumber(), chemical.getAtomicMass()));
    }
    return elements;
  }

  /**
   * @see model.ElementDataMapperInterface#filterByLowInventory().
   */
  @Override
  public List<Element> filterByLowInventory() throws DomainModelException {
    try {
      return convertToElement(
          ChemicalTableDataGateway.getSingletonInstance().getElementsWithLowInventory());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get Elements with low inventory.", e);
    }
  }

}
