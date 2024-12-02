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
 * A mapper for Compound objects.
 * 
 * @author andrewjanuszko
 *
 */
public class CompoundDataMapper implements CompoundDataMapperInterface {

  /**
   * Empty constructor for CompoundDataMapper.
   */
  public CompoundDataMapper() {
    // EMPTY.
  }

  /**
   * @see model.CompoundDataMapperInterface#create(String, double, List).
   */
  @Override
  public Compound create(String name, double inventory, List<Element> madeOf) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(ChemicalEnum.COMPOUND.getIntValue(), name, inventory, 0,
          0, 0, 0, 0);
      final int compoundID = row.getID();
      for (Element element : madeOf) {
        row = new ChemicalRowDataGateway(element.getID());
        ElementCompoundTableDataGateway.getSingletonInstance().create(compoundID, row.getID());
      }
      return new Compound(compoundID, name, inventory, madeOf);
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to create Compound.", e);
    }
  }

  /**
   * @see model.CompoundDataMapperInterface#read(int).
   */
  @Override
  public Compound read(int id) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(id);
      if (row.getType() != ChemicalEnum.COMPOUND.getIntValue()) {
        throw new DatabaseException("ID '" + id + "' does not belong to a Compound.");
      }
      List<ChemicalDTO> elements = ElementCompoundTableDataGateway.getSingletonInstance().readElementsFromCompound(id)
          .getRelations();
      List<Element> madeOf = new ArrayList<>();
      for (ChemicalDTO element : elements) {
        madeOf.add(new Element(element.getID(), element.getName(), element.getInventory(), element.getAtomicNumber(),
            element.getAtomicMass()));
      }
      return new Compound(row.getID(), row.getName(), row.getInventory(), madeOf);
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to read Compound with ID '" + id + "'.", e);
    }
  }

  /**
   * @see model.CompoundDataMapperInterface#update(Compound).
   */
  @Override
  public void update(Compound compound) throws DomainModelException {
    try {
      List<Element> elements = new ElementDataMapper().filterByPartOfCompound(compound.getID());
      for (Element element : elements) {
        ElementCompoundTableDataGateway.getSingletonInstance().delete(compound.getID(), element.getID());
      }
      for (Element element : compound.getMadeOf()) {
        ElementCompoundTableDataGateway.getSingletonInstance().create(compound.getID(), element.getID());
      }
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(compound.getID());
      row.setName(compound.getName());
      row.setInventory(compound.getInventory());
      row.update();
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to update Compound with ID '" + compound.getID() + "'.", e);
    }
  }

  /**
   * @see model.CompoundDataMapperInterface#delete(Compound).
   */
  @Override
  public void delete(Compound compound) throws DomainModelException {
    try {
      List<Element> elements = new ElementDataMapper().filterByPartOfCompound(compound.getID());
      for (Element element : elements) {
        ElementCompoundTableDataGateway.getSingletonInstance().delete(compound.getID(), element.getID());
      }
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(compound.getID());
      row.delete();
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to delete Compound with ID '" + compound.getID() + "'.", e);
    }
  }

  /**
   * @see model.CompoundDataMapperInterface#getAll().
   */
  @Override
  public List<Compound> getAll() throws DomainModelException {
    try {
      return convertToCompound(ChemicalTableDataGateway.getSingletonInstance().getCompounds().executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Compounds.", e);
    }
  }

  /**
   * @see model.CompoundDataMapperInterface#filterByWildCardName(String).
   */
  @Override
  public List<Compound> filterByNameLike(String nameLike) throws DomainModelException {
    try {
      return convertToCompound(
          ChemicalTableDataGateway.getSingletonInstance().getCompounds().filterByNameLike(nameLike).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Compounds with a partial name of '" + nameLike + "'.", e);
    }
  }

  /**
   * @see model.CompoundDataMapperInterface#filterByInventory(double).
   */
  @Override
  public List<Compound> filterByInventory(double inventory) throws DomainModelException {
    try {
      return convertToCompound(
          ChemicalTableDataGateway.getSingletonInstance().getCompounds().filterByInventory(inventory).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Compounds with an inventory of '" + inventory + "'.", e);
    }
  }

  /**
   * @see model.CompoundDataMapperInterface#filterByInventoryRange(double,
   *      double).
   */
  @Override
  public List<Compound> filterByInventoryBetween(double min, double max) throws DomainModelException {
    try {
      return convertToCompound(ChemicalTableDataGateway.getSingletonInstance().getCompounds()
          .filterByInventoryBetween(min, max).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException(
          "Failed to get all Compounds with an inventory between  '" + min + "' < x < '" + max + "'.", e);
    }
  }

  /**
   * @see model.CompoundDataMapperInterface#filterByMadeOf(int).
   */
  @Override
  public List<Compound> filterByMadeOf(int elementID) throws DomainModelException {
    try {
      return convertToCompound(
          ElementCompoundTableDataGateway.getSingletonInstance().readCompoundsWithElement(elementID).getRelations());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Compounds that contain Element '" + elementID + "'.", e);
    }
  }

  /**
   * Converts ChemicalDTOs to a List of Compounds.
   * 
   * @param chemicals the ChemicalDTOs to convert.
   * @return a List of Compounds.
   * @throws DomainModelException when things go wrong.
   */
  private List<Compound> convertToCompound(List<ChemicalDTO> chemicals) throws DomainModelException {
    try {
      List<Compound> compounds = new ArrayList<>();
      for (ChemicalDTO dto : chemicals) {
        ChemicalRowDataGateway row = new ChemicalRowDataGateway(dto.getID());
        List<Element> madeOf = new ElementDataMapper().filterByPartOfCompound(row.getID());
        compounds.add(new Compound(row.getID(), row.getName(), row.getInventory(), madeOf));
      }
      return compounds;
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to convert ChemicalDTO to Compound.", e);
    }
  }

  /**
   * @see model.CompoundDataMapperInterface#filterByLowInventory().
   */
  @Override
  public List<Compound> filterByLowInventory() throws DomainModelException {
    try {
      List<ChemicalDTO> compounds = ChemicalTableDataGateway.getSingletonInstance().getCompounds().executeQuery();
      for (ChemicalDTO compound : compounds) {
        List<ChemicalDTO> elements = ElementCompoundTableDataGateway.getSingletonInstance().readElementsFromCompound(compound.getID()).getRelations();
        double inventoryNeeded = 0;
        for (ChemicalDTO element : elements) {
          inventoryNeeded += element.getInventory();
        }
        inventoryNeeded = inventoryNeeded / elements.size();
        if (inventoryNeeded <= compound.getInventory()) {
          compounds.remove(compound);
        }
      }
      return convertToCompound(compounds);
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Compounds with low inventory.", e);
    }
  }

}
