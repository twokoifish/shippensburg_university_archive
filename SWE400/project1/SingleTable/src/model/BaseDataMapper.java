package model;

import java.util.ArrayList;
import java.util.List;
import dataDTO.ChemicalDTO;
import dataENUM.ChemicalEnum;
import datasource.ChemicalRowDataGateway;
import datasource.ChemicalTableDataGateway;
import datasource.DatabaseException;

/**
 * A mapper for Base objects.
 * 
 * @author andrewjanuszko
 *
 */
public class BaseDataMapper implements BaseDataMapperInterface {

  /**
   * Empty constructor for BaseDataMapper.
   */
  public BaseDataMapper() {
    // EMPTY.
  }

  /**
   * @see model.BaseDataMapperInterface#create(String, double, int).
   */
  @Override
  public Base create(String name, double inventory, Chemical solute) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(ChemicalEnum.BASE.getIntValue(), name, inventory, 0, 0, 0,
          0, solute.getID());
      return new Base(row.getID(), name, inventory, solute);
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to create a Base.", e);
    }
  }

  /**
   * @see model.BaseDataMapperInterface#read(int).
   */
  @Override
  public Base read(int id) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(id);
      if (row.getType() != ChemicalEnum.BASE.getIntValue()) {
        throw new DatabaseException("ID '" + id + "' does not belong to a Base.");
      }
      Chemical solute = new ChemicalDataMapper().read(row.getSolute());
      return new Base(row.getID(), row.getName(), row.getInventory(), solute);
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to read a Base with ID '" + id + "'.", e);
    }
  }

  /**
   * @see model.BaseDataMapperInterface#update(Base).
   */
  @Override
  public void update(Base base) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(base.getID());
      row.setName(base.getName());
      row.setInventory(base.getInventory());
      if (base.getSolute() == null) {
        row.setSolute(0);
      } else {
        row.setSolute(base.getSolute().getID());
      }
      row.update();
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to update a Base with ID '" + base.getID() + "'.", e);
    }
  }

  /**
   * @see model.BaseDataMapperInterface#delete(Base).
   */
  @Override
  public void delete(Base base) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(base.getID());
      row.delete();
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to delete a Base with ID '" + base.getID() + "'.", e);
    }
  }

  /**
   * @see model.BaseDataMapperInterface#getAll().
   */
  @Override
  public List<Base> getAll() throws DomainModelException {
    try {
      return convertToBase(ChemicalTableDataGateway.getSingletonInstance().getBases().executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Bases.", e);
    }
  }

  /**
   * @see model.BaseDataMapperInterface#filterByWildCardName(String).
   */
  @Override
  public List<Base> filterByNameLike(String nameLike) throws DomainModelException {
    try {
      return convertToBase(
          ChemicalTableDataGateway.getSingletonInstance().getBases().filterByNameLike(nameLike).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Bases with name '" + nameLike + "'.", e);
    }
  }

  /**
   * @see model.BaseDataMapperInterface#filterByInventory(double).
   */
  @Override
  public List<Base> filterByInventory(double inventory) throws DomainModelException {
    try {
      return convertToBase(
          ChemicalTableDataGateway.getSingletonInstance().getBases().filterByInventory(inventory).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Bases with inventory of '" + inventory + "'.", e);
    }
  }

  /**
   * @see model.BaseDataMapperInterface#filterByInventoryRange(double, double).
   */
  @Override
  public List<Base> filterByInventoryBetween(double min, double max) throws DomainModelException {
    try {
      return convertToBase(
          ChemicalTableDataGateway.getSingletonInstance().getBases().filterByInventoryBetween(min, max).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException(
          "Failed to get all Bases with inventory between '" + min + "' < x < '" + max + "'.", e);
    }
  }

  /**
   * @see model.BaseDataMapperInterface#filterBySolute(int).
   */
  @Override
  public List<Base> filterBySolute(int chemicalID) throws DomainModelException {
    try {
      return convertToBase(
          ChemicalTableDataGateway.getSingletonInstance().getBases().filterBySolute(chemicalID).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Bases with solute '" + chemicalID + "'.", e);
    }
  }

  /**
   * Converts a ChemicalDTO to a Base.
   * 
   * @param chemicalDTOs the List of ChemicalDTO to convert.
   * @return a List of Bases.
   */
  private List<Base> convertToBase(List<ChemicalDTO> chemicals) throws DomainModelException {
    ArrayList<Base> bases = new ArrayList<Base>();
    for (ChemicalDTO chemical : chemicals) {
      Chemical solute = new ChemicalDataMapper().read(chemical.getSolute());
      bases.add(new Base(chemical.getID(), chemical.getName(), chemical.getInventory(), solute));
    }
    return bases;
  }

  /**
   * @see model.BaseDataMapperInterface#filterByLowInventory().
   */
  @Override
  public List<Base> filterByLowInventory() throws DomainModelException {
    try {
      return convertToBase(
          ChemicalTableDataGateway.getSingletonInstance().getBasesWithLowInventory());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Bases with low inventory.", e);
    }
  }

}
