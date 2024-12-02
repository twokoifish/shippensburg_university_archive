package model;

import java.util.ArrayList;
import java.util.List;
import datasource.ChemicalRowDataGateway;
import datasource.DatabaseException;

/**
 * A mapper for Chemicals.
 * 
 * @author andrewjanuszko
 *
 */
public class ChemicalDataMapper implements ChemicalDataMapperInterface {
  
  @Override
  public Chemical read(int id) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(id);
      switch (row.getType()) {
      case 1:
        return new ElementDataMapper().read(id);
      case 2:
        return new MetalDataMapper().read(id);
      case 3:
        return new CompoundDataMapper().read(id);
      case 4:
        return new BaseDataMapper().read(id);
      case 5:
        return new AcidDataMapper().read(id);
      default:
        throw new DomainModelException("Chemical is not of a valid type.");
      }
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to read a Chemical with ID '" + id + "'.", e);
    }
  }

  /**
   * @see model.ChemicalDataMapperInterface#getAll().
   */
  @Override
  public List<Chemical> getAll() throws DomainModelException {
    List<Chemical> chemicals = new ArrayList<>();
    chemicals.addAll(new ElementDataMapper().getAll());
    chemicals.addAll(new CompoundDataMapper().getAll());
    chemicals.addAll(new BaseDataMapper().getAll());
    chemicals.addAll(new AcidDataMapper().getAll());
    return chemicals;
  }

  /**
   * @see model.ChemicalDataMapperInterface#filterByNameLike(String).
   */
  @Override
  public List<Chemical> filterByNameLike(String nameLike) throws DomainModelException {
    List<Chemical> chemicals = new ArrayList<>();
    chemicals.addAll(new ElementDataMapper().filterByNameLike(nameLike));
    chemicals.addAll(new CompoundDataMapper().filterByNameLike(nameLike));
    chemicals.addAll(new BaseDataMapper().filterByNameLike(nameLike));
    chemicals.addAll(new AcidDataMapper().filterByNameLike(nameLike));
    return chemicals;
  }

  /**
   * @see model.ChemicalDataMapperInterface#filterByInventory(double).
   */
  @Override
  public List<Chemical> filterByInventory(double inventory) throws DomainModelException {
    List<Chemical> chemicals = new ArrayList<>();
    chemicals.addAll(new ElementDataMapper().filterByInventory(inventory));
    chemicals.addAll(new CompoundDataMapper().filterByInventory(inventory));
    chemicals.addAll(new BaseDataMapper().filterByInventory(inventory));
    chemicals.addAll(new AcidDataMapper().filterByInventory(inventory));
    return chemicals;
  }

  /**
   * @see model.ChemicalDataMapperInterface#filterByInventoryBetween(double,
   *      double).
   */
  @Override
  public List<Chemical> filterByInventoryBetween(double min, double max) throws DomainModelException {
    List<Chemical> chemicals = new ArrayList<>();
    chemicals.addAll(new ElementDataMapper().filterByInventoryBetween(min, max));
    chemicals.addAll(new CompoundDataMapper().filterByInventoryBetween(min, max));
    chemicals.addAll(new BaseDataMapper().filterByInventoryBetween(min, max));
    chemicals.addAll(new AcidDataMapper().filterByInventoryBetween(min, max));
    return chemicals;
  }

  @Override
  public List<Chemical> filterByLowInventory() throws DomainModelException {
    // TODO Auto-generated method stub
    return null;
  }

}
