package model;

import java.util.ArrayList;
import java.util.List;
import dataDTO.ChemicalDTO;
import dataENUM.ChemicalEnum;
import datasource.ChemicalRowDataGateway;
import datasource.ChemicalTableDataGateway;
import datasource.DatabaseException;

/**
 * A mapper for Acid objects.
 * 
 * @author andrewjanuszko
 *
 */
public class AcidDataMapper implements AcidDataMapperInterface {

  /**
   * Empty constructor for AcidDataMapper.
   */
  public AcidDataMapper() {
    // EMPTY.
  }

  /**
   * @see model.AcidDataMapperInterface#create(String, double, List, int).
   */
  @Override
  public Acid create(String name, double inventory, List<Metal> dissolves, Chemical solute) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(ChemicalEnum.ACID.getIntValue(), name, inventory, 0, 0, 0,
          0, solute.getID());
      final int acidID = row.getID();
      for (Metal metal : dissolves) {
        row = new ChemicalRowDataGateway(metal.getID());
        row.setDissolvedBy(acidID);
        row.update();
      }
      return new Acid(acidID, name, inventory, dissolves, solute);
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to create an Acid object.", e);
    }

  }

  /**
   * @see model.AcidDataMapperInterface#read(int).
   */
  @Override
  public Acid read(int id) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(id);
      if (row.getType() != ChemicalEnum.ACID.getIntValue()) {
        throw new DatabaseException("ID '" + id + "' does not belong to an Acid.");
      }
      List<Metal> dissolves = new MetalDataMapper().filterByDissolvedBy(id);
      Chemical solute = new ChemicalDataMapper().read(row.getSolute());
      return new Acid(row.getID(), row.getName(), row.getInventory(), dissolves, solute);
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to read an Acid with ID '" + id + "'.", e);
    }
  }

  /**
   * @see model.AcidDataMapperInterface#update(Acid).
   */
  @Override
  public void update(Acid acid) throws DomainModelException {
    try {
      List<Metal> dissolvedBy = new MetalDataMapper().filterByDissolvedBy(acid.getID());
      for (Metal metal : dissolvedBy) {
        ChemicalRowDataGateway row = new ChemicalRowDataGateway(metal.getID());
        row.setDissolvedBy(0);
        row.update();
      }
      for (Metal metal : acid.getDissolves()) {
        ChemicalRowDataGateway row = new ChemicalRowDataGateway(metal.getID());
        row.setDissolvedBy(acid.getID());
        row.update();
      }
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(acid.getID());
      row.setName(acid.getName());
      row.setInventory(acid.getInventory());
      if (acid.getSolute() == null) {
        row.setSolute(0);
      } else {
        row.setSolute(acid.getSolute().getID());
      }
      row.update();
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to update an Acid with ID '" + acid.getID() + "'.", e);
    }
  }

  /**
   * @see model.AcidDataMapperInterface#delete(Acid).
   */
  @Override
  public void delete(Acid acid) throws DomainModelException {
    try {
      ChemicalRowDataGateway row = new ChemicalRowDataGateway(acid.getID());
      List<Metal> dissolves = new MetalDataMapper().filterByDissolvedBy(acid.getID());
      for (Metal metal : dissolves) {
        row = new ChemicalRowDataGateway(metal.getID());
        row.setDissolvedBy(0);
        row.update();
      }
      row.delete();
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to delete an Acid with ID '" + acid.getID() + "'.", e);
    }
  }

  /**
   * @see model.AcidDataMapperInterface#getAll().
   */
  @Override
  public List<Acid> getAll() throws DomainModelException {
    try {
      return convertToAcid(ChemicalTableDataGateway.getSingletonInstance().getAcids().executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Acids.", e);
    }
  }

  /**
   * @see model.AcidDataMapperInterface#filterByWildCardName(String).
   */
  @Override
  public List<Acid> filterByNameLike(String nameLike) throws DomainModelException {
    try {
      return convertToAcid(
          ChemicalTableDataGateway.getSingletonInstance().getAcids().filterByNameLike(nameLike).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Acids with name '" + nameLike + "'.", e);
    }
  }

  /**
   * @see model.AcidDataMapperInterface#filterByInventory(double).
   */
  @Override
  public List<Acid> filterByInventory(double inventory) throws DomainModelException {
    try {
      return convertToAcid(
          ChemicalTableDataGateway.getSingletonInstance().getAcids().filterByInventory(inventory).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Acids with inventory of '" + inventory + "'.", e);
    }
  }

  /**
   * @see model.AcidDataMapperInterface#filterByInventoryRange(double, double).
   */
  @Override
  public List<Acid> filterByInventoryBetween(double min, double max) throws DomainModelException {
    try {
      return convertToAcid(
          ChemicalTableDataGateway.getSingletonInstance().getAcids().filterByInventoryBetween(min, max).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException(
          "Failed to get all Acids with inventory between '" + min + "' < x < '" + max + "'.", e);
    }
  }

  /**
   * @see model.AcidDataMapperInterface#filterBySolute(int).
   */
  @Override
  public List<Acid> filterBySolute(int chemicalID) throws DomainModelException {
    try {
      return convertToAcid(
          ChemicalTableDataGateway.getSingletonInstance().getAcids().filterBySolute(chemicalID).executeQuery());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Acids with solute '" + chemicalID + "'.", e);
    }
  }

  /**
   * Converts ChemicalDTO to Acid object.
   * 
   * @param chemicals the ChemicalDTOs to convert.
   * @return a List of Acids.
   * @throws DomainModelException when things go wrong.
   */
  private List<Acid> convertToAcid(List<ChemicalDTO> chemicals) throws DomainModelException {
    List<Acid> acids = new ArrayList<>();
    for (ChemicalDTO dto : chemicals) {
      List<Metal> dissolves = new MetalDataMapper().filterByDissolvedBy(dto.getID());
      Chemical solute = new ChemicalDataMapper().read(dto.getSolute());
      acids.add(new Acid(dto.getID(), dto.getName(), dto.getInventory(), dissolves, solute));
    }
    return acids;
  }

  /**
   * @see model.AcidDataMapperInterface#filterByLowInventory().
   */
  @Override
  public List<Acid> filterByLowInventory() throws DomainModelException {
    try {
      return convertToAcid(
          ChemicalTableDataGateway.getSingletonInstance().getAcidsWithLowInventory());
    } catch (DatabaseException e) {
      throw new DomainModelException("Failed to get all Acids with low inventory.", e);
    }
  }

}
