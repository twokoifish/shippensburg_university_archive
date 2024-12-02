package model;

import java.util.ArrayList;
import java.util.List;

import datadto.MetalDTO;
import datasource.DatabaseException;
import datasource.MetalRowDataGateway;
import datasource.MetalRowDataGatewayRDS;
import datasource.MetalTableDataGatewayRDS;
import model.MetalDataMapperInterface;

public class MetalDataMapper implements MetalDataMapperInterface {
  public static IdentityMap<Metal> metalMap = new IdentityMap<Metal>();

  /**
   * @see model.MetalDataMapperInterface#create(String, double, int, double,
   *      double).
   */
  public Metal create(String name, double inventory, int atomicNumber, double atomicMass, double acidAmount)
      throws DomainModelException {
    try {
      // HOW DO I GET THE DISOLVED BY???? can it just be null?
      MetalRowDataGateway gateway = new MetalRowDataGatewayRDS(name, inventory, atomicNumber, atomicMass,
          acidAmount, -1); /// -1 = null
      Metal m = new Metal(gateway.getMetalID(), name, inventory, atomicNumber, atomicMass, acidAmount);
      metalMap.add(m);
      return m;
    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  /**
   * @see model.MetalDataMapperInterface#read(int).
   */
  public Metal read(int id) throws DomainModelException {
    try {
      if (metalMap.get(id) == null) {
        MetalRowDataGateway gateway = new MetalRowDataGatewayRDS(id);
        Metal metal = new Metal(gateway.getMetalID(), gateway.getName(), gateway.getInventory(),
            gateway.getAtomicNumber(), gateway.getAtomicMass(), gateway.getAcidAmount());

        metalMap.add(metal);
        return metal;
      } else {
        return metalMap.get(id);
      }

    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  @Override
  public void update(Metal metal) throws DomainModelException {
    try {
      MetalRowDataGateway gateway = new MetalRowDataGatewayRDS(metal.getID());
      gateway.setName(metal.getName());
      gateway.setInventory(metal.getInventory());
      gateway.setAtomicNumber(metal.getAtomicNumber());
      gateway.setAtomicMass(metal.getAtomicMass());
      gateway.setAcidAmount(metal.getAcidAmount());

      // I think dissolved by is being updated in acidMapper
      gateway.persist();
      metalMap.replace(metal);

    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }

  @Override
  public void delete(Metal metal) throws DomainModelException {
    try {
      MetalRowDataGateway gateway = new MetalRowDataGatewayRDS(metal.getID());
      gateway.delete();
      metalMap.remove(metal);
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }

  /**
   * Converts a list of MetalDTOs to a list of Metals.
   * 
   * @param metalDTOList the list of DTOs.
   * @return the converted list of metals.
   */
  public static List<Metal> DTOListToMetalList(List<MetalDTO> metalDTOList) {
    List<Metal> metals = new ArrayList<Metal>();
    for (MetalDTO dto : metalDTOList) {
      int metalID = dto.getID();
      String name = dto.getName();
      double inventory = dto.getInventory();
      int atomicNumber = dto.getAtomicNumber();
      double atomicMass = dto.getAtomicMass();
      double acidAmount = dto.getAcidAmount();

      Metal metal = new Metal(metalID, name, inventory, atomicNumber, atomicMass, acidAmount);
      metalMap.add(metal);
      metals.add(metal);
    }
    return metals;
  }

  @Override
  public List<Metal> getAll() throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.getAll();
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByDissolvedBy(int acidID) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByDissovedBy(acidID);
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByNameLike(String nameLike) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByNameLike(nameLike);
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByInventory(double inventory) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByInventory(inventory);
    return DTOListToMetalList(DTOList);
  }
  
  @Override
  public List<Metal> filterByLowInventory() throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByLowInventory();
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByInventoryBetween(double min, double max) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByInventoryBetween(min, max);
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByAtomicNumber(int atomicNumber) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByAtomicNumber(atomicNumber);
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByAtomicNumberBetween(int min, int max) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByAtomicNumberBetween(min, max);
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByAtomicMass(double atomicMass) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByAtomicMass(atomicMass);
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByAtomicMassBetween(double min, double max) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByAtomicMassBetween(min, max);
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByAcidAmount(double acidAmount) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByAcidAmount(acidAmount);
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByAcidAmountBetween(double min, double max) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByAcidAmountBetween(min, max);
    return DTOListToMetalList(DTOList);
  }

  @Override
  public List<Metal> filterByPartOfCompound(int compoundID) throws DomainModelException {
    List<MetalDTO> DTOList = MetalTableDataGatewayRDS.filterByPartOfCompound(compoundID);
    return DTOListToMetalList(DTOList);
  }
}
