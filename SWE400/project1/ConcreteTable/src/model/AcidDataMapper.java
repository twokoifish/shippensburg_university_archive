package model;

import java.util.ArrayList;

import java.util.List;

import datadto.AcidDTO;
import datasource.AcidRowDataGateway;
import datasource.AcidRowDataGatewayRDS;
import datasource.AcidTableDataGatewayRDS;
import datasource.DatabaseException;
import datasource.MetalRowDataGateway;
import datasource.MetalRowDataGatewayRDS;
import model.AcidDataMapperInterface;

public class AcidDataMapper implements AcidDataMapperInterface {
  public static IdentityMap<Acid> acidMap = new IdentityMap<Acid>();

  @Override
  public Acid create(String name, double inventory, List<Metal> dissolves, Chemical solute)
      throws DomainModelException {
    try {
      AcidRowDataGateway gateway = new AcidRowDataGatewayRDS(name, inventory, solute.getID(),
          solute.getClass().getName());
      Acid a = new Acid(gateway.getAcidID(), name, inventory, dissolves, solute);
      acidMap.add(a);
      for (Metal m : dissolves) {
        MetalRowDataGateway mRDG = new MetalRowDataGatewayRDS(m.getID());
        mRDG.setDissolvedBy(a.getID());
        mRDG.persist();
      }
      return a;
    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  @Override
  public Acid read(int id) throws DomainModelException {

    try {
      if (acidMap.get(id) == null) {
        AcidRowDataGateway gateway = new AcidRowDataGatewayRDS(id);
        MetalDataMapper metalMapper = new MetalDataMapper();

        Acid acid = new Acid(gateway.getAcidID(), gateway.getName(), gateway.getInventory(),
            metalMapper.filterByDissolvedBy(id), new ChemicalSoluteGhost(gateway.getSolute(), gateway.getSoluteType()));

        acidMap.add(acid);
        return acid;
      } else {
        return acidMap.get(id);
      }

    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      // e.printStackTrace();
    }
    return null;
  }

  @Override
  public void update(Acid acid) throws DomainModelException {
    try {
      AcidRowDataGateway gateway = new AcidRowDataGatewayRDS(acid.getID());
      gateway.setName(acid.getName());
      gateway.setInventory(acid.getInventory());
      gateway.setSolute(acid.getSolute().getID());
      gateway.setSoluteType(acid.getSolute().getClass().getName());

      // how to update dissolvedMetals???
      MetalDataMapper metalMapper = new MetalDataMapper();
      for (Metal m : acid.getDissolves()) {
        MetalRowDataGateway metalGate = new MetalRowDataGatewayRDS(m.getID());
        metalGate.setDissolvedBy(acid.getID());
        metalGate.persist();
        metalMapper.update(m);
      }

      gateway.persist();
      acidMap.replace(acid);

    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  @Override
  public void delete(Acid acid) throws DomainModelException {
    try {
      AcidRowDataGateway gateway = new AcidRowDataGatewayRDS(acid.getID());
      gateway.delete();
      acidMap.remove(acid);
    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }


  /**
   * Converts a list of AcidDTOs to a list of Acids.
   * 
   * @param acidDTOList the list of DTOs.
   * @return the converted list of acids.
   */
  public List<Acid> DTOListToAcidList(List<AcidDTO> acidDTOList) {
    List<Acid> acids = new ArrayList<Acid>();
    try {
      for (AcidDTO dto : acidDTOList) {
        int acidID = dto.getAcidID();
        String name = dto.getName();
        int soluteId = dto.getSoluteID();
        String soluteType = dto.getSoluteType();
        Chemical solute = new ChemicalSoluteGhost(soluteId, soluteType);
        MetalDataMapper metalMapper = new MetalDataMapper();
        List<Metal> dissolves = metalMapper.filterByDissolvedBy(dto.getAcidID());
        double inventory = dto.getInventory();

        Acid acid = new Acid(acidID, name, inventory, dissolves, solute);
        acids.add(acid);
        acidMap.add(acid);
      }
      return acids;
    } catch (DomainModelException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return acids;
  }

  @Override
  public List<Acid> getAll() throws DomainModelException {
    List<AcidDTO> DTOList = AcidTableDataGatewayRDS.getAll();
    return DTOListToAcidList(DTOList);
  }

  @Override
  public List<Acid> filterByNameLike(String wildCard) throws DomainModelException {
    List<AcidDTO> DTOList = AcidTableDataGatewayRDS.filterByNameLike(wildCard);
    return DTOListToAcidList(DTOList);
  }

  @Override
  public List<Acid> filterByInventory(double inventory) throws DomainModelException {
    List<AcidDTO> DTOList = AcidTableDataGatewayRDS.filterByInventory(inventory);
    return DTOListToAcidList(DTOList);
  }

  @Override
  public List<Acid> filterByInventoryBetween(double min, double max) throws DomainModelException {
    List<AcidDTO> DTOList = AcidTableDataGatewayRDS.filterByInventoryBetween(min, max);
    return DTOListToAcidList(DTOList);
  }

  @Override
  public List<Acid> filterBySolute(int chemicalID) throws DomainModelException {
    List<AcidDTO> DTOList = AcidTableDataGatewayRDS.filterBySolute(chemicalID);
    return DTOListToAcidList(DTOList);
  }

  @Override
  public List<Acid> filterByLowInventory() throws DomainModelException {
    List<AcidDTO> DTOList = AcidTableDataGatewayRDS.filterByLowInventory();
    return DTOListToAcidList(DTOList);
  }

}
