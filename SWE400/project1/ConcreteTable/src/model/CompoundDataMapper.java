package model;

import java.util.ArrayList;
import java.util.List;

import datadto.CompoundDTO;
import datasource.CompoundMadeOfTableDataGatewayRDS;
import datasource.CompoundRowDataGateway;
import datasource.CompoundRowDataGatewayRDS;
import datasource.CompoundTableDataGatewayRDS;
import datasource.DatabaseException;

public class CompoundDataMapper implements CompoundDataMapperInterface {
  public static IdentityMap<Compound> compoundMap = new IdentityMap<Compound>();

  @Override
  public Compound create(String name, double inventory, List<Element> madeOf) throws DomainModelException {
    // TODO distribute elements??
    ;
    try {
      CompoundRowDataGateway gateway = new CompoundRowDataGatewayRDS(name, inventory);
      Compound a = new Compound(gateway.getCompoundID(), name, inventory, madeOf);
      for(Element e: madeOf) {
        //System.out.println(e.toString());
        CompoundMadeOfTableDataGatewayRDS.addCompoundMadeOf(a.getID(), e.getID(), e.toString());
      }
      compoundMap.add(a);
      
      return a;
    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return null;
  }

  @Override
  public Compound read(int id) throws DomainModelException {
    try {
      if (compoundMap.get(id) == null) {
        CompoundRowDataGateway gateway = new CompoundRowDataGatewayRDS(id);
        ElementDataMapper elementMapper = new ElementDataMapper();
        List<Element> madeOf = elementMapper.filterByPartOfCompound(gateway.getCompoundID());
        Compound compound = new Compound(gateway.getCompoundID(), gateway.getName(), gateway.getInventory(), madeOf);
        
        compoundMap.add(compound);
        return compound;
      } else {
        return compoundMap.get(id);
      }

    } catch (DatabaseException e) {
      // e.printStackTrace();
    }
    return null;
  }

  @Override
  public void update(Compound compound) throws DomainModelException {
    try {
      CompoundRowDataGateway gateway = new CompoundRowDataGatewayRDS(compound.getID());
      gateway.setName(compound.getName());
      gateway.setInventory(compound.getInventory());

      for (Element e : compound.getMadeOf()) {
        System.out.println(e.toString());
        CompoundMadeOfTableDataGatewayRDS.addCompoundMadeOf(compound.getID(), e.getID(), e.toString());
      }
      gateway.persist();
      compoundMap.replace(compound);

    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  @Override
  public void delete(Compound compound) throws DomainModelException {
    try {
      CompoundRowDataGateway gateway = new CompoundRowDataGatewayRDS(compound.getID());
      CompoundMadeOfTableDataGatewayRDS.deleteCompound(compound.getID());
      gateway.delete();

      compoundMap.remove(compound);
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }

  public List<Compound> DTOListToCompoundList(List<CompoundDTO> compoundDTOList) {

    List<Compound> compounds = new ArrayList<Compound>();
    try {
      ElementDataMapper eMapper = new ElementDataMapper();
      for (CompoundDTO dto : compoundDTOList) {
        int compoundID = dto.getCompoundID();
        String name = dto.getName();
        double inventory = dto.getInventory();
        List<Element> madeOf = eMapper.filterByPartOfCompound(dto.getCompoundID());

        Compound compound = new Compound(compoundID, name, inventory, madeOf);
        compounds.add(compound);
        compoundMap.add(compound);
      }
    } catch (DomainModelException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return compounds;
  }

  @Override
  public List<Compound> getAll() throws DomainModelException {
    List<CompoundDTO> DTOList = CompoundTableDataGatewayRDS.getAll();
    return DTOListToCompoundList(DTOList);
  }

  @Override
  public List<Compound> filterByNameLike(String nameLike) throws DomainModelException {
    List<CompoundDTO> DTOList = CompoundTableDataGatewayRDS.filterByNameLike(nameLike);
    return DTOListToCompoundList(DTOList);
  }

  @Override
  public List<Compound> filterByInventory(double inventory) throws DomainModelException {
    List<CompoundDTO> DTOList = CompoundTableDataGatewayRDS.filterByInventory(inventory);
    return DTOListToCompoundList(DTOList);
  }
  
  @Override
  public List<Compound> filterByLowInventory() throws DomainModelException {
    List<CompoundDTO> DTOList = CompoundTableDataGatewayRDS.filterByLowInventory();
    return DTOListToCompoundList(DTOList);
  }

  @Override
  public List<Compound> filterByInventoryBetween(double min, double max) throws DomainModelException {
    List<CompoundDTO> DTOList = CompoundTableDataGatewayRDS.filterByInventoryBetween(min, max);
    return DTOListToCompoundList(DTOList);
  }

  @Override
  public List<Compound> filterByMadeOf(int elementID) throws DomainModelException {
    List<CompoundDTO> DTOList = CompoundTableDataGatewayRDS.filterByMadeOf(elementID);
    return DTOListToCompoundList(DTOList);
  }

}
