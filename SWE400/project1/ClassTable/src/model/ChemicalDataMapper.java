package model;

import java.util.ArrayList;
import java.util.List;

import database.DatabaseException;
import datasource.ChemicalDTO;
import datasource.ChemicalTDGRDS;

public class ChemicalDataMapper implements ChemicalDataMapperInterface {

  @Override
  public List<Chemical> getAll() throws DomainModelException {
    List<Chemical> chemicalObjs = new ArrayList<>();
    List<ChemicalDTO> chemicals = new ArrayList<>();
    try {
      chemicals = ChemicalTDGRDS.getSingleton().executeQuery();
      for(ChemicalDTO c : chemicals) {
        chemicalObjs.add(DTOtoObj(c));
      }
      
      return chemicalObjs;
    } catch (DatabaseException e) {
      e.printStackTrace();
      System.out.println("bad things happened in chemical data mapper. line 27. "); 
      return null; 
    }
  }

  @Override
  public List<Chemical> filterByNameLike(String nameLike) throws DomainModelException {
    List<Chemical> chemicalObjs = new ArrayList<>();
    List<ChemicalDTO> chemicals = new ArrayList<>();
    try {
      chemicals = ChemicalTDGRDS.getSingleton().filterByName(nameLike).executeQuery();
      for(ChemicalDTO c : chemicals) {
        chemicalObjs.add(DTOtoObj(c));
      }
      
      return chemicalObjs;
    } catch (DatabaseException e) {
      e.printStackTrace();
      System.out.println("bad things happened in chemical data mapper. line 37. "); 
      return null; 
    }
  }

  @Override
  public List<Chemical> filterByInventory(double inventory) throws DomainModelException {
    List<Chemical> chemicalObjs = new ArrayList<>();
    List<ChemicalDTO> chemicals = new ArrayList<>();
    try {
      chemicals = ChemicalTDGRDS.getSingleton().filterByInventory(inventory).executeQuery();
      for(ChemicalDTO c : chemicals) {
        chemicalObjs.add(DTOtoObj(c));
      }
      
      return chemicalObjs;
    } catch (DatabaseException e) {
      e.printStackTrace();
      System.out.println("bad things happened in chemical data mapper. line 55. "); 
      return null; 
    }
  }

  @Override
  public List<Chemical> filterByInventoryBetween(double min, double max) throws DomainModelException {
    List<Chemical> chemicalObjs = new ArrayList<>();
    List<ChemicalDTO> chemicals = new ArrayList<>();
    try {
      chemicals = ChemicalTDGRDS.getSingleton().filterByInventoryRange(max, min).executeQuery();
      for(ChemicalDTO c : chemicals) {
        chemicalObjs.add(DTOtoObj(c));
      }
      
      return chemicalObjs;
    } catch (DatabaseException e) {
      e.printStackTrace();
      System.out.println("bad things happened in chemical data mapper. line 73. "); 
      return null; 
    }
  }

  @Override
  public List<Chemical> filterByLowInventory() throws DomainModelException {
    return null;
  }

  @Override
  public Chemical read(int id) throws DomainModelException {
    List<Chemical> chemicals = new ArrayList<>(); 
    chemicals.addAll(new ElementDataMapper().getAll());
    chemicals.addAll(new CompoundDataMapper().getAll());
    chemicals.addAll(new BaseDataMapper().getAll());
    chemicals.addAll(new AcidDataMapper().getAll());
    
    for(Chemical c : chemicals) {
      if(c.getID() == id) {
        return c; 
      }
    }
    throw new DomainModelException(id + " not found."); 
  }
  
  private Chemical DTOtoObj(ChemicalDTO c) {
    return new ChemicalObject(c.getChemicalId(), c.getName(), c.getInventory());
  }
  
}

/**
 * sorry but i need this
 * @author Isabella
 *
 */
class ChemicalObject extends Chemical {

  public ChemicalObject(int id, String name, double inventory) {
    super(id, name, inventory);
  }
}
