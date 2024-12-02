package model;

import java.util.ArrayList;
import java.util.List;

import database.DatabaseException;
import datasource.MetalDTO;
import datasource.MetalRDG;
import datasource.MetalRDGRDS;
import datasource.MetalTDGRDS;

/**
 * Maps MetalDataMapperInterface functions to class table implementation.
 *  
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class MetalDataMapper implements MetalDataMapperInterface {

  @Override
  public Metal create(String name, double inventory, int atomicNumber, double atomicMass, double acidAmount)
      throws DomainModelException {
    MetalRDG row = new MetalRDGRDS(-1, atomicNumber, atomicMass, acidAmount, name, inventory);
    return convertFromDTO(row.getMetal());
  }

  @Override
  public Metal read(int id) {
    MetalRDG row = new MetalRDGRDS(id);
    MetalDTO dto = row.getMetal();
    Metal metal = convertFromDTO(dto);
    return metal;
  }

  @Override
  public void update(Metal metal) {
    MetalRDG row = new MetalRDGRDS(metal.getID());
    row.setName(metal.getName());
    row.setInventory(metal.getInventory());
    row.setMoles(metal.getAcidAmount());
    row.setAtomicNumber(metal.getAtomicNumber());
    row.setAtomicMass(metal.getAtomicMass());
    row.update();

  }

  @Override
  public void delete(Metal metal) {
    MetalRDG row = new MetalRDGRDS(metal.getID());
    row.delete();

  }

  @Override
  public List<Metal> getAll() {
    List<MetalDTO> dtos;
    List<Metal> metal = new ArrayList<>();
    try {
      dtos = MetalTDGRDS.getSingleton().executeQuery();
      
      for(MetalDTO e : dtos) {
        metal.add(convertFromDTO(e));
      }
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
    return metal;
  }

  @Override
  public List<Metal> filterByNameLike(String wildCardName) {
    List<MetalDTO> dtos;
    ArrayList<Metal> metal = new ArrayList<>();
    try {
      dtos = MetalTDGRDS.getSingleton().filterByName(wildCardName).executeQuery();
      for(MetalDTO m : dtos) {
        metal.add(convertFromDTO(m));
      }
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
    return metal;
  }

  @Override
  public List<Metal> filterByInventory(double inventory) {
    List<MetalDTO> dtos;
    ArrayList<Metal> metal = new ArrayList<>();
    try {
      dtos = MetalTDGRDS.getSingleton().filterByInventory(inventory).executeQuery();
      for(MetalDTO m : dtos) {
        metal.add(convertFromDTO(m));
      }
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
    return metal;
  }

  @Override
  public List<Metal> filterByInventoryBetween(double min, double max) {
    List<MetalDTO> dtos;
    ArrayList<Metal> metal = new ArrayList<>();
    try {
      dtos = MetalTDGRDS.getSingleton().filterByInventoryRange(max, min).executeQuery();
      for(MetalDTO m : dtos) {
        metal.add(convertFromDTO(m));
      }
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
    return metal;
  }

  @Override
  public List<Metal> filterByAtomicNumber(int atomicNumber) {
    List<MetalDTO> dtos;
    ArrayList<Metal> metal = new ArrayList<>();
    try {
      dtos = MetalTDGRDS.getSingleton().filterByAtomicNumber(atomicNumber).executeQuery();
      for(MetalDTO m : dtos) {
        metal.add(convertFromDTO(m));
      }
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
    return metal;
  }

  @Override
  public List<Metal> filterByAtomicMass(double atomicMass) {
    List<MetalDTO> dtos;
    ArrayList<Metal> metal = new ArrayList<>();
    try {
      dtos = MetalTDGRDS.getSingleton().filterByAtomicMass(atomicMass).executeQuery();
      for(MetalDTO m : dtos) {
        metal.add(convertFromDTO(m));
      }
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
    return metal;
  }

  @Override
  public List<Metal> filterByAtomicMassBetween(double min, double max) {
    List<MetalDTO> dtos;
    ArrayList<Metal> metal = new ArrayList<>();
    try {
      dtos = MetalTDGRDS.getSingleton().filterByAtomicMassRange(max, min).executeQuery();
      for(MetalDTO m : dtos) {
        metal.add(convertFromDTO(m));
      }
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
    return metal;
  }

  @Override
  public List<Metal> filterByAcidAmount(double acidRequired) {
    List<MetalDTO> dtos;
    ArrayList<Metal> metal = new ArrayList<>();
    try {
      dtos = MetalTDGRDS.getSingleton().filterByMoles(acidRequired).executeQuery();
      for(MetalDTO m : dtos) {
        metal.add(convertFromDTO(m));
      }
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
    return metal;
  }

  @Override
  public List<Metal> filterByAcidAmountBetween(double min, double max) {
    List<MetalDTO> dtos;
    ArrayList<Metal> metal = new ArrayList<>();
    try {
      dtos = MetalTDGRDS.getSingleton().filterByMolesRange(max, min).executeQuery();
      for(MetalDTO m : dtos) {
        metal.add(convertFromDTO(m));
      }
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
    return metal;
  }

  @Override
  public List<Metal> filterByDissolvedBy(int acidID) {
    List<MetalDTO> dtos;
    ArrayList<Metal> metal = new ArrayList<>();
    try {
      dtos = MetalTDGRDS.getSingleton().filterByDissolvedBy(acidID).executeQuery();
      for(MetalDTO m : dtos) {
        metal.add(convertFromDTO(m));
      }
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
    return metal;
  }

  public Metal convertFromDTO(MetalDTO dto) {
    return new Metal(dto.getMetalId(), dto.getName(), dto.getInventory(), dto.getAtomicNumber(), dto.getAtomicMass(),
        dto.getMoles());
  }

  @Override
  public List<Metal> filterByPartOfCompound(int compoundID) throws DomainModelException {
    CompoundDataMapperInterface compoundMapper = new CompoundDataMapper();
    Compound compound = compoundMapper.read(compoundID);
    List<Element> madeOf = compound.getMadeOf();
    List<Metal> filtered = new ArrayList<>();
    for(Element e : madeOf) {
      if(e.getClass() == Metal.class) {
        filtered.add((Metal) e);
      }
    }
    return filtered;
  }

  @Override
  public List<Metal> filterByAtomicNumberBetween(int min, int max) throws DomainModelException {
    // we dont need this 
    return null;
  }

  @Override
  public List<Metal> filterByLowInventory() throws DomainModelException {
    return null;
  }

}
