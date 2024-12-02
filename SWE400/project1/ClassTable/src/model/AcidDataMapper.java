package model;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import database.DatabaseException;
import datasource.AcidDTO;
import datasource.AcidRDG;
import datasource.AcidRDGRDS;
import datasource.AcidTDGRDS;
import datasource.BaseRDGRDS;
import datasource.ChemicalRDG;
import datasource.ChemicalRDGRDS;
import datasource.MetalDTO;
import datasource.MetalRDG;
import datasource.MetalRDGRDS;

public class AcidDataMapper implements AcidDataMapperInterface {
  
  @Override
  public Acid create(String name, double inventory, List<Metal> dissolves, Chemical solute)
      throws DomainModelException {
    // Create acid using AcidRDG
    AcidRDG row;
    try {
      row = new AcidRDGRDS(solute.getID(), name, inventory, getSoluteType(solute.getID()));
      Acid a = convertFromDTO(row.getAcid());

      // Set dissolvedById for metals
      for (Metal m : dissolves) {
        MetalRDG metal = new MetalRDGRDS(m.getID());
        metal.setDissolvedById(a.getID());
      }

      return a;
    } catch (DomainModelException | SQLException | DatabaseException e) {
      throw new DomainModelException("Failed to create acid through data mapper");
    }
  }

  @Override
  public Acid read(int id) throws DomainModelException {
    try {
      // Use AcidRDG to read acid
      AcidRDGRDS row = new AcidRDGRDS(id);
      return convertFromDTO(row.getAcid());
    } catch (DatabaseException | SQLException e) {
      throw new DomainModelException("Problem reading acid id = " + id, e);
    }
  }

  @Override
  public void update(Acid acid) {
    try {
      // Use AcidRDG to fetch acid
      AcidRDG row = new AcidRDGRDS(acid.getID());

      // Use setters to update values
      row.setName(acid.getName());
      row.setInventory(acid.getInventory());
      if(acid.getSolute().getID() > 0) {
        row.setSolute(acid.getSolute().getID());
        row.setSoluteType(getSoluteType(acid.getSolute().getID())); 
      }

      // Update the acid
      row.update();
    } catch (SQLException | DatabaseException | DomainModelException e) {
      System.out.println("Problem updating Acid " + acid.getID());
      e.printStackTrace();
    }

  }

  @Override
  public void delete(Acid acid) {
    try {
      AcidRDG row = new AcidRDGRDS(acid.getID());
      // Use AcidRDG to fetch acid
      row.delete();
      // Delete acid
    } catch (SQLException | DatabaseException e) {
      System.out.println("Failed to delete Acid " + acid.getID());
      e.printStackTrace();
    }
  }

  @Override
  public ArrayList<Acid> getAll() {
    ArrayList<Acid> acids = new ArrayList<>();
    try {
      // Get all acids
      List<AcidDTO> dtos = AcidTDGRDS.getSingleton().getAllAcids().executeQuery();
      // For all acids, convert dto to acid and add to list
      for (AcidDTO a : dtos) {
        acids.add(convertFromDTO(a));
      }
    } catch (DatabaseException e) {
      // Problem with executeQuery()
      e.printStackTrace();
    }

    return acids;
  }

  @Override
  public ArrayList<Acid> filterByNameLike(String wildCardName) {
    ArrayList<Acid> acids = new ArrayList<>();
    try {
      // Get all acids with specific name
      List<AcidDTO> dtos = AcidTDGRDS.getSingleton().filterByName(wildCardName).executeQuery();
      // For all acids, convert dto to acid and add to list
      for (AcidDTO a : dtos) {
        acids.add(convertFromDTO(a));
      }
    } catch (DatabaseException e) {
      // Problem with executeQuery()
      e.printStackTrace();
    }

    return acids;
  }

  @Override
  public ArrayList<Acid> filterByInventory(double inventory) {
    ArrayList<Acid> acids = new ArrayList<>();
    try {
      // Get all acids with specific inventory value
      List<AcidDTO> dtos = AcidTDGRDS.getSingleton().filterByInventory(inventory).executeQuery();
      // For all acids, convert dto to acid and add to list
      for (AcidDTO a : dtos) {
        acids.add(convertFromDTO(a));
      }
    } catch (DatabaseException e) {
      // Problem with executeQuery()
      e.printStackTrace();
    }

    return acids;
  }

  @Override
  public ArrayList<Acid> filterByInventoryBetween(double min, double max) {
    ArrayList<Acid> acids = new ArrayList<>();
    try {
      // Get all acids with specific inventory range
      List<AcidDTO> dtos = AcidTDGRDS.getSingleton().filterByInventoryRange(max, min).executeQuery();
      // For all acids, convert dto to acid and add to list
      for (AcidDTO a : dtos) {
        acids.add(convertFromDTO(a));
      }
    } catch (DatabaseException e) {
      // Problem with executeQuery()
      e.printStackTrace();
    }

    return acids;
  }

  @Override
  public ArrayList<Acid> filterBySolute(int chemicalID) {
    ArrayList<Acid> acids = new ArrayList<>();
    try {
      // Get all acids with specific solute id
      List<AcidDTO> dtos = AcidTDGRDS.getSingleton().filterBySolute(chemicalID).executeQuery();
      // Get all acids
      for (AcidDTO a : dtos) {
        acids.add(convertFromDTO(a));
      }
    } catch (DatabaseException e) {
      // Problem with executeQuery()
      e.printStackTrace();
    }

    return acids;
  }

  /**
   * Convert dto to acid
   * 
   * @param dto to convert
   * @return converted acid
   */
  private Acid convertFromDTO(AcidDTO dto) {
    ArrayList<MetalDTO> metals = AcidTDGRDS.getMetals(dto.getAcidId());
    ArrayList<Metal> betterMetals = new ArrayList<>();
    for (MetalDTO m : metals) {
      betterMetals.add(new Metal(m.getMetalId(), m.getName(), m.getInventory(), m.getAtomicNumber(), m.getAtomicMass(),
          m.getMoles()));
    }
    // id name inv
    try {
      return new Acid(dto.getAcidId(), dto.getName(), dto.getInventory(), betterMetals, makeSolute(dto.getSoluteId()));
    } catch (SQLException | DatabaseException e) {
      // yea
      e.printStackTrace();
      return null; 
    }
  }
  
  private Solute makeSolute(int id) throws SQLException, DatabaseException {
    ChemicalRDG c = new ChemicalRDGRDS(id); 
    return new Solute(c.getChemical().getChemicalId(), c.getChemical().getName(), c.getChemical().getInventory());
  }
  

  @Override
  public List<Acid> filterByLowInventory() throws DomainModelException {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @param s Solute Type
   * @param i ID
   * @return Solute
   * @throws DatabaseException 
   * @throws SQLException 
   */
  private int getSoluteType(int id) throws DomainModelException, SQLException, DatabaseException {
    int type = new ChemicalRDGRDS(id).getChemical().getSoluteType(); 
      switch (type) {
      case (0): 
        return 0; 
      case (1): // acid
        return new AcidRDGRDS(id).getAcid().getSoluteType();
      case (2): // base
        return new BaseRDGRDS(id).getBase().getSoluteType(); 
      case (3): // compound
        return new ChemicalRDGRDS(id).getChemical().getSoluteType(); 
      case (4): // element
        return new ChemicalRDGRDS(id).getChemical().getSoluteType();
      case (5): // metal
        return new ChemicalRDGRDS(id).getChemical().getSoluteType();
      default:
        // MetalDataMapper e = new MetalDataMapper(); 
        throw new DomainModelException("Bad solute type"); 
      } 
  }
  
}

class Solute extends Chemical {
  public Solute(int id, String name, double inventory) {
    super(id, name, inventory);
  }
}
