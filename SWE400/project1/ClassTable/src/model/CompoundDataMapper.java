package model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import database.DatabaseException;
import datasource.CompoundDTO;
import datasource.CompoundRDG;
import datasource.CompoundRDGRDS;
import datasource.CompoundTDGRDS;
import datasource.ElementDTO;

/**
 * Maps CompoundDataMapperInterface functions to class table implementation.
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class CompoundDataMapper implements CompoundDataMapperInterface {

  @Override
  public Compound create(String name, double inventory, List<Element> madeOf) throws DomainModelException {
    List<Integer> madeOfIds = new ArrayList<>(); 
    for(Element e : madeOf) {
      madeOfIds.add(e.getID());
    }
    
    CompoundRDG create = new CompoundRDGRDS(madeOfIds, name, inventory);
    // Create compound through RDG

    return new Compound(create.getCompound().getCompoundId(), name, inventory, madeOf);
    // Return the compound we made
  }

  @Override
  public Compound read(int id) throws DomainModelException {
    CompoundRDG read = new CompoundRDGRDS(id);
    // Read compound

    return new Compound(read.getCompound().getCompoundId(), read.getCompound().getName(),
        read.getCompound().getInventory(), DTOToElement(read.getCompound().getElements()));
    // Return the compound we read
  }

  @Override
  public void update(Compound compound) throws DomainModelException {
    CompoundRDG update = new CompoundRDGRDS(compound.getID());
    // Fetch the compound

    update.update(CompoundToDTO(compound));
    // Update it to all values the compound
  }

  @Override
  public void delete(Compound compound) throws DomainModelException {
    CompoundRDG delete = new CompoundRDGRDS(compound.getID());
    // Fetch the compound

    delete.delete(compound.getID());
    // Delete the compound

  }

  @Override
  public List<Compound> getAll() throws DomainModelException {
    try {
      // Use CompoundTDG to get all compounds, convert DTOs to Compounds, and return
      // that list
      return filterList(ListDTOToListCompound(new CompoundTDGRDS().getAllCompounds().executeQuery()));
    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      throw new DomainModelException("Failed getAll()", e);
    }
  }

  @Override
  public List<Compound> filterByNameLike(String wildCard) throws DomainModelException {
    try {
      // Use CompoundTDG to get all compoundDTOs filtered by name, convert DTOs to
      // Compounds, and return that list
      return filterList(ListDTOToListCompound(CompoundTDGRDS.getSingleton().filterByName(wildCard).executeQuery()));
    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      throw new DomainModelException("Failed filterByWildCardName()", e);
    }
  }

  @Override
  public List<Compound> filterByInventory(double inventory) throws DomainModelException {
    try {
      // Use CompoundTDG to get all compoundDTOs filtered by inventory, convert DTOs
      // to Compounds, return that list
      return filterList(ListDTOToListCompound(new CompoundTDGRDS().filterByInventory(inventory).executeQuery()));
    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      throw new DomainModelException("Failed filterByInventory()", e);
    }
  }

  @Override
  public List<Compound> filterByInventoryBetween(double min, double max) throws DomainModelException {
    try {
      // Use CompoundTDG to get all compounds, filter by inventory range, convert DTOs
      // to Compounds, return that list
      return filterList(ListDTOToListCompound(
          new CompoundTDGRDS().getAllCompounds().filterByInventoryRange(max, min).executeQuery()));
    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      throw new DomainModelException("Failed filterByInventoryRange()", e);
    }
  }

  @Override
  public List<Compound> filterByMadeOf(int elementID) throws DomainModelException {
    try {
      // Use CompoundTDG to get all compounds, filter by madeOf (elementId), convert
      // DTOs to Compounds, return that list
      return filterList(ListDTOToListCompound(new CompoundTDGRDS().getAllCompounds().filterByElements(elementID).executeQuery()));
    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      throw new DomainModelException("Failed filterByMadeOf()", e);
    }
  }

  /**
   * Convert a list of ElementDTOs to a list of Elements
   * 
   * @param dto List<ElementDTO> to convert
   * @return converted List<Element>
   */
  private List<Element> DTOToElement(List<ElementDTO> dto) {
    List<Element> elements = new ArrayList<>();

    // For every ElementDTO in our list dto
    for (ElementDTO e : dto) {
      // Convert it to an element and add it to our list of elements
      elements
          .add(new Element(e.getElementId(), e.getName(), e.getInventory(), e.getAtomicNumber(), e.getAtomicMass()));
    }

    // Return list of converted elements
    return elements;
  }

  /**
   * Convert a list of Elements to a list of ElementDTOs
   * 
   * @param elements List<Element> to convert
   * @return converted List<ElementDTO>
   */
  private List<ElementDTO> ElementToDTO(List<Element> elements) {
    List<ElementDTO> dto = new ArrayList<>();

    // For every Element in our list of elements
    for (Element e : elements) {
      // Convert it to a dto and add it to our list of dtos
      dto.add(new ElementDTO(e.getID(), e.getAtomicNumber(), e.getAtomicMass(), e.getName(), e.getInventory()));
    }

    // Return list of converted dtos
    return dto;

  }

  /**
   * Convert a compound to a compoundDTO
   * 
   * @param c Compound
   * @return CompoundDTO
   */
  private CompoundDTO CompoundToDTO(Compound c) {
    // Create new compoundDTO with compound information and return it.
    return new CompoundDTO(c.getID(), ElementToDTO(c.getMadeOf()), c.getName(), c.getInventory());
  }

  /**
   * Convert a list of CompoundDTOs to a list of Compounds
   * 
   * @param listCompoundDTOs list of compound DTOs to convert
   * @return converted list of compounds
   */
  private List<Compound> ListDTOToListCompound(List<CompoundDTO> listCompoundDTOs) {
    List<Compound> listCompounds = new ArrayList<>();
    // For every compoundDTO in our list of CompoundDTOs
    
    for (CompoundDTO c : listCompoundDTOs) {
      // Use that compounds information to make a compound and add it to our list
      Compound com = new Compound(c.getCompoundId(), c.getName(), c.getInventory(), DTOToElement(c.getElements()));
      System.out.println(com.getID());
      listCompounds.add(com); 
//      listCompounds.add(new Compound(c.getCompoundId(), c.getName(), c.getInventory(), DTOToElement(c.getElements())));
    }
    
    // Return list of compounds
    return filterList(listCompounds);
  }

  @Override
  public List<Compound> filterByLowInventory() throws DomainModelException {
    return null;
  }
  
  private List<Compound> filterList(List<Compound> list) {
    Set<Compound> set = new HashSet<>(list);
    
    List<Compound> filtered = new ArrayList<>(); 
    filtered.addAll(set);
    return filtered; 
    
//    List<Compound> filtered = new ArrayList<>(); 
//    
//    for (Compound c : list) {
//      if (filtered.isEmpty()) {
//        filtered.add(c);
//      } else {
//        for (Compound f : filtered) {
//          if (!(c.equals(f))) {
//            filtered.add(c);
//          }
//        }
//      }
//    }
    
//    for(Compound c : list) {
//      if(!(filtered.contains(c))) {
//        filtered.add(c);
//      }
//    }
  }
}
