package model;

import java.util.ArrayList;
import java.util.List;

import database.DatabaseException;
import datasource.CompoundDTO;
import datasource.CompoundTDGRDS;
import datasource.ElementDTO;
import datasource.ElementRDG;
import datasource.ElementRDGRDS;
import datasource.ElementTDGRDS;

/**
 * Maps ElementDataMapperInterface functions to class table implementation.
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class ElementDataMapper implements ElementDataMapperInterface {

  @Override
  public Element create(String name, double inventory, int atomicNumber, double atomicMass)
      throws DomainModelException {
    ElementRDG row = new ElementRDGRDS(atomicNumber, atomicMass, name, inventory);
    // Use ElementRDG to create the element

    return convertFromDTO(row.getElement());
    // Convert DTO to Element and return it
  }

  @Override
  public Element read(int id) {
    ElementRDG row = new ElementRDGRDS(id);
    // Use ElementRDG to fetch element

    return convertFromDTO(row.getElement());
    // Convert DTO to Element and return it
  }

  @Override
  public void update(Element element) {
    ElementRDG row = new ElementRDGRDS(element.getID());
    // Use ElementRDG to fetch element

    // Set all information to what was passed in from our Element
    row.setName(element.getName());
    row.setInventory(element.getInventory());
    row.setAtomicNumber(element.getAtomicNumber());
    row.setAtomicMass(element.getAtomicMass());

    // Update the element
    row.update();

  }

  @Override
  public void delete(Element element) {
    ElementRDG row = new ElementRDGRDS(element.getID());
    // Use ElementRDG to fetch element

    row.delete();
    // Delete the element
  }

  @Override
  public List<Element> getAll() {
    List<Element> elements = new ArrayList<>();

    try {
      List<ElementDTO> dtos = ElementTDGRDS.getSingleton().executeQuery();
      // Get all Element DTOs

      // For every ElementDTO in dtos
      for (ElementDTO e : dtos) {
        // Convert the dto to and element and add it to our list of elements
        elements.add(convertFromDTO(e));
      }

    } catch (DatabaseException e) {
      // If something goes wrong with our executeQuery()
      e.printStackTrace();
    }

    return elements;
  }

  @Override
  public List<Element> filterByNameLike(String wildCardName) {
    ArrayList<Element> element = new ArrayList<>();
    try {
      List<ElementDTO> dtos = ElementTDGRDS.getSingleton().filterByName(wildCardName).executeQuery();
      // Get all Element DTOs filtered by name

      // For every ElementDTO in dtos
      for (ElementDTO e : dtos) {
        // Convert dto to element and add it to our list of elements
        element.add(convertFromDTO(e));
      }

    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      e.printStackTrace();
    }

    return element;
  }

  @Override
  public List<Element> filterByInventory(double inventory) {
    ArrayList<Element> element = new ArrayList<>();
    try {
      List<ElementDTO> dtos = ElementTDGRDS.getSingleton().filterByInventory(inventory).executeQuery();
      // Get all ElementDTOs filtered by inventory

      // For all ElementDTOs in dtos
      for (ElementDTO e : dtos) {
        // Convert dto to element and add it to our list of elements
        element.add(convertFromDTO(e));
      }

    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      e.printStackTrace();
    }

    return element;
  }

  @Override
  public List<Element> filterByInventoryBetween(double min, double max) {
    ArrayList<Element> element = new ArrayList<>();
    try {
      List<ElementDTO> dtos = ElementTDGRDS.getSingleton().filterByInventoryRange(max, min).executeQuery();
      // Get all ElementDTOs filtered by inventory range

      // For all ElementDTOs in dtos
      for (ElementDTO e : dtos) {
        // Convert dto to element and add it to our list of elements
        element.add(convertFromDTO(e));
      }

    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      e.printStackTrace();
    }

    return element;
  }

  @Override
  public List<Element> filterByAtomicNumber(int atomicNumber) {
    ArrayList<Element> element = new ArrayList<>();
    try {
      List<ElementDTO> dtos = ElementTDGRDS.getSingleton().filterByAtomicNumber(atomicNumber).executeQuery();
      // Get all ElementDTOs filtered by atomic number

      // For all ElementDTOs in dtos
      for (ElementDTO e : dtos) {
        // Convert dto to element and add it to our list of elements
        element.add(convertFromDTO(e));
      }

    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      e.printStackTrace();
    }

    return element;
  }
  
  @Override
  public List<Element> filterByAtomicNumberBetween(int min, int max) throws DomainModelException {
    // yeah we dont need this
    return null;
  }

  @Override
  public List<Element> filterByAtomicMass(double atomicMass) {
    ArrayList<Element> element = new ArrayList<>();
    try {
      List<ElementDTO> dtos = ElementTDGRDS.getSingleton().filterByAtomicMass(atomicMass).executeQuery();
      // Get all ElementDTOs filtered by atomic mass

      // For all ElementDTOs in dtos
      for (ElementDTO e : dtos) {
        // Convert dto to element and add it to our list of elements
        element.add(convertFromDTO(e));
      }

    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      e.printStackTrace();
    }

    return element;
  }

  @Override
  public List<Element> filterByAtomicMassBetween(double min, double max) {
    ArrayList<Element> element = new ArrayList<>();
    try {
      List<ElementDTO> dtos = ElementTDGRDS.getSingleton().filterByAtomicMassRange(max, min).executeQuery();
      // Get all ElementDTOs filtered by atomic mass range

      // For all elementDTOs in dtos
      for (ElementDTO e : dtos) {
        // Convert DTO to Element and add it to our list of elements
        element.add(convertFromDTO(e));
      }

    } catch (DatabaseException e) {
      // If something goes wrong with executeQuery()
      e.printStackTrace();
    }

    return element;
  }

  @Override
  public List<Element> filterByPartOfCompound(int compoundID) throws DomainModelException {
    List<Element> elements = new ArrayList<>();

    try {
      // Get all compounds with this compound id
      List<CompoundDTO> compounds = CompoundTDGRDS.getSingleton().filterByCompoundId(compoundID).executeQuery();

      // For every compound in that list
      for (CompoundDTO c : compounds) {
        // For every elementDTO in the list of elements that the compound is made from
        for (ElementDTO e : c.getElements()) {
          // Convert to element and add to list.
          elements.add(convertFromDTO(e));
        }
      }
    } catch (DatabaseException e) {
      // Problem with executeQuery()
      e.printStackTrace();
    }

    return elements;
  }

  /**
   * Convert a DTO to an element
   * 
   * @param dto to convert
   * @return Element converted from DTO
   */
  private Element convertFromDTO(ElementDTO dto) {
    return new Element(dto.getElementId(), dto.getName(), dto.getInventory(), dto.getAtomicNumber(),
        dto.getAtomicMass());
  }

  @Override
  public List<Element> filterByLowInventory() throws DomainModelException {
    return null;
  }

}
