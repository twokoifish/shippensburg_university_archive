package model;

import java.util.ArrayList;
import java.util.List;

import datadto.ElementDTO;
import datasource.DatabaseException;
import datasource.ElementRowDataGateway;
import datasource.ElementRowDataGatewayRDS;
import datasource.ElementTableDataGatewayRDS;

public class ElementDataMapper implements ElementDataMapperInterface {
  public static IdentityMap<Element> elementMap = new IdentityMap<Element>();

  @Override
  public Element create(String name, double inventory, int atomicNumber, double atomicMass)
      throws DomainModelException {
    try {
      ElementRowDataGateway gateway = new ElementRowDataGatewayRDS(name, inventory, atomicNumber, atomicMass);
      Element e = new Element(gateway.getElementID(), name, inventory, atomicNumber, atomicMass);
      elementMap.add(e);
      return e;
    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  @Override
  public Element read(int id) throws DomainModelException {
    try {
      if (elementMap.get(id) == null) {
        ElementRowDataGateway gateway = new ElementRowDataGatewayRDS(id);
        Element element = new Element(gateway.getElementID(), gateway.getName(), gateway.getInventory(),
            gateway.getAtomicNumber(), gateway.getAtomicMass());

        elementMap.add(element);
        return element;
      } else {
        return elementMap.get(id);
      }

    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      // e.printStackTrace();
    }
    return null;
  }

  @Override
  public void update(Element element) throws DomainModelException {
    try {
      if (element instanceof Metal) {
        MetalDataMapper mMapper = new MetalDataMapper();
        mMapper.update((Metal) element);
      } else {
        ElementRowDataGateway gateway = new ElementRowDataGatewayRDS(element.getID());
        gateway.setName(element.getName());
        gateway.setInventory(element.getInventory());
        gateway.setAtomicMass(element.getAtomicMass());
        gateway.setAtomicNumber(element.getAtomicNumber());
        gateway.persist();
        elementMap.replace(element);
      }
    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }

  @Override
  public void delete(Element element) throws DomainModelException {
    try {
      if (element instanceof Metal) {
        MetalDataMapper mMapper = new MetalDataMapper();
        mMapper.delete((Metal) element);
      } else {
        ElementRowDataGateway gateway = new ElementRowDataGatewayRDS(element.getID());
        gateway.delete();
        elementMap.remove(element);
      }
    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /**
   * Converts a list of ElementDTOs to a list of Elements.
   * 
   * @param elementDTOList the list of DTOs.
   * @return the converted list of elements.
   */
  public List<Element> DTOListToElementList(List<ElementDTO> elementDTOList) throws DomainModelException {
    List<Element> elements = new ArrayList<Element>();
    for (ElementDTO dto : elementDTOList) {
      int elementID = dto.getID();
      String name = dto.getName();
      double inventory = dto.getInventory();
      int atomicNumber = dto.getAtomicNumber();
      double atomicMass = dto.getAtomicMass();

      Element element = new Element(elementID, name, inventory, atomicNumber, atomicMass);
      elements.add(element);
      elementMap.add(element);
    }
    return elements;
  }

  @Override
  public List<Element> getAll() throws DomainModelException {
    List<ElementDTO> DTOList = ElementTableDataGatewayRDS.getAll();
    MetalDataMapper mMapper = new MetalDataMapper();
    List<Element> eList = new ArrayList<Element>();
    eList.addAll(DTOListToElementList(DTOList));
    eList.addAll(mMapper.getAll());
    return eList;
  }

  @Override
  public List<Element> filterByNameLike(String nameLike) throws DomainModelException {
    List<ElementDTO> DTOList = ElementTableDataGatewayRDS.filterByNameLike(nameLike);
    MetalDataMapper metalMapper = new MetalDataMapper();
    List<Element> eList = new ArrayList<Element>();
    eList.addAll(DTOListToElementList(DTOList));
    eList.addAll(metalMapper.filterByNameLike(nameLike));
    return eList;
  }

  @Override
  public List<Element> filterByInventory(double inventory) throws DomainModelException {
    List<ElementDTO> DTOList = ElementTableDataGatewayRDS.filterByInventory(inventory);
    MetalDataMapper metalMapper = new MetalDataMapper();
    List<Element> eList = new ArrayList<Element>();
    eList.addAll(DTOListToElementList(DTOList));
    eList.addAll(metalMapper.filterByInventory(inventory));
    return eList;
  }
  
  @Override
  public List<Element> filterByLowInventory() throws DomainModelException {
    List<ElementDTO> DTOList = ElementTableDataGatewayRDS.filterByLowInventory();
    MetalDataMapper metalMapper = new MetalDataMapper();
    List<Element> eList = new ArrayList<Element>();
    eList.addAll(DTOListToElementList(DTOList));
    eList.addAll(metalMapper.filterByLowInventory());
    return eList;
  }

  @Override
  public List<Element> filterByInventoryBetween(double min, double max) throws DomainModelException {
    List<ElementDTO> DTOList = ElementTableDataGatewayRDS.filterByInventoryBetween(min, max);
    MetalDataMapper metalMapper = new MetalDataMapper();
    List<Element> eList = new ArrayList<Element>();
    eList.addAll(DTOListToElementList(DTOList));
    eList.addAll(metalMapper.filterByInventoryBetween(min, max));
    return eList;
  }

  @Override
  public List<Element> filterByAtomicNumber(int atomicNumber) throws DomainModelException {
    List<ElementDTO> DTOList = ElementTableDataGatewayRDS.filterByAtomicNumber(atomicNumber);
    MetalDataMapper metalMapper = new MetalDataMapper();
    List<Element> eList = new ArrayList<Element>();
    eList.addAll(DTOListToElementList(DTOList));
    eList.addAll(metalMapper.filterByAtomicNumber(atomicNumber));
    return eList;
  }

  @Override
  public List<Element> filterByAtomicNumberBetween(int min, int max) throws DomainModelException {
    List<ElementDTO> DTOList = ElementTableDataGatewayRDS.filterByAtomicNumberBetween(min, max);
    MetalDataMapper metalMapper = new MetalDataMapper();
    List<Element> eList = new ArrayList<Element>();
    eList.addAll(DTOListToElementList(DTOList));
    eList.addAll(metalMapper.filterByAtomicNumberBetween(min, max));
    return eList;
  }

  @Override
  public List<Element> filterByAtomicMass(double atomicMass) throws DomainModelException {
    List<ElementDTO> DTOList = ElementTableDataGatewayRDS.filterByAtomicMass(atomicMass);
    MetalDataMapper metalMapper = new MetalDataMapper();
    List<Element> eList = new ArrayList<Element>();
    eList.addAll(DTOListToElementList(DTOList));
    eList.addAll(metalMapper.filterByAtomicMass(atomicMass));
    return eList;
  }

  @Override
  public List<Element> filterByAtomicMassBetween(double min, double max) throws DomainModelException {
    List<ElementDTO> DTOList = ElementTableDataGatewayRDS.filterByAtomicMassBetween(min, max);
    MetalDataMapper metalMapper = new MetalDataMapper();
    List<Element> eList = new ArrayList<Element>();
    eList.addAll(DTOListToElementList(DTOList));
    eList.addAll(metalMapper.filterByAtomicMassBetween(min, max));
    return eList;
  }

  @Override
  public List<Element> filterByPartOfCompound(int compoundID) throws DomainModelException {
    List<ElementDTO> DTOList = ElementTableDataGatewayRDS.filterByPartOfCompound(compoundID);
    MetalDataMapper metalMapper = new MetalDataMapper();
    List<Element> eList = new ArrayList<Element>();
    eList.addAll(DTOListToElementList(DTOList));
    eList.addAll(metalMapper.filterByPartOfCompound(compoundID));
    return eList;
  }
}
