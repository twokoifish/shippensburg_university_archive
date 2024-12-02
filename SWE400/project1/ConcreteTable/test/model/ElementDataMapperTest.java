package model;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import datadto.ElementDTO;
import datasource.ConcreteTableInitializer;
import datasource.DatabaseException;

class ElementDataMapperTest {

  @BeforeEach
  void setup() throws DatabaseException {
    ConcreteTableInitializer.clearMaps();
    ConcreteTableInitializer.dropTables();
    ConcreteTableInitializer.createTables();
    
  }
  
  @Test
  void testCreate() throws DomainModelException {
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name", 1.0, 1, 1.0);
    Element element2 = mapper.read(element1.getID());

    assertEquals(element1.getID(), element2.getID());
    assertEquals(element1.getName(), element2.getName());
    assertEquals(element1.getInventory(), element2.getInventory());
    assertEquals(element1.getAtomicNumber(), element2.getAtomicNumber());
    assertEquals(element1.getAtomicMass(), element2.getAtomicMass());
  }
  
  @Test
  void testDelete() throws DomainModelException {
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name", 1.0, 1, 1);

    mapper.delete(element1);

    assertEquals(null, mapper.read(element1.getID()));
  }
  
  @Test
  void testUpdate() throws DomainModelException {
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name", 1.0, 1, 1.0);
    element1.setName("new name");
    mapper.update(element1);
    Element element1Copy = mapper.read(element1.getID());
    assertEquals(element1.getID(), element1Copy.getID());
  }
  
  @Test
  void testDTOListToAcidList() throws DomainModelException {
    List<ElementDTO> dtoList = new ArrayList<ElementDTO>();

    int id = 12;
    String name = "name";
    double inventory = 1.0;
    int atomicNumber = 1;
    double atomicMass = 1.0;

    ElementDTO dto = new ElementDTO(id, name, inventory, atomicNumber, atomicMass);
    Element element = new Element(id, name, inventory, atomicNumber, atomicMass);
    dtoList.add(dto);

    assertEquals(dto.getID(), element.getID());
  }
  
  @Test
  void testGetAll() throws DomainModelException {
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name1", 1.0, 1, 1.0);
    Element element2 = mapper.create("name2", 1.0, 1, 1.0);
    List<Element> result = mapper.getAll();

    assertEquals(element1.getID(), result.get(0).getID());
    assertEquals(element2.getID(), result.get(1).getID());
  }
  
  @Test
  void testFilterByNameLike() throws DomainModelException{
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name1", 1.0, 1, 1.0);
    
    List<Element> result = mapper.filterByNameLike(element1.getName());
    assertEquals(element1.getID(), result.get(0).getID());
    
  }
  
  @Test
  void testFilterByInventory() throws DomainModelException{
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name1", 1.0, 1, 1.0);
    
    List<Element> result = mapper.filterByInventory(element1.getInventory());
    assertEquals(element1.getID(), result.get(0).getID());
    
  }
  
  @Test
  void testFilterByInventoryBetween() throws DomainModelException{
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name1", 1.0, 1, 1.0);
    
    List<Element> result = mapper.filterByInventoryBetween(element1.getInventory(), element1.getInventory());
    assertEquals(element1.getID(), result.get(0).getID());
    
  }
  
  @Test
  void testFilterByAtomicNumber() throws DomainModelException{
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name1", 1.0, 1, 1.0);
    
    List<Element> result = mapper.filterByAtomicNumber(element1.getAtomicNumber());
    assertEquals(element1.getID(), result.get(0).getID());
    
  }
  
  @Test
  void testFilterByAtomicNumberBetween() throws DomainModelException{
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name1", 1.0, 1, 1.0);
    
    List<Element> result = mapper.filterByAtomicNumberBetween(element1.getAtomicNumber(), element1.getAtomicNumber());
    assertEquals(element1.getID(), result.get(0).getID());
    
  }
  
  @Test
  void testFilterByAtomicMass() throws DomainModelException{
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name1", 1.0, 1, 1.0);
    
    List<Element> result = mapper.filterByAtomicMass(element1.getAtomicMass());
    assertEquals(element1.getID(), result.get(0).getID());
    
  }
  
  @Test
  void testFilterByAtomicMassBetween() throws DomainModelException{
    ElementDataMapper mapper = new ElementDataMapper();
    Element element1 = mapper.create("name1", 1.0, 1, 1.0);
    
    List<Element> result = mapper.filterByAtomicMassBetween(element1.getAtomicMass(), element1.getAtomicMass());
    assertEquals(element1.getID(), result.get(0).getID());
    
  }
  
  @Test
  void testFilterByPartOfCompound() throws DomainModelException{
    ElementDataMapper mapper = new ElementDataMapper();
    CompoundDataMapper mapperC = new CompoundDataMapper();
    Element element1 = mapper.create("name1", 1.0, 1, 1.0);
    List<Element> elList = new ArrayList<Element>();
    elList.add(element1);
    Compound compound1 = mapperC.create("name", 1.0, elList);
    
    List<Element> result = mapper.filterByPartOfCompound(compound1.getID());
    assertEquals(element1.getID(), result.get(0).getID());
    
  }

}
