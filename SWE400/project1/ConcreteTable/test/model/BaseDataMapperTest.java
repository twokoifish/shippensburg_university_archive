package model;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import datadto.BaseDTO;
import datasource.ConcreteTableInitializer;
import datasource.DatabaseException;

class BaseDataMapperTest {

  @BeforeEach
  void setup() throws DatabaseException {
    ConcreteTableInitializer.clearMaps();
    ConcreteTableInitializer.dropTables();
    ConcreteTableInitializer.createTables();
  }
  
  @Test
  void testCreate() throws DomainModelException {
    BaseDataMapper baseMapper = new BaseDataMapper();
    ElementDataMapper eMapper = new ElementDataMapper();
    Element e1 = eMapper.create("element", 1, 1, 1);
    Base base1 = baseMapper.create("name", 1.0, e1);
    Base base2 = baseMapper.read(base1.getID());

    assertEquals(base1.getID(), base2.getID());
    assertEquals(base1.getName(), base2.getName());
    assertEquals(base1.getInventory(), base2.getInventory());
    assertEquals(base1.getSolute(), base2.getSolute());
  }
  
  /**
   * Tests deleting an base.
   * @throws DomainModelException
   */
  @Test
  void testDelete() throws DomainModelException {
    BaseDataMapper baseMapper = new BaseDataMapper();
    ElementDataMapper eMapper = new ElementDataMapper();
    Element e1 = eMapper.create("element", 1, 1, 1);
    Base base1 = baseMapper.create("name", 1.0, e1);
    baseMapper.delete(base1);
    assertEquals(null, baseMapper.read(base1.getID()));
  }
  
  /**
   * Tests updataing an base.
   * @throws DomainModelException
   */
  @Test
  void testUpdate() throws DomainModelException {
    BaseDataMapper baseMapper = new BaseDataMapper();
    ElementDataMapper eMapper = new ElementDataMapper();
    Element e1 = eMapper.create("element", 1, 1, 1);
    Base base1 = baseMapper.create("name", 1.0, e1);
    base1.setName("new name");
    baseMapper.update(base1);
    
    Base base1Copy = baseMapper.read(base1.getID());
    
    assertEquals(base1.getID(),base1Copy.getID());
    assertEquals(base1.getName(), base1Copy.getName());
    assertEquals(base1.getInventory(), base1Copy.getInventory());
    assertEquals(base1.getSolute(), base1Copy.getSolute());
  }
  
  @Test
  void testDTOListToBaseList() throws DomainModelException {
    List<BaseDTO> dtoList = new ArrayList<BaseDTO>();
  
    int id = 12;
    String name = "name";
    double inventory = 1.0;
    int solute = 1;
    
    Element e1 = new Element(1,"element", 1, 1, 1);
    
    BaseDTO dto = new BaseDTO(id, name, inventory, e1.getID(), e1.getClass().getName());
    Base base = new Base(id, name, inventory, e1);
    
    dtoList.add(dto);
    
    assertEquals(dto.getBaseID(), base.getID());
    assertEquals(dto.getName(), base.getName());
    assertEquals(dto.getInventory(), base.getInventory());
    assertEquals(dto.getSoluteID(), base.getSolute().getID());
  }
  
  @Test
  void testGetAll() throws DomainModelException {
    BaseDataMapper baseMapper = new BaseDataMapper();
    ElementDataMapper eMapper = new ElementDataMapper();
    Element e1 = eMapper.create("element", 1, 1, 1);
    Base base1 = baseMapper.create("name", 1.0, e1);
    
    List<Base> result = baseMapper.getAll();
    assertEquals(base1.getID(), result.get(0).getID());
  }
  
  @Test
  void testFilterByNameLike() throws DomainModelException{
    BaseDataMapper baseMapper = new BaseDataMapper();
    ElementDataMapper eMapper = new ElementDataMapper();
    Element e1 = eMapper.create("element", 1, 1, 1);
    Base base1 = baseMapper.create("name", 1.0, e1);
        
    List<Base> result = baseMapper.filterByNameLike(base1.getName());
    assertEquals(base1.getID(), result.get(0).getID());
    
  }
  
  @Test
  void testFilterByInventory() throws DomainModelException{
    BaseDataMapper baseMapper = new BaseDataMapper();   
    ElementDataMapper eMapper = new ElementDataMapper();
    Element e1 = eMapper.create("element", 1, 1, 1);
    Base base1 = baseMapper.create("name", 1.0, e1);
    
    List<Base> result = baseMapper.filterByInventory(base1.getInventory());
    assertEquals(base1.getID(), result.get(0).getID());
    
  }
  
  @Test
  void testFilterByInventoryAmountBetween() throws DomainModelException{
    BaseDataMapper baseMapper = new BaseDataMapper();  
    ElementDataMapper eMapper = new ElementDataMapper();
    Element e1 = eMapper.create("element", 1, 1, 1);
    Base base1 = baseMapper.create("name", 1.0, e1);
    
    List<Base> result = baseMapper.filterByInventoryBetween(base1.getInventory(), base1.getInventory());
    assertEquals(base1.getID(), result.get(0).getID());
    
  }
  
  @Test
  void testFilterBySolute() throws DomainModelException{
    BaseDataMapper baseMapper = new BaseDataMapper();
    ElementDataMapper eMapper = new ElementDataMapper();
    Element e1 = eMapper.create("element", 1, 1, 1);
    Base base1 = baseMapper.create("name", 1.0, e1);
       
    List<Base> result = baseMapper.filterByInventoryBetween(base1.getInventory(), base1.getInventory());
    assertEquals(base1.getID(), result.get(0).getID());
    
  }
}
