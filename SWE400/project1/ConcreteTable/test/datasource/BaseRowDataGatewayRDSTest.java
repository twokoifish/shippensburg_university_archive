package datasource;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
/**
 * JUnit tests for Base Gateway
 * @author JOJO
 *
 */
class BaseRowDataGatewayRDSTest extends DatabaseTest{
  /**
   * Setup.
   * @throws DatabaseException
   */
  @BeforeEach
  void setup() throws DatabaseException{
    ConcreteTableInitializer.clearMaps();
    ConcreteTableInitializer.dropTables();
    ConcreteTableInitializer.createTables();
  }
  
  /**
   * Tests constructors.
   * @throws DatabaseException
   */
  @Test
  void testConstructors() throws DatabaseException{
    ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 0, 1.0);

    BaseRowDataGateway base1 = new BaseRowDataGatewayRDS("base", 1.0, element1.getElementID(), "Element");
    BaseRowDataGateway base1FindByID = new BaseRowDataGatewayRDS(base1.getBaseID());
    BaseRowDataGateway base1FindByName = new BaseRowDataGatewayRDS("base");
    
    assertEquals(base1.getBaseID(), base1FindByID.getBaseID());
    assertEquals(base1.getName(), base1FindByID.getName());
    assertEquals(base1.getInventory(), base1FindByID.getInventory());
    assertEquals(base1.getSolute(), base1FindByID.getSolute());
    
    assertEquals(base1.getBaseID(), base1FindByName.getBaseID());
    assertEquals(base1.getName(), base1FindByName.getName());
    assertEquals(base1.getInventory(), base1FindByName.getInventory());
    assertEquals(base1.getSolute(), base1FindByName.getSolute());
  }

  /**
   * Tests getters. 
   * @throws DatabaseException
   */
  @Test
  void testGetters() throws DatabaseException{
   ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 0, 1.0);
   BaseRowDataGateway base1 = new BaseRowDataGatewayRDS("base", 1.0, element1.getElementID(), "Element");
   
   assertEquals(1, base1.getBaseID());
   assertEquals("base", base1.getName());
   assertEquals(1.0, base1.getInventory());
   assertEquals(1, base1.getSolute());
  }
  
  /**
   * Tests setters.
   * @throws DatabaseException
   */
  @Test
  void testSetters() throws DatabaseException{
    ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 0, 1.0);
    BaseRowDataGateway base1 = new BaseRowDataGatewayRDS("base", 1.0, element1.getElementID(), "Element");
    base1.setInventory(2.0);
    base1.setName("new");
    base1.setSolute(2);
    
    assertEquals("new", base1.getName());
    assertEquals(2.0, base1.getInventory());
    assertEquals(2, base1.getSolute());
  }
  
  /**
   * Tests persist.
   * @throws DatabaseException
   */
  @Test
  void testPersist() throws DatabaseException {
    ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 0, 1.0);
    BaseRowDataGateway base1 = new BaseRowDataGatewayRDS("base", 1.0, element1.getElementID(), "Element");
    base1.setName("newName");
    assertTrue(base1.persist());
    base1 = null;
    
    BaseRowDataGateway base1Copy = new BaseRowDataGatewayRDS(1);
    assertEquals("newName", base1Copy.getName());
  }
  
  /**
   * Tests delete.
   * @throws DatabaseException
   */
  @Test
  void testDelete() throws DatabaseException {
    ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 0, 1.0);
    BaseRowDataGateway base1 = new BaseRowDataGatewayRDS("base", 1.0, element1.getElementID(), "Element");
    
    assertTrue(base1.delete());
  }
}
