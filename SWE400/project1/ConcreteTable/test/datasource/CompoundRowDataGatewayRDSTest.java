package datasource;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class CompoundRowDataGatewayRDSTest extends DatabaseTest{
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
   * @throws DatacompoundException
   */
  @Test
  void testConstructors() throws DatabaseException{
    CompoundRowDataGateway compound1 = new CompoundRowDataGatewayRDS("compound", 1.0);
    CompoundRowDataGateway compound1FindByID = new CompoundRowDataGatewayRDS(1);
    CompoundRowDataGateway compound1FindByName = new CompoundRowDataGatewayRDS("compound");
    
    assertEquals(compound1.getCompoundID(), compound1FindByID.getCompoundID());
    assertEquals(compound1.getName(), compound1FindByID.getName());
    assertEquals(compound1.getInventory(), compound1FindByID.getInventory());
        
    assertEquals(compound1.getCompoundID(), compound1FindByName.getCompoundID());
    assertEquals(compound1.getName(), compound1FindByName.getName());
    assertEquals(compound1.getInventory(), compound1FindByName.getInventory());
  }

  /**
   * Tests getters. 
   * @throws DatacompoundException
   */
  @Test
  void testGetters() throws DatabaseException{
   CompoundRowDataGateway compound1 = new CompoundRowDataGatewayRDS("compound", 1.0);
   
   assertEquals(1, compound1.getCompoundID());
   assertEquals("compound", compound1.getName());
   assertEquals(1.0, compound1.getInventory());
  }
  
  /**
   * Tests setters.
   * @throws DatacompoundException
   */
  @Test
  void testSetters() throws DatabaseException{
    CompoundRowDataGateway compound1 = new CompoundRowDataGatewayRDS("compound", 1.0);
    compound1.setInventory(2.0);
    compound1.setName("new");
    
    assertEquals("new", compound1.getName());
    assertEquals(2.0, compound1.getInventory());
  }
  
  /**
   * Tests persist.
   * @throws DatacompoundException
   */
  @Test
  void testPersist() throws DatabaseException {
    CompoundRowDataGateway compound1 = new CompoundRowDataGatewayRDS("compound", 1.0);
    compound1.setName("newName");
    compound1.persist();
    
    CompoundRowDataGateway compound1Copy = new CompoundRowDataGatewayRDS(compound1.getCompoundID());
    assertEquals("newName", compound1Copy.getName());
  }
  
  /**
   * Tests delete.
   * @throws DatacompoundException
   */
  @Test
  void testDelete() throws DatabaseException {
    CompoundRowDataGateway compound1 = new CompoundRowDataGatewayRDS("compound", 1.0);
    
    assertTrue(compound1.delete());
  }

}
