package datasource;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * 
 * @author You - the viewer :) jk it was Joel and Chase
 *
 */
class AcidRowDataGatewayRDSTest extends DatabaseTest {
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
    AcidRowDataGateway acid1 = new AcidRowDataGatewayRDS("acid", 1.0, element1.getElementID(), "Element");
    AcidRowDataGateway acid1FindByID = new AcidRowDataGatewayRDS(acid1.getAcidID());
    AcidRowDataGateway acid1FindByName = new AcidRowDataGatewayRDS("acid");
    
    assertEquals(acid1.getAcidID(), acid1FindByID.getAcidID());
    assertEquals(acid1.getName(), acid1FindByID.getName());
    assertEquals(acid1.getInventory(), acid1FindByID.getInventory());
    assertEquals(acid1.getSolute(), acid1FindByID.getSolute());
    
    assertEquals(acid1.getAcidID(), acid1FindByName.getAcidID());
    assertEquals(acid1.getName(), acid1FindByName.getName());
    assertEquals(acid1.getInventory(), acid1FindByName.getInventory());
    assertEquals(acid1.getSolute(), acid1FindByName.getSolute());
  }
  
	/**
	 * Tests getters. 
	 * @throws DatabaseException
	 */
  @Test
  void testGetters() throws DatabaseException{
   ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 0, 1.0);

	 AcidRowDataGateway acid1 = new AcidRowDataGatewayRDS("acid", 1.0, element1.getElementID(), "Element");
	 
	 assertEquals(1, acid1.getAcidID());
	 assertEquals("acid", acid1.getName());
	 assertEquals(1.0, acid1.getInventory());
	 assertEquals(1, acid1.getSolute());
  }
  
  /**
   * Tests setters.
   * @throws DatabaseException
   */
  @Test
  void testSetters() throws DatabaseException{
    ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 0, 1.0);

    AcidRowDataGateway acid1 = new AcidRowDataGatewayRDS("acid", 1.0, element1.getElementID(), "Element");
    acid1.setInventory(2.0);
    acid1.setName("new");
    acid1.setSolute(2);
    
    assertEquals("new", acid1.getName());
    assertEquals(2.0, acid1.getInventory());
    assertEquals(2, acid1.getSolute());
  }
  
  /**
   * Tests persist.
   * @throws DatabaseException
   */
  @Test
  void testPersist() throws DatabaseException {
    ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 0, 1.0);

    AcidRowDataGateway acid1 = new AcidRowDataGatewayRDS("acid", 1.0, element1.getElementID(), "Element");
    acid1.setName("newName");
    assertTrue(acid1.persist());
    
    
    AcidRowDataGateway acid1Copy = new AcidRowDataGatewayRDS(acid1.getAcidID());
    assertEquals("newName", acid1Copy.getName());
  }
  
  /**
   * Tests delete.
   * @throws DatabaseException
   */
  @Test
  void testDelete() throws DatabaseException {
    ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 0, 1.0);

    AcidRowDataGateway acid1 = new AcidRowDataGatewayRDS("acid", 1.0, element1.getElementID(), "Element");
    MetalRowDataGatewayRDS.createTable();
    assertTrue(acid1.delete());
  }
}
