package datasource;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class ElementRowDataGatewayRDSTest extends DatabaseTest{
  
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
  
	@Test
	void testGetters() throws DatabaseException {
		ElementRowDataGateway element = new ElementRowDataGatewayRDS("element", 1.0, 5, 10.0);
		ElementRowDataGateway element2 = new ElementRowDataGatewayRDS("element2", 1.0, 5, 10.0);
		
		assertEquals(1, element.getElementID());
		assertEquals("element", element.getName());
		assertEquals(1.0, element.getInventory());
		assertEquals(5, element.getAtomicNumber());
		assertEquals(10.0, element.getAtomicMass());
		
		assertEquals(2, element2.getElementID());
    assertEquals("element2", element2.getName());
    assertEquals(1.0, element2.getInventory());
    assertEquals(5, element2.getAtomicNumber());
    assertEquals(10.0, element2.getAtomicMass());
	}
	
	@Test
	void testSetters() throws DatabaseException {
		ElementRowDataGateway element = new ElementRowDataGatewayRDS("element", 1.0, 5, 10.0);
		
		element.setName("name");
		element.setInventory(2.0);
		element.setAtomicNumber(10);
		element.setAtomicMass(20.0);
		
		assertEquals(1, element.getElementID());
		assertEquals("name", element.getName());
		assertEquals(2.0, element.getInventory());
		assertEquals(10, element.getAtomicNumber());
		assertEquals(20.0, element.getAtomicMass());
	}

	@Test
	void testDelete() throws DatabaseException {
	    ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 5, 10.0);
	    
	    assertTrue(element1.delete());
	}
	
	@Test
	void testConstructors() throws DatabaseException{
	    ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 5, 10.0);
	    ElementRowDataGateway element2 = new ElementRowDataGatewayRDS("element2", 1.0, 5, 10.0);
	   // ElementRowDataGateway element1FindByID = new ElementRowDataGatewayRDS(1);
	    ElementRowDataGateway element1FindByName = new ElementRowDataGatewayRDS("element");
	    ElementRowDataGateway element2FindByName = new ElementRowDataGatewayRDS("element2");
	    
//	    assertEquals(element1.getElementID(), element1FindByID.getElementID());
//	    assertEquals(element1.getName(), element1FindByID.getName());
//	    assertEquals(element1.getInventory(), element1FindByID.getInventory());
//	    assertEquals(element1.getAtomicNumber(), element1FindByID.getAtomicNumber());
//	    assertEquals(element1.getAtomicMass(), element1FindByID.getAtomicMass());
//	    
	    assertEquals(element1.getElementID(), element1FindByName.getElementID());
	    assertEquals(element1.getName(), element1FindByName.getName());
	    assertEquals(element1.getInventory(), element1FindByName.getInventory());
	    assertEquals(element1.getAtomicNumber(), element1FindByName.getAtomicNumber());
	    assertEquals(element1.getAtomicMass(), element1FindByName.getAtomicMass());
	    
	    assertEquals(element2.getElementID(), element2FindByName.getElementID());
      assertEquals(element2.getName(), element2FindByName.getName());
      assertEquals(element2.getInventory(), element2FindByName.getInventory());
      assertEquals(element2.getAtomicNumber(), element2FindByName.getAtomicNumber());
      assertEquals(element2.getAtomicMass(), element2FindByName.getAtomicMass());
	}
	
	@Test
	void testPersist() throws DatabaseException {
		ElementRowDataGateway element1 = new ElementRowDataGatewayRDS("element", 1.0, 5, 10.0);
	    element1.setName("newName");
	    assertTrue(element1.persist());
	    element1 = null;
	    
	    ElementRowDataGateway element1Copy = new ElementRowDataGatewayRDS("newName");
	    assertEquals("newName", element1Copy.getName());
	}
}
