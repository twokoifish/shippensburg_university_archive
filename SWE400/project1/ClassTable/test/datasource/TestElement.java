package datasource;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;

import database.DatabaseException;

/**
 * 
 * @author kimberlyoneill
 *
 */
class TestElement extends DatabaseTest {

  /**
   * Test that the getAtomicNumber function in ElementRDGRDS works
   */
  @Test
  static void testGetAtomicNumber() {
    // Fetch elements
    ElementRDG elementGet1 = new ElementRDGRDS(24);
    ElementRDG elementGet2 = new ElementRDGRDS(25);
    ElementRDG elementGet3 = new ElementRDGRDS(26);
    ElementRDG elementGet4 = new ElementRDGRDS(27);

    // Test getAtomicNumber
    assertEquals(1, elementGet1.getElement().getAtomicNumber());
    assertEquals(2, elementGet2.getElement().getAtomicNumber()); 
    assertEquals(3, elementGet3.getElement().getAtomicNumber());
    assertEquals(4, elementGet4.getElement().getAtomicNumber());
  }

  /**
   * Test that the getAtomicMass function in ElementRDGRDS works
   */
  @Test
  static void testGetAtomicMass() {
    // Fetch elements
    ElementRDG elementGet1 = new ElementRDGRDS(24);
    ElementRDG elementGet2 = new ElementRDGRDS(25);
    ElementRDG elementGet3 = new ElementRDGRDS(26);
    ElementRDG elementGet4 = new ElementRDGRDS(27);

    // Test getAtomicMass
    assertEquals(1.1, elementGet1.getElement().getAtomicMass(), 0.1);
    assertEquals(2.1, elementGet2.getElement().getAtomicMass(), 0.1);
    assertEquals(3.1, elementGet3.getElement().getAtomicMass(), 0.1);
    assertEquals(4.1, elementGet4.getElement().getAtomicMass(), 0.1);
  }

  /**
   * Test that the getName function in ElementRDGRDS works
   */
  @Test
  static void testGetName() {
    // Fetch elements
    ElementRDG elementGet1 = new ElementRDGRDS(24);
    ElementRDG elementGet2 = new ElementRDGRDS(25);
    ElementRDG elementGet3 = new ElementRDGRDS(26);
    ElementRDG elementGet4 = new ElementRDGRDS(27);

    // Test getName
    assertEquals("compoundname2", elementGet1.getElement().getName());
    assertEquals("metalname1", elementGet2.getElement().getName());
    assertEquals("metalname2", elementGet3.getElement().getName());
    assertEquals("metalname3", elementGet4.getElement().getName());
  }

  /**
   * Test that the getInventory function works
   */
  @Test
  static void testGetInventory() {
    // Fetch elements
    ElementRDG elementGet1 = new ElementRDGRDS(24);
    ElementRDG elementGet2 = new ElementRDGRDS(25);
    ElementRDG elementGet3 = new ElementRDGRDS(26);
    ElementRDG elementGet4 = new ElementRDGRDS(27);

    // Test getInventory
    assertEquals(1.2, elementGet1.getElement().getInventory(), 0.1);
    assertEquals(41.1, elementGet2.getElement().getInventory(), 0.1);
    assertEquals(42.1, elementGet3.getElement().getInventory(), 0.1);
    assertEquals(43.1, elementGet4.getElement().getInventory(), 0.1);
  }
  
  /**
   * Test the getAll function in ElementTDGRDS
   */
  @Test
  static void testGetAll() {
    try {
      List<ElementDTO> get = new ElementTDGRDS().getAllElements().executeQuery();
      
      assertEquals(9, get.size());
      assertEquals(17, get.get(0).getElementId());
      assertEquals(18, get.get(1).getElementId());
      assertEquals(19, get.get(2).getElementId());
      assertEquals(20, get.get(3).getElementId());
      assertEquals(24, get.get(4).getElementId());
      assertEquals(25, get.get(5).getElementId());
      assertEquals(26, get.get(6).getElementId());
      assertEquals(27, get.get(7).getElementId());
      assertEquals(28, get.get(8).getElementId());

      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  public static void testFilterByName() {
    try {
      List<ElementDTO> get = new ElementTDGRDS().getAllElements().filterByName("elementname1").executeQuery();
      
      assertEquals(1, get.size());
      assertEquals(17, get.get(0).getElementId());
      
    } catch(DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  public static void testFilterByInventory() {
    try {
      List<ElementDTO> get = new ElementTDGRDS().getAllElements().filterByInventory(49.2).executeQuery();
      
      assertEquals(19, get.get(0).getElementId());
      
      get = new ElementTDGRDS().getAllElements().filterByInventoryRange(42, 40).executeQuery();
    
      assertEquals(18, get.get(0).getElementId());
      
      get = new ElementTDGRDS().getAllElements().filterByInventoryRange(43, 40).executeQuery();
      
      assertEquals(18, get.get(0).getElementId());
      assertEquals(25, get.get(1).getElementId());
      assertEquals(26, get.get(2).getElementId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  public static void testFilterByAtomicNumber() {
    try {
      List<ElementDTO> get = new ElementTDGRDS().getAllElements().filterByAtomicNumber(1).executeQuery();
      
      assertEquals(1, get.get(0).getAtomicNumber());
      
      get = new ElementTDGRDS().getAllElements().filterByAtomicNumberRange(2, 0).executeQuery();
      
      assertEquals(2, get.size());
      assertEquals(24, get.get(0).getElementId());
      assertEquals(25, get.get(1).getElementId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  public static void testFilterByAtomicMass() {
    try {
      List<ElementDTO> get = new ElementTDGRDS().getAllElements().filterByAtomicMass(1.1).executeQuery();
      
      assertEquals(24, get.get(0).getElementId());
      
      get = new ElementTDGRDS().getAllElements().filterByAtomicMassRange(3, 0).executeQuery();
      
      assertEquals(2, get.size());
      assertEquals(24, get.get(0).getElementId());
      assertEquals(25, get.get(1).getElementId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }

  /**
   * Run all tests in TestElement
   */
  static void testAll() {
    testGetAtomicNumber();
    testGetAtomicMass();
    testGetName();
    testGetInventory();
    testGetAll();
    testFilterByName();
    testFilterByInventory();
    testFilterByAtomicNumber();
    testFilterByAtomicMass();
  }

}
