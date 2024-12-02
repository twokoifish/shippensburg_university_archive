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
class TestMetal {

  /**
   * Test that the getName function in MetalRDGRDS works
   */
  @Test
  static void testGetName() {
    // Fetch metals
    MetalRDG metalGet1 = new MetalRDGRDS(25);
    MetalRDG metalGet2 = new MetalRDGRDS(26);
    MetalRDG metalGet3 = new MetalRDGRDS(27);
    MetalRDG metalGet4 = new MetalRDGRDS(28);

    // Test getName
    assertEquals("metalname1", metalGet1.getMetal().getName());
    assertEquals("metalname2", metalGet2.getMetal().getName());
    assertEquals("metalname3", metalGet3.getMetal().getName());
    assertEquals("metalname4", metalGet4.getMetal().getName());
  }

  /**
   * Test that the getInventory function in MetalRDGRDS works
   */
  @Test
  static void testGetInventory() {
    // Fetch metals
    MetalRDG metalGet1 = new MetalRDGRDS(25);
    MetalRDG metalGet2 = new MetalRDGRDS(26);
    MetalRDG metalGet3 = new MetalRDGRDS(27);
    MetalRDG metalGet4 = new MetalRDGRDS(28);

    // Test getInventory
    assertEquals(41.1, metalGet1.getMetal().getInventory(), 0.1);
    assertEquals(42.1, metalGet2.getMetal().getInventory(), 0.1);
    assertEquals(43.1, metalGet3.getMetal().getInventory(), 0.1);
    assertEquals(44.1, metalGet4.getMetal().getInventory(), 0.1);
  }

  /**
   * Test that the getDissolvedBy function in MetalRDGRDS works
   */
  @Test
  static void testGetDissolvedBy() {
    // Fetch metals
    MetalRDG metalGet1 = new MetalRDGRDS(25);
    MetalRDG metalGet2 = new MetalRDGRDS(26);
    MetalRDG metalGet3 = new MetalRDGRDS(27);
    MetalRDG metalGet4 = new MetalRDGRDS(28);

    // Test that getDissolvedBy works
    assertEquals(2, metalGet1.getMetal().getDissolvedById());
    assertEquals(3, metalGet2.getMetal().getDissolvedById());
    assertEquals(4, metalGet3.getMetal().getDissolvedById());
    assertEquals(5, metalGet4.getMetal().getDissolvedById());
  }
  
  /**
   * Test the getAll function in MetalDGRDS
   */
  @Test
  static void testGetAll() {
    try {
      List<MetalDTO> get = new MetalTDGRDS().getAllMetals().executeQuery();
      
      assertEquals(5, get.size());
      assertEquals(24, get.get(0).getMetalId());
      assertEquals(25, get.get(1).getMetalId());
      assertEquals(26, get.get(2).getMetalId());
      assertEquals(27, get.get(3).getMetalId());
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }

  /**
   * 
   */
  @Test
  static void testFilterByInventory() {
    try {
      List<MetalDTO> get = new MetalTDGRDS().getAllMetals().filterByInventory(41.1).executeQuery();
    
      assertEquals(1, get.size());
      assertEquals(25, get.get(0).getMetalId());
      
      get = new MetalTDGRDS().getAllMetals().filterByInventoryRange(42, 40).executeQuery();
      
      assertEquals(1, get.size());
      assertEquals(25, get.get(0).getMetalId());
      
      get = new MetalTDGRDS().getAllMetals().filterByInventoryRange(43, 40).executeQuery();
      
      assertEquals(2, get.size());
      assertEquals(25, get.get(0).getMetalId());
      assertEquals(26, get.get(1).getMetalId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  static void testFilterByAtomicNumber() {
    try {
      List<MetalDTO> get = new MetalTDGRDS().getAllMetals().filterByAtomicNumber(1).executeQuery();
      
      assertEquals(24, get.get(0).getMetalId());
      
      get = new MetalTDGRDS().getAllMetals().filterByAtomicNumberRange(2, 0).executeQuery();
      
      assertEquals(2, get.size());
      assertEquals(24, get.get(0).getMetalId());
      assertEquals(25, get.get(1).getMetalId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  static void testFilterByAtomicMass() {
    try {
      List<MetalDTO> get = new MetalTDGRDS().getAllMetals().filterByAtomicMass(1.1).executeQuery();
      
      assertEquals(24, get.get(0).getMetalId());
      
      get = new MetalTDGRDS().getAllMetals().filterByAtomicMassRange(3, 0).executeQuery();
      
      assertEquals(2, get.size());
      assertEquals(24, get.get(0).getMetalId());
      assertEquals(25, get.get(1).getMetalId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  static void testFilterByDissolvedBy() {
    try {
      List<MetalDTO> get = new MetalTDGRDS().getAllMetals().filterByDissolvedBy(1).executeQuery();
      
      assertEquals(1, get.size());
      assertEquals(24, get.get(0).getMetalId());
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  static void testFilterByMoles() {
    try {
      List<MetalDTO> get = new MetalTDGRDS().getAllMetals().filterByMoles(11.1).executeQuery();
      
      assertEquals(1, get.size());
      assertEquals(24, get.get(0).getMetalId());
      
      get = new MetalTDGRDS().getAllMetals().filterByMolesRange(14.0, 11.0).executeQuery();
      
      assertEquals(3, get.size());
      assertEquals(24, get.get(0).getMetalId());
      assertEquals(25, get.get(1).getMetalId());
      assertEquals(26, get.get(2).getMetalId());
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  /**
   * Run all tests in TestMetal
   */
  static void testAll() {
    testGetName();
    testGetInventory();
    testGetDissolvedBy();
    testGetAll();
    testFilterByInventory();
    testFilterByAtomicMass();
    testFilterByAtomicNumber();
    testFilterByDissolvedBy();
    testFilterByMoles();
  }
  
}
