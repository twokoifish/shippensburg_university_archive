package datasource;

import static org.junit.Assert.assertEquals;
import org.junit.jupiter.api.Test;

/**
 * 
 * @author kimberlyoneill
 *
 */
class TestCompoundsMadeOf extends DatabaseTest {

  /**
   * Test that the getName function of compoundTDGRDS works
   */
  @Test
  static void testGetName() {
    // Fetch compounds
    CompoundRDG compound1 = new CompoundRDGRDS(23);
    CompoundRDG compound2 = new CompoundRDGRDS(24);

    // Test
    assertEquals("compoundname1", compound1.getCompound().getName());
    assertEquals("compoundname2", compound2.getCompound().getName());
  }

  /**
   * Test that the getInventory function of compoundTDGRDS works
   */
  @Test
  static void testGetInventory() {
    // Fetch compounds
    CompoundRDG compound1 = new CompoundRDGRDS(23);
    CompoundRDG compound2 = new CompoundRDGRDS(24);

    // Tests
    assertEquals(1.1, compound1.getCompound().getInventory(), 0.1);
    assertEquals(1.2, compound2.getCompound().getInventory(), 0.1);
  }

  /**
   * Run all tests in TestCompounds
   */
  static void testAll() {
    testGetName();
    testGetInventory();
  }

}
