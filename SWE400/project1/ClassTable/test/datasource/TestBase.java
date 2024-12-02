package datasource;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.sql.SQLException;
import java.util.List;

import org.junit.jupiter.api.Test;

import database.DatabaseException;

/**
 * 
 * @author Isabella Boone
 *
 */
class TestBase extends DatabaseTest {

  /**
   * Test that the getName function in BaseRDGRDS works
   * 
   * @throws SQLException
   * @throws DatabaseException
   */
  @Test
  static void testGetName() throws SQLException, DatabaseException {
    // Fetch bases
    BaseRDG base1 = new BaseRDGRDS(11), base2 = new BaseRDGRDS(12), base3 = new BaseRDGRDS(13),
        base4 = new BaseRDGRDS(14);

    // Tests
    assertEquals("basename1", base1.getBase().getName());
    assertEquals("basename2", base2.getBase().getName());
    assertEquals("basename3", base3.getBase().getName());
    assertEquals("basename4", base4.getBase().getName());
  }

  /**
   * Test that the getInventory function in BaseRDGRDS works
   * 
   * @throws SQLException
   * @throws DatabaseException
   */
  @Test
  static void testGetInventory() throws SQLException, DatabaseException {
    // Fetch bases
    BaseRDG base1 = new BaseRDGRDS(11), base2 = new BaseRDGRDS(12), base3 = new BaseRDGRDS(13),
        base4 = new BaseRDGRDS(14);

    // Tests
    assertEquals(1.1, base1.getBase().getInventory(), 0.1);
    assertEquals(1.2, base2.getBase().getInventory(), 0.1);
    assertEquals(1.3, base3.getBase().getInventory(), 0.1);
    assertEquals(1.4, base4.getBase().getInventory(), 0.1);
  }

  /**
   * Test that the getSolute function in BaseRDGRDS works
   * 
   * @throws SQLException
   * @throws DatabaseException
   */
  @Test
  static void testGetSolute() throws SQLException, DatabaseException {
    // Fetch bases
    BaseRDG base1 = new BaseRDGRDS(11), base2 = new BaseRDGRDS(12), base3 = new BaseRDGRDS(13),
        base4 = new BaseRDGRDS(14);

    // Tests
    assertEquals(1, base1.getBase().getSoluteId());
    assertEquals(1, base2.getBase().getSoluteId());
    assertEquals(2, base3.getBase().getSoluteId());
    assertEquals(2, base4.getBase().getSoluteId());
  }

  /**
   * Test that the update function in BaseRDGRDS works
   * 
   * @throws SQLException
   * @throws DatabaseException
   */
  @Test
  static void testUpdate() throws SQLException, DatabaseException {
    // Create a new base and getter for the base
    BaseRDG base_setter = new BaseRDGRDS(59, "basename9", 1.9, "base"), base_getter = new BaseRDGRDS(37);

    // Ensure the base has been added properly
    assertEquals("basename9", base_getter.getBase().getName());
    assertEquals(1.9, base_getter.getBase().getInventory(), 0.1);
    assertEquals(59, base_getter.getBase().getSoluteId());

    // Set new values and update, update getter
    base_setter.setName("basename6");
    base_setter.setInventory(1.8);
    base_setter.setSolute(2);
    base_setter.setSoluteType("base");
    base_setter.update();
    base_getter = new BaseRDGRDS(37);

    // Ensure update method changed our values
    assertEquals("basename6", base_getter.getBase().getName());
    assertEquals(1.8, base_getter.getBase().getInventory(), 0.1);
    assertEquals(2, base_getter.getBase().getSoluteId());

    base_getter.delete(); // Delete because we don't need
  }

  /**
   * Test that the delete function in BaseRDGRDS works
   */
  @Test
  static void testDelete() {
    // Create a new base
    BaseRDG base = new BaseRDGRDS(9, "basename9", 1.9, "base");

    // Ensure the base has been added properly
    assertEquals("basename9", base.getBase().getName());
    assertEquals(2, base.getBase().getSoluteId());
    assertEquals(1.9, base.getBase().getInventory(), 0.1);

    // Delete
    base.delete();

    // When we try to fetch the base it should fail
    try {
      base = new BaseRDGRDS(1);
    } catch (DatabaseException | SQLException e) {
      assertTrue(true);
    }
  }

  /**
   * Test that the getSet function in BaseRDGRDS works
   */
  @Test
  static void testGetSet() {
    BaseRDG getter = new BaseRDGRDS(); 
    List<BaseRDGRDS> baseGet = getter.findSet(55);

    // Test
    assertEquals("basename5", baseGet.get(0).getBase().getName());
    assertEquals("basename6", baseGet.get(1).getBase().getName());
  }
  
  /**
   * Test the getAll function in BaseTDGRDS
   */
  @Test
  static void testGetAll() {
    List<BaseDTO> getAll;
    try {
      getAll = new BaseTDGRDS().getAllBases().executeQuery();
      
      // Assert that we have 6 bases, and that they are the right ids. 
      assertEquals(9, getAll.size());
      assertEquals(9, getAll.get(0).getBaseId());
      assertEquals(10, getAll.get(1).getBaseId());
      assertEquals(11, getAll.get(2).getBaseId());
      assertEquals(12, getAll.get(3).getBaseId());
      assertEquals(13, getAll.get(4).getBaseId());
      assertEquals(14, getAll.get(5).getBaseId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    } 
  }
  
  @Test
  public static void testFilterByName() {
    try {
      List<BaseDTO> get = new BaseTDGRDS().getAllBases().filterByName("funky").executeQuery();
      
      assertEquals(2, get.size());
      assertEquals(15, get.get(0).getBaseId());
      assertEquals(16, get.get(1).getBaseId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  public static void testFilterByInventory() {
    try {
      List<BaseDTO> get = new BaseTDGRDS().getAllBases().filterByInventory(41.2).executeQuery();
      
      assertEquals(15, get.get(0).getBaseId());
      
      get = new BaseTDGRDS().getAllBases().filterByInventoryRange(42, 40).executeQuery();
      
      assertEquals(15, get.get(0).getBaseId());
      
      get = new BaseTDGRDS().getAllBases().filterByInventoryRange(43, 40).executeQuery();
      
      assertEquals(15, get.get(0).getBaseId());
      assertEquals(16, get.get(1).getBaseId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  public static void testFilterBySolute() {
    try {
      List<BaseDTO> get = new BaseTDGRDS().getAllBases().filterBySolute(15).executeQuery();
      
      assertEquals(15, get.get(0).getSoluteId());
      
    } catch(DatabaseException e) {
      e.printStackTrace();
    }
  }

  /**
   * Run every test function in this class
   */
  static void testAll() {
    try {
      testGetName();
      testGetInventory();
      testGetSolute();
      testDelete();
      testUpdate();
      testGetSet();
      testGetAll();
      testFilterByName();
      testFilterByInventory();
      testFilterBySolute();
    } catch (SQLException | DatabaseException e) {
      e.printStackTrace();
    }
  }

}
