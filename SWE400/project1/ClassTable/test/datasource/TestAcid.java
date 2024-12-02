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
class TestAcid extends DatabaseTest {

  /**
   * Test that the getName function in AcidRDGRDS works.
   * 
   * @throws SQLException
   * @throws DatabaseException
   */
  @Test
  static void testGetName() throws SQLException, DatabaseException {
    // Fetch acids
    AcidRDG acid1 = new AcidRDGRDS(3), acid2 = new AcidRDGRDS(4), acid3 = new AcidRDGRDS(5), acid4 = new AcidRDGRDS(6);

    // Test
    assertEquals("acidname1", acid1.getAcid().getName());
    assertEquals("acidname2", acid2.getAcid().getName());
    assertEquals("acidname3", acid3.getAcid().getName());
    assertEquals("acidname4", acid4.getAcid().getName());
  }

  /**
   * Test that the getInventory function in AcidRDGRDS works.
   * 
   * @throws SQLException
   * @throws DatabaseException
   */
  @Test
  static void testGetInventory() throws SQLException, DatabaseException {
    // Fetch acids
    AcidRDG acid1 = new AcidRDGRDS(3), acid2 = new AcidRDGRDS(4), acid3 = new AcidRDGRDS(5), acid4 = new AcidRDGRDS(6);

    // Test
    assertEquals(1.1, acid1.getAcid().getInventory(), 0.1);
    assertEquals(1.2, acid2.getAcid().getInventory(), 0.1);
    assertEquals(1.3, acid3.getAcid().getInventory(), 0.1);
    assertEquals(1.4, acid4.getAcid().getInventory(), 0.1);
  }

  /**
   * Test that the getSolute function in AcidRDGRDS works.
   * 
   * @throws SQLException
   * @throws DatabaseException
   */
  @Test
  static void testGetSolute() throws SQLException, DatabaseException {
    // Fetch acids
    AcidRDG acid1 = new AcidRDGRDS(3), acid2 = new AcidRDGRDS(4), acid3 = new AcidRDGRDS(5), acid4 = new AcidRDGRDS(6);

    // Test
    assertEquals(51, acid1.getAcid().getSoluteId());
    assertEquals(52, acid2.getAcid().getSoluteId());
    assertEquals(53, acid3.getAcid().getSoluteId());
    assertEquals(54, acid4.getAcid().getSoluteId());
  }

  /**
   * Test that the delete function in AcidRDGRDS works.
   */
  @Test
  static void testDelete() {
    // Create acid
    AcidRDG acid = new AcidRDGRDS(1, "acidname100", 1.9, "acid");
    acid = new AcidRDGRDS();
    // Ensure it has been added
    assertEquals("acidname1", acid.getAcid().getName());
    assertEquals(1.9, acid.getAcid().getInventory(), 0.1);
    assertEquals(59, acid.getAcid().getSoluteId());

    // Delete
    acid.delete();

    // Retrieving this acid should now result in a failure
    try {
      acid = new AcidRDGRDS(29);
    } catch (DatabaseException | SQLException e) {
      assertTrue(true);
    }
  }

  /**
   * Test that the update function in AcidRDGRDS works.
   * 
   * @throws SQLException
   * @throws DatabaseException
   */
  @Test
  static void testUpdate() throws SQLException, DatabaseException {
    // Create acid and getter for that acid
    AcidRDG acid_setter = new AcidRDGRDS(59, "acidname9", 1.9, "Acid"), 
        acid_getter = new AcidRDGRDS(33);

    // Ensure that acid has been added and fetches the right information
    assertEquals("acidname9", acid_getter.getAcid().getName());
    assertEquals(1.9, acid_getter.getAcid().getInventory(), 0.1);
    assertEquals(59, acid_getter.getAcid().getSoluteId());

    // Change the information, then update and refresh the getter
    acid_setter.setName("acidname6");
    acid_setter.setInventory(1.8);
    acid_setter.setSolute(56);
    acid_setter.update();
    acid_getter = new AcidRDGRDS(33);

    // Test that the new information has been updated
    assertEquals("acidname6", acid_getter.getAcid().getName());
    assertEquals(1.8, acid_getter.getAcid().getInventory(), 0.1);
    assertEquals(56, acid_getter.getAcid().getSoluteId());

    // Delete because we don't need it.
    acid_getter.delete();
  }

  /**
   * Test that the getSet function in AcidRDGRDS works.
   */
  @Test
  static void testGetSet() {
    AcidRDG getter = new AcidRDGRDS(); // Empty AcidRDG
    List<AcidRDGRDS> acidGet = getter.findSet(55); // Get set

    // Test
    assertEquals("acidname5", acidGet.get(0).getAcid().getName());
    assertEquals("acidname6", acidGet.get(1).getAcid().getName());
  }
  
  /**
   * Test the getAll function inside AcidTDGRDS
   */
  @Test
  static void testGetAll() {
    try {
      AcidTDG acid = new AcidTDGRDS().getAllAcids(); 
      
      List<AcidDTO> getAll = acid.executeQuery();
      
      // Assert that we have 6 acids, and that they are the right ids.
      assertEquals(9, getAll.size());
      assertEquals(3, getAll.get(0).getAcidId());
      assertEquals(4, getAll.get(1).getAcidId());
      assertEquals(5, getAll.get(2).getAcidId());
      assertEquals(6, getAll.get(3).getAcidId());
      assertEquals(7, getAll.get(4).getAcidId());
      assertEquals(8, getAll.get(5).getAcidId());
      
    } catch (DatabaseException e) {
      System.out.println("Failed testGetAll()");
      e.printStackTrace();
    } 
  }
  
  @Test
  public static void testFilterByName() {
    try {
      List<AcidDTO> get = new AcidTDGRDS().getAllAcids().filterByName("funky").executeQuery();
      
      assertEquals(2, get.size());
      assertEquals(9, get.get(0).getAcidId());
      assertEquals(10, get.get(1).getAcidId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  
  @Test
  public static void testFilterByInventory() {
    try {
      List<AcidDTO> get = new AcidTDGRDS().getAllAcids().filterByInventory(41.2).executeQuery();
      
      assertEquals(9, get.get(0).getAcidId());
      
      get = new AcidTDGRDS().getAllAcids().filterByInventoryRange(42, 40).executeQuery();
      
      assertEquals(9, get.get(0).getAcidId());
      
      get = new AcidTDGRDS().getAllAcids().filterByInventoryRange(43, 40).executeQuery();
      
      assertEquals(9, get.get(0).getAcidId());
      assertEquals(10, get.get(1).getAcidId());
      
    } catch (DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @Test
  public static void testFilterBySolute() {
    try {
      List<AcidDTO> get = new AcidTDGRDS().getAllAcids().filterBySolute(2).executeQuery();
      
      assertEquals(4, get.size());
      
    } catch(DatabaseException e) {
      e.printStackTrace();
    }
  }

  /**
   * Test all functions in TestAcid
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
