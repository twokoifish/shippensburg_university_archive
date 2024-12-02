package model;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import org.junit.jupiter.api.Test;

import datasource.DatabaseTest;

class AcidDataMapperTest extends DatabaseTest {

  /**
   * Test reading Acids from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testRead() throws DomainModelException {
    Acid nitricAcid = new AcidDataMapper().read(20);
    assertEquals("Nitric Acid", nitricAcid.getName());
  }
  
  /**
   * Test updating Acids from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testUpdate() throws DomainModelException {
    try {
      Acid nitricAcid = new AcidDataMapper().read(20);
      assertEquals("Nitric Acid", nitricAcid.getName());
      nitricAcid.setName("Nitric OwO");
      new AcidDataMapper().update(nitricAcid);
      nitricAcid = new AcidDataMapper().read(20);
      assertEquals("Nitric OwO", nitricAcid.getName());
    } catch (DomainModelException e) {
      fail();
    }
  }
  
  /**
   * Test deleting Acids from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testDelete() throws DomainModelException {
    try {
      Acid nitricAcid = new AcidDataMapper().read(20);
      assertEquals("Nitric Acid", nitricAcid.getName());
      
      new AcidDataMapper().delete(nitricAcid);
      
      nitricAcid = new AcidDataMapper().read(nitricAcid.getID());
      assertEquals("Nitric Acid", nitricAcid.getName());
      fail();
    } catch (DomainModelException e) {
      assertTrue(true);
    }
  }
  
  /**
   * Test getting all Acids.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testGetAll() throws DomainModelException {
    List<Acid> acids = new AcidDataMapper().getAll();
    assertEquals(6, acids.size());
  }
  
  /**
   * Test getting Acids with names like.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testNameLike() throws DomainModelException {
    List<Acid> acids = new AcidDataMapper().filterByNameLike("Acid");
    assertEquals(6, acids.size());
  }
  
  /**
   * Test getting Acids with specific and ranged inventories.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testInventorySpecificAndRange() throws DomainModelException {
    List<Acid> acids = new AcidDataMapper().filterByInventory(10);
    assertEquals(3, acids.size());
    
    acids = new AcidDataMapper().filterByInventoryBetween(10, 30);
    assertEquals(5, acids.size());
  }
  
  /**
   * Test getting all Acids.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testGetSolute() throws DomainModelException {
    List<Acid> acids = new AcidDataMapper().filterBySolute(27);
    assertEquals(1, acids.size());
  }

}
