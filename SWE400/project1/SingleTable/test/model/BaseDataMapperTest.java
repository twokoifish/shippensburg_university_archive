package model;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import org.junit.jupiter.api.Test;

import datasource.DatabaseTest;

class BaseDataMapperTest extends DatabaseTest {

  /**
   * Test reading Bases from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testRead() throws DomainModelException {
    Base potassiumHydroxide = new BaseDataMapper().read(25);
    assertEquals("Potassium Hydroxide", potassiumHydroxide.getName());
  }
  
  /**
   * Test updating Bases from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testUpdate() throws DomainModelException {
    try {
      Base potassiumHydroxide = new BaseDataMapper().read(25);
      assertEquals("Potassium Hydroxide", potassiumHydroxide.getName());
      potassiumHydroxide.setName("Potassium OwO");
      new BaseDataMapper().update(potassiumHydroxide);
      potassiumHydroxide = new BaseDataMapper().read(25);
      assertEquals("Potassium OwO", potassiumHydroxide.getName());
    } catch (DomainModelException e) {
      fail();
    }
  }
  
  /**
   * Test deleting Bases from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testDelete() throws DomainModelException {
    try {
      Base potassiumHydroxide = new BaseDataMapper().read(25);
      assertEquals("Potassium Hydroxide", potassiumHydroxide.getName());
      
      new BaseDataMapper().delete(potassiumHydroxide);
      
      potassiumHydroxide = new BaseDataMapper().read(potassiumHydroxide.getID());
      assertEquals("Potassium Hydroxide", potassiumHydroxide.getName());
      fail();
    } catch (DomainModelException e) {
      assertTrue(true);
    }
  }
  
  /**
   * Test getting all Bases.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testGetAll() throws DomainModelException {
    List<Base> bases = new BaseDataMapper().getAll();
    assertEquals(6, bases.size());
  }
  
  /**
   * Test getting Bases with names like.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testNameLike() throws DomainModelException {
    List<Base> bases = new BaseDataMapper().filterByNameLike("Hydroxide");
    assertEquals(6, bases.size());
  }
  
  /**
   * Test getting Bases with specific and ranged inventories.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testInventorySpecificAndRange() throws DomainModelException {
    List<Base> bases = new BaseDataMapper().filterByInventory(18);
    assertEquals(1, bases.size());
    
    bases = new BaseDataMapper().filterByInventoryBetween(10, 30);
    assertEquals(5, bases.size());
  }
  
  /**
   * Test getting all Bases.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testGetSolute() throws DomainModelException {
    List<Base> bases = new BaseDataMapper().filterBySolute(24);
    assertEquals(1, bases.size());
  }
}
