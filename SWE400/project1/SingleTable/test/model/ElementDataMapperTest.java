package model;

import static org.junit.jupiter.api.Assertions.*;
import java.util.List;
import org.junit.jupiter.api.Test;
import datasource.DatabaseTest;

/**
 * Test cases for ElementDataMapper().
 * @author andrewjanuszko
 *
 */
public class ElementDataMapperTest extends DatabaseTest {

  /**
   * Test creating duplicate Elements.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testCreateDuplicate() throws DomainModelException {
    try {
      Element oxygen = new ElementDataMapper().create("Oxygen", 70, 8, 15.999);
      assertEquals("Oxygen", oxygen.getName());
      fail();
    } catch (DomainModelException e) {
      assertTrue(true);
    }
  }
  
  /**
   * Test reading Elements from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testRead() throws DomainModelException {
    Element nitrogen = new ElementDataMapper().read(4);
    assertEquals("Nitrogen", nitrogen.getName());
  }
  
  /**
   * Test updating Elements from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testUpdate() throws DomainModelException {
    try {
      Element iron = new ElementDataMapper().read(8);
      assertEquals("Iron", iron.getName());
      iron.setName("Wacky Iron");
      new ElementDataMapper().update(iron);
      iron = new ElementDataMapper().read(8);
      assertEquals("Wacky Iron", iron.getName());
    } catch (DomainModelException e) {
      System.out.println(e);
      fail();
    }
  }
  
  /**
   * Test deleting Elements from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testDelete() throws DomainModelException {
    try {
      Element hydrogen = new ElementDataMapper().read(1);
      assertEquals("Hydrogen", hydrogen.getName());
      
      new ElementDataMapper().delete(hydrogen);
      
      Element readHydrogen = new ElementDataMapper().read(hydrogen.getID());
      assertEquals("Hydrogen", readHydrogen.getName());
      fail();
    } catch (DomainModelException e) {
      assertTrue(true);
    }
  }
  
  /**
   * Test getting all Elements.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testGetAll() throws DomainModelException {
    List<Element> elements = new ElementDataMapper().getAll();
    assertEquals(12, elements.size());
  }
  
  /**
   * Test getting Elements with names like.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testNameLike() throws DomainModelException {
    List<Element> elements = new ElementDataMapper().filterByNameLike("Carbon");
    assertEquals(1, elements.size());
  }
  
  /**
   * Test getting Elements with specific and ranged inventories.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testInventorySpecificAndRange() throws DomainModelException {
    List<Element> elements = new ElementDataMapper().filterByInventory(3.14);
    assertEquals(1, elements.size());
    
    elements = new ElementDataMapper().filterByInventoryBetween(6, 13);
    assertEquals(3, elements.size());
  }
  
  /**
   * Test getting Elements with specific and ranged atomic numbers.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testAtomicNumberSpecificAndRange() throws DomainModelException {
    List<Element> elements = new ElementDataMapper().filterByAtomicNumber(6);
    assertEquals(1, elements.size());
    
    elements = new ElementDataMapper().filterByAtomicNumberBetween(6, 8);
    assertEquals(3, elements.size());
  }
  
  /**
   * Test getting Elements with specific and ranged atomic masses.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testAtomicMassSpecificAndRange() throws DomainModelException {
    List<Element> elements = new ElementDataMapper().filterByAtomicMass(14.007);
    assertEquals(1, elements.size());
    
    elements = new ElementDataMapper().filterByAtomicMassBetween(0, 20);
    assertEquals(5, elements.size());
  }
  
  /**
   * Test getting all Elements in a Compound.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testElementsInCompound() throws DomainModelException {
    List<Element> inWater = new ElementDataMapper().filterByPartOfCompound(13);
    assertEquals(2, inWater.size());
  }
  
  
}