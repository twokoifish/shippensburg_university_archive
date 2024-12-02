package model;

import static org.junit.jupiter.api.Assertions.*;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;
import datasource.DatabaseTest;

/**
 * Test cases for CompoundDataMapper().
 * @author andrewjanuszko
 *
 */
public class CompoundDataMapperTest extends DatabaseTest {
 
  /**
   * Test reading a Compound.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testRead() throws DomainModelException {
    Compound hydrogenDioxide = new CompoundDataMapper().read(13);
    assertEquals("Hydrogen Dioxide", hydrogenDioxide.getName());
    assertEquals(2, hydrogenDioxide.getMadeOf().size());
  }
  
  /**
   * Test updating a Compound.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testUpdate() throws DomainModelException {
    Compound hydrogenDioxide = new CompoundDataMapper().read(13);
    assertEquals("Hydrogen Dioxide", hydrogenDioxide.getName());
    assertEquals(2, hydrogenDioxide.getMadeOf().size());
    hydrogenDioxide.setMadeOf(Arrays.asList(new ElementDataMapper().read(4), new ElementDataMapper().read(1), new ElementDataMapper().read(2)));
    new CompoundDataMapper().update(hydrogenDioxide);
    hydrogenDioxide = new CompoundDataMapper().read(13);
    assertEquals(3, hydrogenDioxide.getMadeOf().size());   
  }
  
  /**
   * Test deleting a Compound.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testDelete() throws DomainModelException {
    Compound sucrose = new CompoundDataMapper().read(16);
    assertEquals("Sucrose", sucrose.getName());
    assertEquals(3, sucrose.getMadeOf().size());
    new CompoundDataMapper().delete(sucrose);
    try {
      sucrose = new CompoundDataMapper().read(16);
      fail();
    } catch (DomainModelException e) {
      assertTrue(true);
    }
  }
  
  /**
   * Test getting all Compounds.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testGetAll() throws DomainModelException {
    List<Compound> compounds = new CompoundDataMapper().getAll();
    assertEquals(6, compounds.size());
  }
  
  /**
   * Test getting all Compounds with a name like.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testNameLike() throws DomainModelException {
    List<Compound> compounds = new CompoundDataMapper().filterByNameLike("o");
    assertEquals(5, compounds.size());
  }
  
  /**
   * Test getting Compound with specific and ranged inventory.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testInventorySpecificAndRange() throws DomainModelException {
    List<Compound> compounds = new CompoundDataMapper().filterByInventory(10);
    assertEquals(1, compounds.size());
    compounds = new CompoundDataMapper().filterByInventoryBetween(20, 30);
    assertEquals(3, compounds.size());
  }
  
  /**
   * Test getting the Compounds that contain the same Element.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testMadeOf() throws DomainModelException {
    List<Compound> compounds = new CompoundDataMapper().filterByMadeOf(5);
    assertEquals(4, compounds.size());
  }

}