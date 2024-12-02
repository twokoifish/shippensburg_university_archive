package model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.List;
import org.junit.jupiter.api.Test;

import datasource.DatabaseTest;

/**
 * Test cases for MetalDataMapper().
 * @author andrewjanuszko
 *
 */
public class MetalDataMapperTest extends DatabaseTest {

  /**
   * Test creating a duplicate Metal
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testCreateDuplicate() throws DomainModelException {
    Metal iron = new MetalDataMapper().read(8);
    assertEquals("Iron", iron.getName());
    try {
      iron = new MetalDataMapper().create("Iron", 50.0, 26, 55.8, 4);
      assertEquals("Iron", iron.getName());
      fail();
    } catch (DomainModelException e) {
      assertTrue(true);
    }
  }

  /**
   * Test reading Metals from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testRead() throws DomainModelException {
    Metal zinc = new MetalDataMapper().read(11);
    assertEquals("Zinc", zinc.getName());
  }

  /**
   * Test updating a Metal in the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testUpdate() throws DomainModelException {
    Metal zinc = new MetalDataMapper().read(11);
    assertEquals("Zinc", zinc.getName());
    
    zinc.setName("Whacky Zinc");
    new MetalDataMapper().update(zinc);
    
    zinc = new MetalDataMapper().read(11);
    assertEquals("Whacky Zinc", zinc.getName());
  }

  /**
   * Test deleting a Metal from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testDelete() throws DomainModelException {
    Metal Iron = new MetalDataMapper().read(8);
    assertEquals("Iron", Iron.getName());
    try {
      new MetalDataMapper().delete(Iron);
      Iron = new MetalDataMapper().read(8);
      fail();
    } catch (DomainModelException e) {
      assertTrue(true);
    }
  }

  /**
   * Test getting all Metals from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testGetAll() throws DomainModelException {
    List<Metal> metals = new MetalDataMapper().getAll();
    assertEquals(6, metals.size());
  }

  /**
   * Test getting all Metals with a name like from the database.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testNameLike() throws DomainModelException {
    List<Metal> metals = new MetalDataMapper().filterByNameLike("o");
    assertEquals(3, metals.size());
  }

  /**
   * Test getting Metals with specific and ranged inventory.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testInventorySpecificAndRange() throws DomainModelException {
    List<Metal> metals = new MetalDataMapper().filterByInventory(28.26);
    assertEquals(1, metals.size());
    metals = new MetalDataMapper().filterByInventoryBetween(20, 40);
    assertEquals(6, metals.size());
  }

  /**
   * Test getting Metals with specific and ranged atomic number.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testAtomicNumberSpecificAndRange() throws DomainModelException {
    List<Metal> metals = new MetalDataMapper().filterByAtomicNumber(47);
    assertEquals(1, metals.size());
    metals = new MetalDataMapper().filterByAtomicNumberBetween(20, 30);
    assertEquals(3, metals.size());
  }

  /**
   * Test getting Metals with specific and ranged atomic masses.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testAtomicMassSpecificAndRange() throws DomainModelException {
    List<Metal> metals = new MetalDataMapper().filterByAtomicMass(107.87);
    assertEquals(1, metals.size());
    metals = new MetalDataMapper().filterByAtomicMassBetween(50, 70);
    assertEquals(3, metals.size());
  }

  /**
   * Test getting Metals with specific and ranged acid amount.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testAcidAmountSpecificAndRange() throws DomainModelException {
    List<Metal> metals = new MetalDataMapper().filterByAcidAmount(2);
    assertEquals(1, metals.size());
    metals = new MetalDataMapper().filterByAcidAmountBetween(10, 15);
    assertEquals(3, metals.size());
  }

  /**
   * Test getting Metals with specific dissolved by.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testDissolvedBy() throws DomainModelException {
    List<Metal> metals = new MetalDataMapper().filterByDissolvedBy(20);
    assertEquals(1, metals.size());
  }

  /**
   * Test getting Metals that are in a Compound.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  public void testPartOfCompund() throws DomainModelException {
    List<Metal> metals = new MetalDataMapper().filterByPartOfCompound(17);
    assertEquals(1, metals.size());
  }


}
