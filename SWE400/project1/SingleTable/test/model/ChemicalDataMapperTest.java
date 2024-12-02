package model;

import static org.junit.jupiter.api.Assertions.*;
import java.util.List;
import org.junit.jupiter.api.Test;

import datasource.DatabaseTest;

/**
 * Test cases for ChemicalDataMapper().
 * @author andrewjanuszko
 *
 */
class ChemicalDataMapperTest extends DatabaseTest {

  /**
   * Get all Chemicals.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  void testGetAll() throws DomainModelException {
      List<Chemical> chemicals = new ChemicalDataMapper().getAll();
      assertEquals(30, chemicals.size());
  }
  
  /**
   * Get all Chemicals with name like.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  void testNameLike() throws DomainModelException {
    List<Chemical> chemicals = new ChemicalDataMapper().filterByNameLike("Sodium");
    assertEquals(3, chemicals.size());
  }
 
  /**
   * Get all Chemicals with specific and ranged inventory.
   * @throws DomainModelException when things go wrong.
   */
  @Test
  void testInventory() throws DomainModelException {
    List<Chemical> chemicals = new ChemicalDataMapper().filterByInventory(10);
    assertEquals(6, chemicals.size());
    chemicals = new ChemicalDataMapper().filterByInventoryBetween(10, 50);
    assertEquals(26, chemicals.size());
  }

}
