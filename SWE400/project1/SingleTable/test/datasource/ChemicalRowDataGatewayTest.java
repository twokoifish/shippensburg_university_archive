package datasource;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import org.junit.jupiter.api.Test;

import dataENUM.ChemicalEnum;

public class ChemicalRowDataGatewayTest extends DatabaseTest {

  /**
   * Test the creation of a table and inserting into it.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  public void testInsert() throws DatabaseException {
    ChemicalRowDataGateway carbon = new ChemicalRowDataGateway(3);
    assertEquals("Carbon", carbon.getName());
    assertEquals(6, carbon.getAtomicNumber());
    assertEquals(12.011, carbon.getAtomicMass());
  }

  /**
   * Test the insertion of duplicate entries.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  public void testDuplicateEntry() throws DatabaseException {
    try {
      @SuppressWarnings("unused")
      ChemicalRowDataGateway hydrogen = new ChemicalRowDataGateway(ChemicalEnum.ELEMENT.getIntValue(), "Hydrogen", 3.14, 1, 1.008, 0, 0, 0);
      fail();
    } catch (DatabaseException e) {
      assertTrue(true);
    }
  }

  /**
   * Test dropping a chemical from the table.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  public void testDeleteChemical() throws DatabaseException {
    ChemicalRowDataGateway helium = new ChemicalRowDataGateway(2);
    assertEquals("Helium", helium.getName());
    helium.delete();

    try {
      @SuppressWarnings("unused")
      ChemicalRowDataGateway helium2 = new ChemicalRowDataGateway(2);
      fail();
    } catch (DatabaseException e) {
      assertTrue(true);
    }
  }

  /**
   * Test updating an entry in the database.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  public void testUpdate() throws DatabaseException {
    ChemicalRowDataGateway carbon = new ChemicalRowDataGateway(3);
    assertEquals("Carbon", carbon.getName());
    assertEquals(6, carbon.getAtomicNumber());
    assertEquals(12.011, carbon.getAtomicMass());

    carbon.setName("Carbon-13");
    carbon.setInventory(12.345);
    carbon.setAtomicMass(13.003);
    carbon.update();

    ChemicalRowDataGateway carbon13 = new ChemicalRowDataGateway(3);

    assertEquals("Carbon-13", carbon13.getName());
    assertEquals(6, carbon.getAtomicNumber());
    assertEquals(13.003, carbon13.getAtomicMass(), 0.001);
  }

}