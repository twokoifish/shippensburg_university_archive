package datasource;

import static org.junit.jupiter.api.Assertions.*;
import java.util.List;
import org.junit.jupiter.api.Test;
import dataDTO.ChemicalDTO;

public abstract class ChemicalTableDataGatewayInterfaceTest extends DatabaseTest {

  protected ChemicalTableDataGatewayInterface gateway = getGateway();

  public abstract ChemicalTableDataGatewayInterface getGateway();

  /**
   * Test pulling everything out of the table.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchAll() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getAll().executeQuery();
    assertEquals(30, chemicals.size());
    assertEquals("Hydrogen", chemicals.get(0).getName());
    assertEquals("Silver Chloride", chemicals.get(16).getName());
    assertEquals("Strontium Hydroxide", chemicals.get(29).getName());
  }

  /**
   * Test pulling elements out of the table.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchElements() throws DatabaseException {
    List<ChemicalDTO> elements = gateway.getElements().executeQuery();
    assertEquals(12, elements.size());
    assertEquals("Hydrogen", elements.get(0).getName());
    assertEquals("Chlorine", elements.get(5).getName());
    assertEquals("Silver", elements.get(11).getName());
  }

  /**
   * Test pulling metals out of the table.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchMetals() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getMetals().executeQuery();
    assertEquals(6, chemicals.size());
    assertEquals("Sodium", chemicals.get(0).getName());
    assertEquals("Mercury", chemicals.get(3).getName());
    assertEquals("Silver", chemicals.get(5).getName());
  }

  /**
   * Test pulling compounds out of the table.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchCompounds() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getCompounds().executeQuery();
    assertEquals(6, chemicals.size());
    assertEquals("Hydrogen Dioxide", chemicals.get(0).getName());
    assertEquals("Sucrose", chemicals.get(3).getName());
    assertEquals("Nitrate", chemicals.get(5).getName());
  }

  /**
   * Test pulling bases out of the table.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchBases() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getBases().executeQuery();
    assertEquals(6, chemicals.size());
    assertEquals("Potassium Hydroxide", chemicals.get(0).getName());
    assertEquals("Lithium Hydroxide", chemicals.get(3).getName());
    assertEquals("Strontium Hydroxide", chemicals.get(5).getName());
  }

  /**
   * Test pulling acids out of the table.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchAcids() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getAcids().executeQuery();
    assertEquals(6, chemicals.size());
    assertEquals("Hydrochloric Acid", chemicals.get(0).getName());
    assertEquals("Carbonic Acid", chemicals.get(3).getName());
    assertEquals("Citric Acid", chemicals.get(5).getName());
  }

  /**
   * Test finding things by name.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchByName() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getAll().filterByNameLike("Hydro").executeQuery();
    assertEquals(9, chemicals.size());
    assertEquals("Hydrogen", chemicals.get(0).getName());
    assertEquals("Sodium Hydroxide", chemicals.get(4).getName());
    assertEquals("Strontium Hydroxide", chemicals.get(8).getName());
  }

  /**
   * Test finding things by habitat.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchByInventory() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getAll().filterByInventory(10.0).executeQuery();
    assertEquals(6, chemicals.size());
    assertEquals("Hydrogen Dioxide", chemicals.get(0).getName());
    assertEquals("Citric Acid", chemicals.get(3).getName());
    assertEquals("Barium Hydroxide", chemicals.get(5).getName());
  }

  /**
   * Test finding things by atomic number.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchByAtomicNumber() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getElements().filterByAtomicNumber(6).executeQuery();
    assertEquals(1, chemicals.size());
    assertEquals("Carbon", chemicals.get(0).getName());
  }

  /**
   * Test finding things by atomic mass.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchByAtomicMassValue() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getElements().filterByAtomicMass(1.008).executeQuery();
    assertEquals(1, chemicals.size());
    assertEquals("Hydrogen", chemicals.get(0).getName());
  }

  /**
   * Test finding things by dissolved by.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchByDissolvedBy() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getMetals().filterByDissolvedBy(20).executeQuery();
    assertEquals(1, chemicals.size());
    assertEquals("Sodium", chemicals.get(0).getName());
  }
  
  /**
   * Test getting things by their moles.
   * @throws DatabaseException
   */
  @Test
  void testFetchByAcidAmount() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getMetals().filterByAcidAmountBetween(10, 15).executeQuery();
    assertEquals(3, chemicals.size());
    assertEquals("Iron", chemicals.get(0).getName());
    assertEquals("Copper", chemicals.get(1).getName());
    assertEquals("Silver", chemicals.get(2).getName());
  }

  /**
   * Test finding things by solute.
   * 
   * @throws DatabaseException when things go wrong.
   */
  @Test
  void testFetchBySolute() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getAll().filterBySolute(21).executeQuery();
    assertEquals(1, chemicals.size());
    assertEquals("Calcium Hydroxide", chemicals.get(0).getName());
  }
  
  /**
   * Test getting things with low inventory.
   * @throws DatabaseException
   */
  @Test
  void testGetLowAll() throws DatabaseException {
    List<ChemicalDTO> chemicals = gateway.getAllWithLowInventory();
    assertEquals(15, chemicals.size());
    assertEquals("Hydrogen", chemicals.get(0).getName());
    assertEquals("Calcium Hydroxide", chemicals.get(7).getName());
    assertEquals("Citric Acid", chemicals.get(14).getName());
  }

}
