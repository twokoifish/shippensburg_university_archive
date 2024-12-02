package datasource;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class MetalRowDataGatewayRDSTest extends DatabaseTest {

  /**
   * Setup.
   * 
   * @throws DatabaseException
   */
  @BeforeEach
  void setup() throws DatabaseException {
    ConcreteTableInitializer.clearMaps();
    ConcreteTableInitializer.dropTables();
    ConcreteTableInitializer.createTables();
  }

  @Test
  void testGetters() throws DatabaseException {
    AcidRowDataGateway acid1 = new AcidRowDataGatewayRDS("acid", 1.0, 1, "Null");
    MetalRowDataGateway Metal = new MetalRowDataGatewayRDS("Metal", 1.0, 5, 10.0, 10.0, acid1.getAcidID());

    assertEquals(1, Metal.getMetalID());
    assertEquals("Metal", Metal.getName());
    assertEquals(1.0, Metal.getInventory());
    assertEquals(5, Metal.getAtomicNumber());
    assertEquals(10.0, Metal.getAtomicMass());
    assertEquals(10.0, Metal.getAcidAmount());
    assertEquals(acid1.getAcidID(), Metal.getDissolvedBy());
  }

  @Test
  void testSetters() throws DatabaseException {
    AcidRowDataGateway acid1 = new AcidRowDataGatewayRDS("acid", 1.0, 1, "Null");
    AcidRowDataGateway acid2 = new AcidRowDataGatewayRDS("otherAcid", 1.0, 1, "Null");
    MetalRowDataGateway Metal = new MetalRowDataGatewayRDS("Metal", 1.0, 5, 10.0, 10.0, acid1.getAcidID());

    Metal.setName("name");
    Metal.setInventory(2.0);
    Metal.setAtomicNumber(10);
    Metal.setAtomicMass(20.0);
    Metal.setAcidAmount(20.0);
    Metal.setDissolvedBy(acid2.getAcidID());

    assertEquals(1, Metal.getMetalID());
    assertEquals("name", Metal.getName());
    assertEquals(2.0, Metal.getInventory());
    assertEquals(10, Metal.getAtomicNumber());
    assertEquals(20.0, Metal.getAtomicMass());
    assertEquals(20.0, Metal.getAcidAmount());
    assertEquals(acid2.getAcidID(), Metal.getDissolvedBy());
  }

  @Test
  void testDelete() throws DatabaseException {
    MetalRowDataGateway metal1 = new MetalRowDataGatewayRDS("metal", 1.0, 5, 10.0, 10.0, 10000);

    assertTrue(metal1.delete());
  }

  @Test
  void testConstructors() throws DatabaseException {
    AcidRowDataGateway acid1 = new AcidRowDataGatewayRDS("acid", 1.0, 1, "Null");
    MetalRowDataGateway metal1 = new MetalRowDataGatewayRDS("metal", 1.0, 5, 10.0, 10.0, acid1.getAcidID());
    MetalRowDataGateway metal1FindByID = new MetalRowDataGatewayRDS(1);
    MetalRowDataGateway metal1FindByName = new MetalRowDataGatewayRDS("metal");

    assertEquals(metal1.getMetalID(), metal1FindByID.getMetalID());
    assertEquals(metal1.getName(), metal1FindByID.getName());
    assertEquals(metal1.getInventory(), metal1FindByID.getInventory());
    assertEquals(metal1.getAtomicNumber(), metal1FindByID.getAtomicNumber());
    assertEquals(metal1.getAtomicMass(), metal1FindByID.getAtomicMass());
    assertEquals(metal1.getAcidAmount(), metal1FindByID.getAcidAmount());
    assertEquals(metal1.getDissolvedBy(), metal1FindByID.getDissolvedBy());

    assertEquals(metal1.getMetalID(), metal1FindByName.getMetalID());
    assertEquals(metal1.getName(), metal1FindByName.getName());
    assertEquals(metal1.getInventory(), metal1FindByName.getInventory());
    assertEquals(metal1.getAtomicNumber(), metal1FindByName.getAtomicNumber());
    assertEquals(metal1.getAtomicMass(), metal1FindByName.getAtomicMass());
    assertEquals(metal1.getAcidAmount(), metal1FindByID.getAcidAmount());
    assertEquals(metal1.getDissolvedBy(), metal1FindByName.getDissolvedBy());
  }

  @Test
  void testPersist() throws DatabaseException {
    AcidRowDataGateway acid1 = new AcidRowDataGatewayRDS("acid", 1.0, 1, "Null");
    MetalRowDataGateway metal1 = new MetalRowDataGatewayRDS("metal", 1.0, 5, 10.0, 10.0, acid1.getAcidID());
    metal1.setName("newName");
    assertTrue(metal1.persist());
    metal1 = null;

    MetalRowDataGateway metal1Copy = new MetalRowDataGatewayRDS(1);
    assertEquals("newName", metal1Copy.getName());
  }
}
