package datasource;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
import dataDTO.ElementCompoundDTO;

public abstract class CompoundElementTableDataGatewayTest extends DatabaseTest {

  protected ElementCompoundTableDataGateway gateway;

  /**
   * Gets a singleton.
   */
  protected abstract ElementCompoundTableDataGateway getSingletonInstance();

  @Test
  public void isSingleton() throws DatabaseException {
    ElementCompoundTableDataGatewayInterface gateway1 = getSingletonInstance();
    ElementCompoundTableDataGatewayInterface gateway2 = getSingletonInstance();
    assertNotNull(gateway1);
    assertNotNull(gateway2);
    assertEquals(gateway1, gateway2);
  }
  
  @Test
  public void readWithCompoundID() throws DatabaseException {
    ElementCompoundDTO sodiumChloride = ElementCompoundTableDataGateway.getSingletonInstance().readElementsFromCompound(15);
    assertEquals(2, sodiumChloride.getRelations().size());
    assertEquals("Chlorine", sodiumChloride.getRelations().get(0).getName());
    assertEquals("Sodium", sodiumChloride.getRelations().get(1).getName());
  }
  
  @Test
  public void createUpdate() throws DatabaseException {
	  ElementCompoundTableDataGateway.getSingletonInstance().update(15, 7, 15, 1);
	  ElementCompoundDTO sodiumChloride = ElementCompoundTableDataGateway.getSingletonInstance().readElementsFromCompound(15);
	  assertEquals(2, sodiumChloride.getRelations().size());
	  assertEquals("Hydrogen", sodiumChloride.getRelations().get(0).getName());
	  assertEquals("Chlorine", sodiumChloride.getRelations().get(1).getName());
  }
  
  @Test
  public void createDelete() throws DatabaseException {
	  ElementCompoundTableDataGateway.getSingletonInstance().delete(15, 7);
	  ElementCompoundDTO sodiumChloride = ElementCompoundTableDataGateway.getSingletonInstance().readElementsFromCompound(15);
	  assertEquals(1, sodiumChloride.getRelations().size());
	  assertEquals("Chlorine", sodiumChloride.getRelations().get(0).getName());
  }
}
