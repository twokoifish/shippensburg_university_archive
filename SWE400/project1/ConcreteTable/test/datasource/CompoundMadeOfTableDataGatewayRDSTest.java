package datasource;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import datadto.CompoundMadeOfDTO;

class CompoundMadeOfTableDataGatewayRDSTest extends DatabaseTest{
  
  @BeforeEach
  void setup() throws DatabaseException {
    ConcreteTableInitializer.clearMaps();
    ConcreteTableInitializer.dropTables();
    ConcreteTableInitializer.createTables();
  }
  
	
  @Test
	void testAddCompound() throws DatabaseException {
		CompoundRowDataGateway c1 = new CompoundRowDataGatewayRDS("compound", 1.0);
		ElementRowDataGateway e1 = new ElementRowDataGatewayRDS("element", 1.0, 5, 10.0);
		MetalRowDataGateway m1 = new MetalRowDataGatewayRDS("metal", 1.0, 1, 1, 1, 1);
		
		CompoundMadeOfTableDataGatewayRDS.addCompoundMadeOf(c1.getCompoundID(), e1.getElementID(), e1.toString());
    CompoundMadeOfTableDataGatewayRDS.addCompoundMadeOf(c1.getCompoundID(), m1.getMetalID(), m1.toString());

		CompoundMadeOfDTO dtoCompoundElement = new CompoundMadeOfDTO(c1.getCompoundID(), e1.getElementID(), -1);
    CompoundMadeOfDTO dtoCompoundMetal = new CompoundMadeOfDTO(c1.getCompoundID(), -1, m1.getMetalID());

    assertEquals(dtoCompoundElement.getMetalID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByCompound(c1.getCompoundID()).get(0).getMetalID());
		assertEquals(dtoCompoundElement.getElementID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByCompound(c1.getCompoundID()).get(0).getElementID());
    assertEquals(dtoCompoundElement.getCompoundID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByCompound(c1.getCompoundID()).get(0).getCompoundID());

    assertEquals(dtoCompoundMetal.getMetalID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByCompound(c1.getCompoundID()).get(1).getMetalID());
    assertEquals(dtoCompoundMetal.getElementID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByCompound(c1.getCompoundID()).get(1).getElementID());
    assertEquals(dtoCompoundMetal.getCompoundID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByCompound(c1.getCompoundID()).get(1).getCompoundID());

	}
  
  @Test
  void testFindByElement() throws DatabaseException {
    CompoundRowDataGateway c1 = new CompoundRowDataGatewayRDS("compound", 1.0);
    ElementRowDataGateway e1 = new ElementRowDataGatewayRDS("element", 1.0, 5, 10.0);
    
    CompoundMadeOfTableDataGatewayRDS.addCompoundMadeOf(c1.getCompoundID(), e1.getElementID(), e1.toString());
    CompoundMadeOfDTO dtoCompoundElement = new CompoundMadeOfDTO(c1.getCompoundID(), e1.getElementID(), -1);
    
    assertEquals(dtoCompoundElement.getMetalID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByElement(c1.getCompoundID()).get(0).getMetalID());
    assertEquals(dtoCompoundElement.getElementID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByElement(c1.getCompoundID()).get(0).getElementID());
    assertEquals(dtoCompoundElement.getCompoundID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByElement(c1.getCompoundID()).get(0).getCompoundID());
   }
  
  @Test
  void testAddMetal() throws DatabaseException {
    CompoundRowDataGateway c1 = new CompoundRowDataGatewayRDS("compound", 1.0);
    MetalRowDataGateway m1 = new MetalRowDataGatewayRDS("metal", 1.0, 1, 1, 1, 1);
    
    CompoundMadeOfTableDataGatewayRDS.addCompoundMadeOf(c1.getCompoundID(), m1.getMetalID(), m1.toString());
    CompoundMadeOfDTO dtoCompoundMetal = new CompoundMadeOfDTO(c1.getCompoundID(), -1, m1.getMetalID());

    assertEquals(dtoCompoundMetal.getMetalID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByMetal(c1.getCompoundID()).get(0).getMetalID());
    assertEquals(dtoCompoundMetal.getElementID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByMetal(c1.getCompoundID()).get(0).getElementID());
    assertEquals(dtoCompoundMetal.getCompoundID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByMetal(c1.getCompoundID()).get(0).getCompoundID());
  }
  
  @Test
  void testDelete() throws DatabaseException {
    CompoundRowDataGateway c1 = new CompoundRowDataGatewayRDS("compound", 1.0);
    ElementRowDataGateway e1 = new ElementRowDataGatewayRDS("element", 1.0, 5, 10.0);
    
    CompoundMadeOfDTO dtoCompoundElement = new CompoundMadeOfDTO(c1.getCompoundID(), e1.getElementID(), -1);
    CompoundMadeOfTableDataGatewayRDS.addCompoundMadeOf(c1.getCompoundID(), e1.getElementID(), e1.toString());
    
    //makes sure it was inserted correctly
    assertEquals(dtoCompoundElement.getMetalID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByElement(c1.getCompoundID()).get(0).getMetalID());
    assertEquals(dtoCompoundElement.getElementID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByElement(c1.getCompoundID()).get(0).getElementID());
    assertEquals(dtoCompoundElement.getCompoundID(), CompoundMadeOfTableDataGatewayRDS.findCompoundsByElement(c1.getCompoundID()).get(0).getCompoundID()); 
    
    CompoundMadeOfTableDataGatewayRDS.deleteCompound(c1.getCompoundID());
    
    //No compounds will be found because of the delete, so the returning lists will have a length 0.
    assertEquals(0, CompoundMadeOfTableDataGatewayRDS.findCompoundsByElement(c1.getCompoundID()).size());
    assertEquals(0, CompoundMadeOfTableDataGatewayRDS.findCompoundsByElement(c1.getCompoundID()).size());
    assertEquals(0, CompoundMadeOfTableDataGatewayRDS.findCompoundsByElement(c1.getCompoundID()).size());
   
    
  }

}
