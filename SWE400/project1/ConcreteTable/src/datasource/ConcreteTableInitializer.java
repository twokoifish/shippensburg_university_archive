package datasource;

import java.sql.SQLException;
import java.sql.Statement;

import model.AcidDataMapper;
import model.BaseDataMapper;
import model.CompoundDataMapper;
import model.ElementDataMapper;
import model.MetalDataMapper;

public class ConcreteTableInitializer {
  public static void createTables() {
    try {
      Statement stmt = DatabaseManager.getSingleton().getConnection().createStatement();

      String[] create = {
          "CREATE TABLE Acid (" + "acidID INT NOT NULL AUTO_INCREMENT, " + "name VARCHAR(30) NOT NULL, "
              + "inventory Double, " + "solute INT, " + 
              "soluteType VARCHAR(30), " + "UNIQUE(name), " + "PRIMARY KEY(acidID) );",
          "CREATE TABLE Base (" + "baseID INT NOT NULL AUTO_INCREMENT, " + "name VARCHAR(30) NOT NULL, "
              + "inventory DOUBLE, " + "solute INT, " + 
              "soluteType VARCHAR(30), " + "UNIQUE(name), " + "PRIMARY KEY(baseID)) ;",
          "CREATE TABLE Metal (" + "metalID INT NOT NULL AUTO_INCREMENT, " + "name VARCHAR(30) NOT NULL, "
              + "inventory Double, " + "atomicNumber INT NOT NULL, " + "atomicMass DOUBLE NOT NULL, "
              + "acidAmount DOUBLE NOT NULL, " + "dissolvedBy INT, " + "UNIQUE(name), " + "PRIMARY KEY(metalID), "
              + "FOREIGN KEY(dissolvedBy) REFERENCES Acid(acidID)); ",
          "CREATE TABLE Element (" + "elementID INT NOT NULL AUTO_INCREMENT, " + "name VARCHAR(30) NOT NULL, "
              + "inventory Double, " + "atomicNumber INT NOT NULL, " + "atomicMass DOUBLE NOT NULL," + "UNIQUE(name),"
              + "PRIMARY KEY(elementID));",
          "CREATE TABLE Compound (" + "compoundID INT NOT NULL AUTO_INCREMENT, " + "name VARCHAR(30) NOT NULL, "
              + "inventory Double, " + "UNIQUE(name), " + "PRIMARY KEY(compoundID)) ;",
          "CREATE TABLE CompoundMadeOf (" + "compoundID INT NOT NULL," + "elementID INT," + "metalID INT,"
              + "FOREIGN KEY(compoundID) REFERENCES Compound(compoundID), "
              + "FOREIGN KEY(elementID) REFERENCES Element(elementID), "
              + "FOREIGN KEY(metalID) REFERENCES Metal(metalID));" };
      
      for (int i = 0; i < create.length; i++) {
        stmt.execute(create[i]);
      }
    } catch (DatabaseException | SQLException e) {
      e.printStackTrace();
    }
  }
  
  public static void dropTables() {
    try {
      Statement stmt = DatabaseManager.getSingleton().getConnection().createStatement();
      
      String[] drop = {    
          "SET FOREIGN_KEY_CHECKS = 0;",
          "DROP TABLE IF EXISTS Chemical;",
          "DROP TABLE IF EXISTS Acid;",
          "DROP TABLE IF EXISTS Base;",
          "DROP TABLE IF EXISTS Compound;",
          "DROP TABLE IF EXISTS Element;",
          "DROP TABLE IF EXISTS Metal;",
          "DROP TABLE IF EXISTS CompoundMadeOf;",
          "SET FOREIGN_KEY_CHECKS = 0;"
      };
      
      for (int i = 0; i < drop.length; i++ ) {
        stmt.execute(drop[i]);
      }
    } catch(SQLException | DatabaseException e) {
      e.printStackTrace();
    }
  }
  public static void clearMaps() {
    AcidDataMapper.acidMap.clear();
    BaseDataMapper.baseMap.clear();
    CompoundDataMapper.compoundMap.clear();
    ElementDataMapper.elementMap.clear();
    MetalDataMapper.metalMap.clear();
    
  }
  
  private static void populate() throws DatabaseException {
    ElementRowDataGateway e2 = new ElementRowDataGatewayRDS("Helium", 2, 2, 1000.0);
    ElementRowDataGateway e1 = new ElementRowDataGatewayRDS("Hydrogen", 10, 1, 50.0);
    ElementRowDataGateway e3 = new ElementRowDataGatewayRDS("Chlorine", 10, 17, 50.0);
    ElementRowDataGateway e4 = new ElementRowDataGatewayRDS("Oxygen", 10, 8, 50.0);

    MetalRowDataGateway m1 = new MetalRowDataGatewayRDS("Sodium", 10, 11, 22.990, 5, -1);
    MetalRowDataGateway m2 = new MetalRowDataGatewayRDS("Iron", 10, 26, 55.938, 5, -1);
    MetalRowDataGateway m3 = new MetalRowDataGatewayRDS("Copper", 10, 29, 69.420, 5, -1);
    
    CompoundRowDataGateway c1 = new CompoundRowDataGatewayRDS("Hydrogen Chloride", 10);
    
    CompoundMadeOfTableDataGatewayRDS.addCompoundMadeOf(c1.getCompoundID(), e1.getElementID(), "Element");
    CompoundMadeOfTableDataGatewayRDS.addCompoundMadeOf(c1.getCompoundID(), e3.getElementID(), "Element");
    
    BaseRowDataGateway b1 = new BaseRowDataGatewayRDS("Potassium Hydroxide", 55, e4.getElementID(), "Element");
    
    AcidRowDataGateway a = new AcidRowDataGatewayRDS("Hydroclhoric Acid", 10.0, m3.getMetalID(), "Metal");
  }
  
  public static void main(String[] args) {
    dropTables();
    createTables();
    try {
      populate();
    } catch (DatabaseException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }
}
