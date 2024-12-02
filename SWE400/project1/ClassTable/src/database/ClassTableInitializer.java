package database;

import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import datasource.AcidRDG;
import datasource.AcidRDGRDS;
import datasource.BaseRDG;
import datasource.BaseRDGRDS;
import datasource.ChemicalRDG;
import datasource.ChemicalRDGRDS;
import datasource.CompoundRDG;
import datasource.CompoundRDGRDS;
import datasource.ElementRDG;
import datasource.ElementRDGRDS;
import datasource.MetalRDG;
import datasource.MetalRDGRDS;

public class ClassTableInitializer {
  
  public static void createTables() {
    try {
      Statement stmt = DatabaseManager.getSingleton().getConnection().createStatement();
      
      String[] create = { 
          
          "CREATE TABLE IF NOT EXISTS Chemical(chemicalId INT NOT NULL AUTO_INCREMENT, name VARCHAR(20), "
          + "type INT, inventory DOUBLE, PRIMARY KEY (chemicalId));",
              
          "CREATE TABLE IF NOT EXISTS Acid" + "(acidId INT NOT NULL, solute INT, " 
              + "FOREIGN KEY(acidId) REFERENCES Chemical(chemicalId));",
              
          "CREATE TABLE IF NOT EXISTS Base(baseId INT NOT NULL, solute INT, "
              + "FOREIGN KEY(baseId) REFERENCES Chemical(chemicalId));",
              
          "CREATE TABLE IF NOT EXISTS Element(elementId INT NOT NULL, atomicNumber INT, "
              + "atomicMass DOUBLE, FOREIGN KEY(elementId) REFERENCES Chemical(chemicalId));",
              
          "CREATE TABLE IF NOT EXISTS Metal(metalId INT NOT NULL, dissolvedBy INT, moles DOUBLE, "
              + "FOREIGN KEY(dissolvedBy) REFERENCES Acid(acidId), "
              + "FOREIGN KEY(metalId) REFERENCES Element(elementId)); ",
              
          "CREATE TABLE IF NOT EXISTS Compound(compoundId INT NOT NULL, "
              + "elementId INT NOT NULL, FOREIGN KEY (compoundId) REFERENCES Chemical(chemicalId), "
              + "FOREIGN KEY (elementId) REFERENCES Element(elementId));"    
      };
      
      for (int i = 0; i < create.length; i++) {
        stmt.execute(create[i]);
      }
      
    } catch(DatabaseException | SQLException e) {
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
          "SET FOREIGN_KEY_CHECKS = 0;"
      };
      
      for (int i = 0; i < drop.length; i++ ) {
        stmt.execute(drop[i]);
      }
    } catch(SQLException | DatabaseException e) {
      e.printStackTrace();
    }
  }
  
  @SuppressWarnings("unused")
  public static void populateTables() {
    
    // Insert solutes
    AcidRDG solute1 = new AcidRDGRDS(0, "acidsoluteboy", 62.4, 0);
    BaseRDG solute2 = new BaseRDGRDS(0, "basesoluteboy", 64.2, 0); 
    
    AcidRDG acid = new AcidRDGRDS(1, "acidname1", 1.1, 1);
    acid = new AcidRDGRDS(1, "acidname2", 1.2, 1);
    acid = new AcidRDGRDS(1, "acidname3", 1.3, 1);
    acid = new AcidRDGRDS(1, "acidname4", 1.4, 1);
    acid = new AcidRDGRDS(2, "acidname5", 1.5, 2);
    acid = new AcidRDGRDS(2, "acidname6", 1.6, 2);
    acid = new AcidRDGRDS(2, "funkyacid1", 41.2, 2); 
    acid = new AcidRDGRDS(2, "funkyacid2", 42.4, 2); 
    
    // Bases
    BaseRDG base = new BaseRDGRDS(1, "basename1", 1.1, 1);
    base = new BaseRDGRDS(1, "basename2", 1.2, 1);
    base = new BaseRDGRDS(1, "basename3", 1.3, 1);
    base = new BaseRDGRDS(1, "basename4", 1.4, 1);
    base = new BaseRDGRDS(2, "basename5", 1.5, 2);
    base = new BaseRDGRDS(2, "basename6", 1.6, 2);
    base = new BaseRDGRDS(2, "funkybase1", 41.2, 2); 
    base = new BaseRDGRDS(2, "funkybase2", 42.4, 2); 
    
    // Elements
    ElementRDG element = new ElementRDGRDS(24, 19.2, "elementname1", 11.9);
    element = new ElementRDGRDS(25, 15.7, "elementname2", 40.2);
    element = new ElementRDGRDS(29, 12.9, "elementname3", 49.2);
    element = new ElementRDGRDS(31, 14.6, "elementname4", 45.7);
    
    // Chemicals
    ChemicalRDG gateway1 = new ChemicalRDGRDS("funkychem1", 41.2);
    ChemicalRDG gateway2 = new ChemicalRDGRDS("funkychem2", 42.4);
    
    // Compounds
    List<Integer> madeOf1 = new ArrayList<Integer>(), madeOf2 = new ArrayList<Integer>();
    madeOf1.add(19);
    madeOf1.add(20);
    madeOf1.add(21);
    madeOf2.add(22);

    CompoundRDG RDG1 = new CompoundRDGRDS(madeOf1, "compoundname1", 1.1); 
    CompoundRDG RDG2 = new CompoundRDGRDS(madeOf2, "compoundname2", 1.2); 
    
    // Metals
    MetalRDG metal = new MetalRDGRDS(1, 1, 1.1, 11.1, "metalname1", 41.1);
    metal = new MetalRDGRDS(2, 2, 2.1, 12.2, "metalname2", 42.1);
    metal = new MetalRDGRDS(3, 3, 3.1, 13.3, "metalname3", 43.1);
    metal = new MetalRDGRDS(4, 4, 4.1, 14.4, "metalname4", 44.1);
    metal = new MetalRDGRDS(5, 5, 5.1, 15.4, "metalname5", 45.1);
    
    
  }
}
