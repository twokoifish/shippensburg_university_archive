package all;

import dataENUM.ChemicalEnum;
import datasource.ChemicalRowDataGateway;
import datasource.DatabaseException;
import datasource.ElementCompoundTableDataGateway;

/**
 * Fill the database with sample data.
 * @author andrewjanuszko
 *
 */
public class PopulateDatabase {

  @SuppressWarnings("unused")
  public static void main(String[] args) throws DatabaseException {
    
    System.out.println("Populating database, please wait...");
    ChemicalRowDataGateway.createTable();
    ElementCompoundTableDataGateway.getSingletonInstance().createTable();
    
    /**
     * Insert Elements into the table.
     */
    ChemicalRowDataGateway hydrogen = new ChemicalRowDataGateway(ChemicalEnum.ELEMENT.getIntValue(), "Hydrogen", 3.14, 1, 1.008, 0, 0, 0);
    ChemicalRowDataGateway helium = new ChemicalRowDataGateway(ChemicalEnum.ELEMENT.getIntValue(), "Helium", 6.28, 2, 4.003, 0, 0, 0);
    ChemicalRowDataGateway carbon = new ChemicalRowDataGateway(ChemicalEnum.ELEMENT.getIntValue(), "Carbon", 9.42, 6, 12.011, 0, 0, 0);
    ChemicalRowDataGateway nitrogen = new ChemicalRowDataGateway(ChemicalEnum.ELEMENT.getIntValue(), "Nitrogen", 12.56, 7, 14.007, 0, 0, 0);
    ChemicalRowDataGateway oxygen = new ChemicalRowDataGateway(ChemicalEnum.ELEMENT.getIntValue(), "Oxygen", 15.70, 8, 15.999, 0, 0, 0);
    ChemicalRowDataGateway chlorine = new ChemicalRowDataGateway(ChemicalEnum.ELEMENT.getIntValue(), "Chlorine", 18.84, 17, 35.45, 0, 0, 0);
    
    /**
     * Insert Metals into the table.
     */
    ChemicalRowDataGateway sodium = new ChemicalRowDataGateway(ChemicalEnum.METAL.getIntValue(), "Sodium", 21.98, 11, 22.990, 0, 5, 0);
    ChemicalRowDataGateway iron = new ChemicalRowDataGateway(ChemicalEnum.METAL.getIntValue(), "Iron", 25.12, 26, 55.938, 0, 10, 0);
    ChemicalRowDataGateway copper = new ChemicalRowDataGateway(ChemicalEnum.METAL.getIntValue(), "Copper", 28.26, 29, 63.546, 0, 15, 0);
    ChemicalRowDataGateway mercury = new ChemicalRowDataGateway(ChemicalEnum.METAL.getIntValue(), "Mercury", 31.40, 80, 200.59, 0, 6, 0);
    ChemicalRowDataGateway zinc = new ChemicalRowDataGateway(ChemicalEnum.METAL.getIntValue(), "Zinc", 34.54, 30, 65.38, 0, 2, 0);
    ChemicalRowDataGateway silver = new ChemicalRowDataGateway(ChemicalEnum.METAL.getIntValue(), "Silver", 37.68, 47, 107.87, 0, 12, 0);
    
    /**
     * Insert Compounds into the table.
     */
    ChemicalRowDataGateway hydrogenDioxide = new ChemicalRowDataGateway(ChemicalEnum.COMPOUND.getIntValue(), "Hydrogen Dioxide", 10, 0, 0, 0, 0, 0);
    ChemicalRowDataGateway carbonMonoxide = new ChemicalRowDataGateway(ChemicalEnum.COMPOUND.getIntValue(), "Carbon Monoxide", 20, 0, 0, 0, 0, 0);
    ChemicalRowDataGateway sodiumChloride = new ChemicalRowDataGateway(ChemicalEnum.COMPOUND.getIntValue(), "Sodium Chloride", 30, 0, 0, 0, 0, 0);
    ChemicalRowDataGateway sucrose = new ChemicalRowDataGateway(ChemicalEnum.COMPOUND.getIntValue(), "Sucrose", 15, 0, 0, 0, 0, 0);
    ChemicalRowDataGateway silverChloride = new ChemicalRowDataGateway(ChemicalEnum.COMPOUND.getIntValue(), "Silver Chloride", 25, 0, 0, 0, 0, 0);
    ChemicalRowDataGateway nitrate = new ChemicalRowDataGateway(ChemicalEnum.COMPOUND.getIntValue(), "Nitrate", 35, 0, 0, 0, 0, 0);
    
    /**
     * Insert Acids into the table.
     */
    ChemicalRowDataGateway hydrochloricAcid = new ChemicalRowDataGateway(ChemicalEnum.ACID.getIntValue(), "Hydrochloric Acid", 10, 0, 0, 0, 0, 1);
    ChemicalRowDataGateway nitricAcid = new ChemicalRowDataGateway(ChemicalEnum.ACID.getIntValue(), "Nitric Acid", 20, 0, 0, 0, 0, 2);
    ChemicalRowDataGateway sulfuricAcid = new ChemicalRowDataGateway(ChemicalEnum.ACID.getIntValue(), "Sulfuric Acid", 30, 0, 0, 0, 0, 3);
    ChemicalRowDataGateway carbonicAcid = new ChemicalRowDataGateway(ChemicalEnum.ACID.getIntValue(), "Carbonic Acid", 40, 0, 0, 0, 0, 4);
    ChemicalRowDataGateway formicAcid = new ChemicalRowDataGateway(ChemicalEnum.ACID.getIntValue(), "Formic Acid", 10, 0, 0, 0, 0, 5);
    ChemicalRowDataGateway citricAcid = new ChemicalRowDataGateway(ChemicalEnum.ACID.getIntValue(), "Citric Acid", 10, 0, 0, 0, 0, 6);
    
    sodium.setDissolvedBy(nitricAcid.getID());
    sodium.update();
    iron.setDissolvedBy(carbonicAcid.getID());
    iron.update();
    copper.setDissolvedBy(hydrochloricAcid.getID());
    copper.update();
    mercury.setDissolvedBy(citricAcid.getID());
    mercury.update();
    zinc.setDissolvedBy(sulfuricAcid.getID());
    zinc.update();
    silver.setDissolvedBy(formicAcid.getID());
    silver.update();
    
    /**
     * Insert Bases into the table.
     */
    ChemicalRowDataGateway potassiumHydroxide = new ChemicalRowDataGateway(ChemicalEnum.BASE.getIntValue(), "Potassium Hydroxide", 10, 0, 0, 0, 0, 7);
    ChemicalRowDataGateway sodiumHydroxide = new ChemicalRowDataGateway(ChemicalEnum.BASE.getIntValue(), "Sodium Hydroxide", 70, 0, 0, 0, 0, 8);
    ChemicalRowDataGateway calciumHydroxide = new ChemicalRowDataGateway(ChemicalEnum.BASE.getIntValue(), "Calcium Hydroxide", 18, 0, 0, 0, 0, 9);
    ChemicalRowDataGateway lithiumHydroxide = new ChemicalRowDataGateway(ChemicalEnum.BASE.getIntValue(), "Lithium Hydroxide", 30, 0, 0, 0, 0, 10);
    ChemicalRowDataGateway bariumHydroxide = new ChemicalRowDataGateway(ChemicalEnum.BASE.getIntValue(), "Barium Hydroxide", 10, 0, 0, 0, 0, 11);
    ChemicalRowDataGateway strontiumHydroxide = new ChemicalRowDataGateway(ChemicalEnum.BASE.getIntValue(), "Strontium Hydroxide",20, 0, 0, 0, 0, 12);
    /**
     * Create link Elements to Compound 'hydrogenDioxide'
     */
    ElementCompoundTableDataGateway.getSingletonInstance().create(hydrogenDioxide.getID(), hydrogen.getID());
    ElementCompoundTableDataGateway.getSingletonInstance().create(hydrogenDioxide.getID(), oxygen.getID());
    
    /**
     * Create link Elements to Compound 'Sucrose'
     */
    ElementCompoundTableDataGateway.getSingletonInstance().create(sucrose.getID(), carbon.getID());
    ElementCompoundTableDataGateway.getSingletonInstance().create(sucrose.getID(), hydrogen.getID());
    ElementCompoundTableDataGateway.getSingletonInstance().create(sucrose.getID(), oxygen.getID());
    
    /**
     * Create link Elements to Compound 'Nitrate'
     */
    ElementCompoundTableDataGateway.getSingletonInstance().create(nitrate.getID(), nitrogen.getID());
    ElementCompoundTableDataGateway.getSingletonInstance().create(nitrate.getID(), oxygen.getID());
    
    /**
     * Create link Elements to Compound 'Carbon Monoxide'
     */
    ElementCompoundTableDataGateway.getSingletonInstance().create(carbonMonoxide.getID(), carbon.getID());
    ElementCompoundTableDataGateway.getSingletonInstance().create(carbonMonoxide.getID(), oxygen.getID());
    
    /**
     * Create link Elements to Compound 'Sodium Chloride'
     */
    ElementCompoundTableDataGateway.getSingletonInstance().create(sodiumChloride.getID(), sodium.getID());
    ElementCompoundTableDataGateway.getSingletonInstance().create(sodiumChloride.getID(), chlorine.getID());
    
    /**
     * Create link Elements to Compound 'Silver Chloride'
     */
    ElementCompoundTableDataGateway.getSingletonInstance().create(silverChloride.getID(), silver.getID());
    ElementCompoundTableDataGateway.getSingletonInstance().create(silverChloride.getID(), chlorine.getID());
    
    System.out.println("Database populated. Have a good day!");
  }
  
}
