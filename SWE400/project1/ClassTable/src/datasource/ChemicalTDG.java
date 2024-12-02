package datasource;

import java.util.List;

import database.DatabaseException;

/**
 * ChemicalTDG interface
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public interface ChemicalTDG {

  public ChemicalTDGRDS getAllChemicals();

  public ChemicalTDGRDS filterByName(String name);

  public ChemicalTDGRDS filterByInventory(double inventory);

  public ChemicalTDGRDS filterByInventoryRange(double high, double low);

  public ChemicalTDGRDS filterLowInventory(double inventoryNeeded);

  public List<ChemicalDTO> executeQuery() throws DatabaseException;
}
