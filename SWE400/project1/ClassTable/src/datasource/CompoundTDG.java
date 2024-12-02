package datasource;

import java.util.List;

import database.DatabaseException;

/**
 * CompoundTDG interface
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public interface CompoundTDG {

  List<CompoundDTO> executeQuery() throws DatabaseException;

  CompoundTDGRDS filterByName(String name);

  CompoundTDGRDS filterByInventoryRange(double high, double low);

  CompoundTDGRDS filterByInventory(double inventory);

  CompoundTDGRDS filterByElements(int elementId);

  CompoundTDGRDS getAllCompounds();

}
