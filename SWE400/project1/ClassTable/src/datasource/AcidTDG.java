package datasource;

import java.util.List;

import database.DatabaseException;

/**
 * AcidTDG interface
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public interface AcidTDG {

  public AcidTDGRDS getAllAcids();

  public AcidTDGRDS filterByName(String name);

  public AcidTDGRDS filterByInventory(double inventory);

  public AcidTDGRDS filterBySolute(int solute);

  public AcidTDGRDS filterByInventoryRange(double high, double low);

  public List<AcidDTO> executeQuery() throws DatabaseException;

}
