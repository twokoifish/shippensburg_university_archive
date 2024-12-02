package datasource;

import java.util.List;

/**
 * AcidRDG interface
 * 
 * @author Isabella Boone, Kim O'Neill
 */
public interface AcidRDG {

  public void setSolute(int newSolute);

  public void setName(String newName);

  public void setInventory(double inventory);
  
  public void setSoluteType(int s);

  public AcidDTO getAcid();

  public void update();

  public void delete();

  public List<AcidRDGRDS> findSet(int i);

}
