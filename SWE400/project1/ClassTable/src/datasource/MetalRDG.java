package datasource;

import java.util.List;

/**
 * MetalRDG interface 
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public interface MetalRDG {
  
  public void delete();

  public void update();

   public List<MetalRDGRDS> findSet(int dissolvedById);

   public void setDissolvedById(int dissolvedById);

   public void setName(String name);

   public void setInventory(double inventory);
   
   public void setMoles(double moles);
   
   public void setAtomicNumber(int atomicNumber);
   
   public void setAtomicMass(double atomicMass);

   MetalDTO getMetal();

}
