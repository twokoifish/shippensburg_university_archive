package datasource;

/**
 * MetalTDG interface. 
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public interface MetalTDG {
  
  public MetalTDGRDS getAllMetals();

  public MetalTDGRDS filterByName(String name);

  public MetalTDGRDS filterByInventory(double inventory);

  public MetalTDGRDS filterByInventoryRange(double high, double low);
  
  public MetalTDGRDS filterByAtomicMass(double atomicMass);
  
  public MetalTDGRDS filterByAtomicMassRange(double high, double low);
  
  public MetalTDGRDS filterByAtomicNumber(int atomicNumber);
  
  public MetalTDGRDS filterByAtomicNumberRange(int high, int low);
  
  public MetalTDGRDS filterByDissolvedBy(int dissolvedBy);
  
  public MetalTDGRDS filterByMoles(double moles);
  
  public MetalTDGRDS filterByMolesRange(double high, double low);
  
}
