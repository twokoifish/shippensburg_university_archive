package datasource;

/**
 * ElementTDG interface
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public interface ElementTDG {
  public ElementTDGRDS getAllElements();

  public ElementTDGRDS filterByName(String name);

  public ElementTDGRDS filterByInventory(double inventory);

  public ElementTDGRDS filterByInventoryRange(double high, double low);

  public ElementTDGRDS filterByAtomicMass(double atomicMass);

  public ElementTDGRDS filterByAtomicMassRange(double high, double low);

  public ElementTDGRDS filterByAtomicNumber(int atomicNumber);

  public ElementTDGRDS filterByAtomicNumberRange(int high, int low);

}
