package datasource;

/**
 * ElementRDG Interface
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public interface ElementRDG {

  public void delete();

  public void update();

  public ElementDTO findByAtomicNumber(int atomicNum);

  public ElementDTO findByAtomicMass(double atomicMass);

  public void setAtomicNumber(int atomicNumber);

  public void setAtomicMass(double atomicMass);

  public void setName(String name);

  public void setInventory(double inventory);

  ElementDTO getElement();

}
