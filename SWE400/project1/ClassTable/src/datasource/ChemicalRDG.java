package datasource;

/**
 * ChemicalRDG interface
 * 
 * @author Isabella Boone, Kim O'Neill
 */
public interface ChemicalRDG {

  public void delete();

  public void update();

  public ChemicalDTO getChemical();

  public void setName(String newName);

  public void setInventory(double inventory);

}
