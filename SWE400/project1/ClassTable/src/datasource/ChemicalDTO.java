package datasource;

/**
 * Probably don't need this, but this is a Chemical data transfer object
 * 
 * @author Isabella Boone, Kim O'Neill
 */
public class ChemicalDTO {
  int chemicalId, soluteType;
  String name;
  double inventory;

  /**
   * Initialize a Chemical DTO
   * 
   * @param chemicalId
   * @param name
   * @param inventory
   */
  public ChemicalDTO(int chemicalId, String name, double inventory) {
    this.chemicalId = chemicalId;
    this.name = name;
    this.inventory = inventory;
  }

  /**
   * Get chemical id
   * 
   * @return chemicalId
   */
  public int getChemicalId() {
    return chemicalId;
  }

  /**
   * Get name
   * 
   * @return name
   */
  public String getName() {
    return name;
  }

  public int getSoluteType() {
    return soluteType;
  }
  
  public void setSoluteType(int soluteType) {
    this.soluteType = soluteType;
  }
  /**
   * Get inventory
   * 
   * @return inventory
   */
  public double getInventory() {
    return inventory;
  }

  /**
   * Set chemical id
   * 
   * @param chemicalId
   */
  public void setChemicalId(int chemicalId) {
    this.chemicalId = chemicalId;
  }

  /**
   * Set name
   * 
   * @param name
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Set inventory
   * 
   * @param inventory
   */
  public void setInventory(double inventory) {
    this.inventory = inventory;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + chemicalId;
    long temp;
    temp = Double.doubleToLongBits(inventory);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + ((name == null) ? 0 : name.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    ChemicalDTO other = (ChemicalDTO) obj;
    if (chemicalId != other.chemicalId)
      return false;
    if (Double.doubleToLongBits(inventory) != Double.doubleToLongBits(other.inventory))
      return false;
    if (name == null) {
      if (other.name != null)
        return false;
    } else if (!name.equals(other.name))
      return false;
    return true;
  }

}
