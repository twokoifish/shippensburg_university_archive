package datasource;

/**
 * Base data transfer object
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class BaseDTO {
  private int baseId, soluteId, soluteType;
  private String name;
  private double inventory; 

  /**
   * Initialize a base DTO
   * 
   * @param baseId    of the base
   * @param soluteId  of the base
   * @param name      of the base
   * @param inventory of the base
   */
  public BaseDTO(int baseId, int soluteId, String name, double inventory, int soluteType) {
    this.baseId = baseId;
    this.soluteId = soluteId;
    this.name = name;
    this.inventory = inventory;
    this.soluteType = soluteType; 
  }
  
  public void setSoluteType(int soluteType) {
    this.soluteType = soluteType; 
  }
  
  public int getSoluteType() {
    return soluteType;
  }

  /**
   * Get base id of dto
   * 
   * @return baseID
   */
  public int getBaseId() {
    return baseId;
  }

  /**
   * Get solute id of the dto
   * 
   * @return soluteId
   */
  public int getSoluteId() {
    return soluteId;
  }

  /**
   * Get name of the dto
   * 
   * @return name
   */
  public String getName() {
    return name;
  }

  /**
   * Get inventory of dto
   * 
   * @return inventory
   */
  public double getInventory() {
    return inventory;
  }

  /**
   * Set solute id
   * 
   * @param soluteId
   */
  public void setSoluteId(int soluteId) {
    this.soluteId = soluteId;
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
    result = prime * result + baseId;
    long temp;
    temp = Double.doubleToLongBits(inventory);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + ((name == null) ? 0 : name.hashCode());
    result = prime * result + soluteId;
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
    BaseDTO other = (BaseDTO) obj;
    if (baseId != other.baseId)
      return false;
    if (Double.doubleToLongBits(inventory) != Double.doubleToLongBits(other.inventory))
      return false;
    if (name == null) {
      if (other.name != null)
        return false;
    } else if (!name.equals(other.name))
      return false;
    if (soluteId != other.soluteId)
      return false;
    return true;
  }

}
