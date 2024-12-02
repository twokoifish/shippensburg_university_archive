package datasource;

/**
 * Acid data transfer object
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class AcidDTO {
  private int acidId, soluteId, soluteType;
  private String name;
  private double inventory;

  /**
   * Initialize an acid DTO.
   * 
   * @param acidId    int
   * @param soluteId  int
   * @param name      String
   * @param inventory double
   */
  public AcidDTO(int acidId, int soluteId, String name, double inventory, int soluteType) {
    this.acidId = acidId;
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
   * Get AcidId from DTO
   * 
   * @return int acidId
   */
  public int getAcidId() {
    return acidId;
  }

  /**
   * Get SoluteId from DTO
   * 
   * @return int soluteId
   */
  public int getSoluteId() {
    return soluteId;
  }

  /**
   * Get Name of Acid DTO
   * 
   * @return String name of acid
   */
  public String getName() {
    return name;
  }

  /**
   * Get Inventory amount of acid
   * 
   * @return double inventory
   */
  public double getInventory() {
    return inventory;
  }

  /**
   * set acid id
   * 
   * @param acidId int
   */
  public void setAcidId(int acidId) {
    this.acidId = acidId;
  }

  /**
   * set solute id
   * 
   * @param soluteId int
   */
  public void setSoluteId(int soluteId) {
    this.soluteId = soluteId;
  }

  /**
   * set name
   * 
   * @param name String
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * set inventory
   * 
   * @param inventory double
   */
  public void setInventory(double inventory) {
    this.inventory = inventory;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + acidId;
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
    AcidDTO other = (AcidDTO) obj;
    if (acidId != other.acidId)
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
