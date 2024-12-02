package datadto;

public class AcidDTO {
  private int acidID, soluteID;
  private String name;
  private double inventory;
  private String soluteType;

  /**
   * Initialize an acid DTO.
   * @param acidID int
   * @param soluteID int
   * @param name String
   * @param inventory double
   */
  public AcidDTO(int acidID, String name,double inventory, int soluteID, String soluteType) {
    this.acidID = acidID;
    this.soluteID = soluteID;
    this.name = name;
    this.inventory = inventory;
    this.soluteType = soluteType;
  }

  /**
   * Get AcidId from DTO
   * @return int acidID
   */
  public int getAcidID() {
    return acidID;
  }

  /**
   * Get SoluteId from DTO
   * @return int soluteID
   */ 
  public int getSoluteID() {
    return soluteID;
  }

  /**
   * Get Name of Acid DTO
   * @return String name of acid
   */
  public String getName() {
    return name;
  }

  /**
   * Get Inventory amount of acid 
   * @return double inventory
   */
  public double getInventory() {
    return inventory;
  }
  
  public String getSoluteType() {
    return soluteType;
  }

  /**
   * set acid id
   * @param acidID int
   */ 
  public void setAcidID(int acidID) {
    this.acidID = acidID;
  }

  /**
   * set solute id
   * @param soluteID int
   */
  public void setSoluteID(int soluteID) {
    this.soluteID = soluteID;
  }

  /**
   * set name 
   * @param name String
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * set inventory
   * @param inventory double
   */
  public void setInventory(double inventory) {
    this.inventory = inventory;
  }
  
  public void setSoluteType(String soluteType) {
    this.soluteType = soluteType;
  }
}
