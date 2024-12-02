package datadto;

public class BaseDTO {
  private int baseID, soluteID;
  private String name;
  private double inventory;
  private String soluteType;

  public BaseDTO(int baseID, String name, double inventory, int soluteID, String soluteType) {
    this.baseID = baseID;
    this.soluteID = soluteID;
    this.name = name;
    this.inventory = inventory;
    this.soluteType = soluteType;
  }

  public int getBaseID() {
    return baseID;
  }

  public int getSoluteID() {
    return soluteID;
  }

  public String getName() {
    return name;
  }

  public double getInventory() {
    return inventory;
  }
  
  public String getSoluteType() {
    return soluteType;
  }
  
  public void setBaseID(int baseID) {
    this.baseID = baseID;
  }

  public void setSoluteID(int soluteID) {
    this.soluteID = soluteID;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setInventory(double inventory) {
    this.inventory = inventory;
  }
  
  public void setSoluteType(String soluteType) {
    this.soluteType = soluteType;
  }
}
