package datadto;

public class CompoundDTO {
  private int compoundID;
  
  private String name;
  private double inventory;

  public CompoundDTO(int id, String name, double inv) {
    this.compoundID = id;
    this.name = name;
    this.inventory = inv;
  }

  public int getCompoundID() {
    return compoundID;
  }

  public String getName() {
    return name;
  }

  public double getInventory() {
    return inventory;
  }

  public void setCompoundID(int compoundId) {
    this.compoundID = compoundId;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setInventory(double inventory) {
    this.inventory = inventory;
  }
}
