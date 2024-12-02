package datadto;

public class ElementDTO {
  private int elementID, atomicNumber;
  private double atomicMass, inventory;
  private String name;

  public ElementDTO(int id, String name, double inventory, int atomicNumber, double atomicMass) {
    this.elementID = id;
    this.atomicNumber = atomicNumber;
    this.atomicMass = atomicMass;
    this.name = name;
    this.inventory = inventory;
  }

  public int getID() {
    return elementID;
  }

  public int getAtomicNumber() {
    return atomicNumber;
  }

  public double getAtomicMass() {
    return atomicMass;
  }

  public double getInventory() {
    return inventory;
  }

  public String getName() {
    return name;
  }

  public void setID(int elementID) {
    this.elementID = elementID;
  }

  public void setAtomicNumber(int atomicNumber) {
    this.atomicNumber = atomicNumber;
  }

  public void setAtomicMass(double atomicMass) {
    this.atomicMass = atomicMass;
  }

  public void setInventory(double inventory) {
    this.inventory = inventory;
  }

  public void setName(String name) {
    this.name = name;
  }
}
