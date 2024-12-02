package datasource;

/**
 * Element data transfer object
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class ElementDTO {
  int elementId, atomicNumber;
  double atomicMass, inventory;
  String name;

  /**
   * Initialize an ElementDTO 
   * @param elementId of the element
   * @param atomicNumber of the element
   * @param atomicMass of the element
   * @param name of the element
   * @param inventory of the element
   */
  public ElementDTO(int elementId, int atomicNumber, double atomicMass, String name, double inventory) {
    this.elementId = elementId;
    this.atomicNumber = atomicNumber;
    this.atomicMass = atomicMass;
    this.name = name;
    this.inventory = inventory;
  }
  
  /**
   * Get element id of the dto
   * @return element id
   */
  public int getElementId() {
    return elementId;
  }

  /**
   * Get atomic number
   * @return atomic number
   */
  public int getAtomicNumber() {
    return atomicNumber;
  }

  /**
   * Get atomic mass
   * @return atomic mass
   */
  public double getAtomicMass() {
    return atomicMass;
  }

  /**
   * Get inventory
   * @return inventory
   */
  public double getInventory() {
    return inventory;
  }

  /**
   * Get name
   * @return name
   */
  public String getName() {
    return name;
  }

  /**
   * Set atomic number
   * @param atomicNumber
   */
  public void setAtomicNumber(int atomicNumber) {
    this.atomicNumber = atomicNumber;
  }

  /**
   * Set atomic mass
   * @param atomicMass
   */
  public void setAtomicMass(double atomicMass) {
    this.atomicMass = atomicMass;
  }

  /**
   * Set inventory
   * @param inventory
   */
  public void setInventory(double inventory) {
    this.inventory = inventory;
  }

  /**
   * Set name
   * @param name
   */
  public void setName(String name) {
    this.name = name;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    long temp;
    temp = Double.doubleToLongBits(atomicMass);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + atomicNumber;
    result = prime * result + elementId;
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
    ElementDTO other = (ElementDTO) obj;
    if (Double.doubleToLongBits(atomicMass) != Double.doubleToLongBits(other.atomicMass))
      return false;
    if (atomicNumber != other.atomicNumber)
      return false;
    if (elementId != other.elementId)
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
