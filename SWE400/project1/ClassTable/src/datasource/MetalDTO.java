package datasource;

/**
 * Metal data transfer object
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public class MetalDTO {
  int metalId, dissolvedById, atomicNumber;
  String name;
  double inventory, atomicMass, moles;

  /**
   * Initialize a metal DTO 
   * @param metalId
   * @param dissolvedById
   * @param atomicNumber
   * @param atomicMass
   * @param moles
   * @param name
   * @param inventory
   */
  public MetalDTO(int metalId, int dissolvedById, int atomicNumber, double atomicMass, double moles, String name,
      double inventory) {
    this.metalId = metalId;
    this.dissolvedById = dissolvedById;
    this.atomicNumber = atomicNumber;
    this.atomicMass = atomicMass;
    this.moles = moles;
    this.name = name;
    this.inventory = inventory;
  }

  /**
   * Get metalId
   * @return metalId
   */
  public int getMetalId() {
    return metalId;
  }

  /**
   * Get moles
   * @return moles
   */
  public double getMoles() {
    return moles;
  }

  /**
   * Set moles
   * @param moles
   */
  public void setMoles(double moles) {
    this.moles = moles;
  }

  /**
   * Get dissolvedById 
   * @return dissolvedById
   */
  public int getDissolvedById() {
    return dissolvedById;
  }

  /**
   * Get name
   * @return name
   */
  public String getName() {
    return name;
  }

  /**
   * Get inventory
   * @return inventory
   */
  public double getInventory() {
    return inventory;
  }

  /**
   * Set dissolvedById
   * @param dissolvedById
   */
  public void setDissolvedById(int dissolvedById) {
    this.dissolvedById = dissolvedById;
  }

  /**
   * Set name
   * @param name
   */
  public void setName(String name) {
    this.name = name;
  }

  /** 
   * Set inventory
   * @param inventory
   */
  public void setInventory(double inventory) {
    this.inventory = inventory;
  }

  /**
   * Get atomic number
   * @return atomicNumber
   */
  public int getAtomicNumber() {
    return atomicNumber;
  }

  /**
   * Set atomic number
   * @param atomicNumber
   */
  public void setAtomicNumber(int atomicNumber) {
    this.atomicNumber = atomicNumber;
  }

  /**
   * Get atomic mass
   * @return atomicMass
   */
  public double getAtomicMass() {
    return atomicMass;
  }

  /**
   * Set atomicMass
   * @param atomicMass
   */
  public void setAtomicMass(double atomicMass) {
    this.atomicMass = atomicMass;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + dissolvedById;
    long temp;
    temp = Double.doubleToLongBits(inventory);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + metalId;
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
    MetalDTO other = (MetalDTO) obj;
    if (dissolvedById != other.dissolvedById)
      return false;
    if (Double.doubleToLongBits(inventory) != Double.doubleToLongBits(other.inventory))
      return false;
    if (metalId != other.metalId)
      return false;
    if (name == null) {
      if (other.name != null)
        return false;
    } else if (!name.equals(other.name))
      return false;
    return true;
  }

}
