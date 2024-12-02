package model;

/**
 * Class for creating an Element.
 * 
 * @author andrewjanuszko & morgan williams-burrell
 */
public class Element extends Chemical {

  private int atomicNumber;
  private double atomicMass;

  /**
   * Constructor to create an Element.
   * 
   * @param id           the ID of the Element.
   * @param name         the name of the Element.
   * @param inventory    the inventory of the Element.
   * @param atomicNumber the atomic number of the Element.
   * @param atomicMass   the atomic mass of the Element.
   */
  public Element(int id, String name, double inventory, int atomicNumber, double atomicMass) {
    super(id, name, inventory);
    setAtomicNumber(atomicNumber);
    setAtomicMass(atomicMass);
  }

  /**
   * Get the atomic number of the Element.
   * 
   * @return the atomic number of the Element.
   */
  public int getAtomicNumber() {
    return atomicNumber;
  }

  /**
   * Get the atomic mass of the Element.
   * 
   * @return the atomic mass of the Element.
   */
  public double getAtomicMass() {
    return atomicMass;
  }

  /**
   * Set the atomic number of the Element.
   * 
   * @param atomicNumber the atomic number of the Element.
   */
  public void setAtomicNumber(int atomicNumber) {
    this.atomicNumber = atomicNumber;
  }

  /**
   * Set the atomic mass of the Element.
   * 
   * @param atomicMass the atomic mass of the Element.
   */
  public void setAtomicMass(double atomicMass) {
    this.atomicMass = atomicMass;
  }

}
