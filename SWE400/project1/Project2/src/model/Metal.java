package model;

/**
 * Class for creating a Metal.
 * 
 * @author andrewjanuszko & morgan williams-burrell
 */
public class Metal extends Element {

  private double acidAmount;

  /**
   * Constructor for creating a Metal.
   * 
   * @param id           the ID of the Metal.
   * @param name         the name of the Metal.
   * @param inventory    the inventory of the Metal.
   * @param atomicNumber the atomic number of the Metal.
   * @param atomicMass   the atomic mass of the Metal.
   * @param acidAmount   the amount of Acid required to dissolve a Metal.
   */
  public Metal(int id, String name, double inventory, int atomicNumber, double atomicMass, double acidAmount) {
    super(id, name, inventory, atomicNumber, atomicMass);
    setAcidAmount(acidAmount);
  }

  /**
   * Get the amount of Acid required to dissolve a Metal.
   * 
   * @return the amount of Acid required to dissolve a Metal.
   */
  public double getAcidAmount() {
    return acidAmount;
  }

  /**
   * Set the amount of Acid required to dissolve a Metal.
   * 
   * @param acidRequired the amount of Acid required to dissolve a Metal.
   */
  public void setAcidAmount(double acidAmount) {
    this.acidAmount = acidAmount;
  }

}
