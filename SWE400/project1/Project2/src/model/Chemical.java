package model;

/**
 * Abstract class for creating Chemicals.
 * 
 * @author andrewjanuszko & morgan williams-burrell
 */
public abstract class Chemical {

  private int id;
  private String name;
  private double inventory;

  /**
   * Constructor for a Chemical object.
   * 
   * @param id        the ID of the Chemical.
   * @param name      the name of the Chemical.
   * @param inventory the current inventory of the Chemical.
   */
  public Chemical(int id, String name, double inventory) {
    setID(id);
    setName(name);
    setInventory(inventory);
  }

  /**
   * Sets the ID of the Chemical.
   * 
   * @param id the ID of the Chemical.
   */
  private void setID(int id) {
    this.id = id;
  }

  /**
   * Sets the name of the Chemical.
   * 
   * @param name the name of the Chemical.
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Sets the inventory of the Chemical.
   * 
   * @param inventory the inventory of the Chemical.
   */
  public void setInventory(double inventory) {
    this.inventory = inventory;
  }

  /**
   * Gets the ID of the Chemical.
   * 
   * @return the ID of the Chemical.
   */
  public int getID() {
    return id;
  }

  /**
   * Gets the name of the Chemical.
   * 
   * @return the name of the Chemical.
   */
  public String getName() {
    return name;
  }

  /**
   * Gets the inventory of the Chemical.
   * 
   * @return the inventory of the Chemical.
   */
  public double getInventory() {
    return inventory;
  }

}
