package command.metal;

import model.MetalDataMapper;
import command.CreateCommandInterface;
import model.DomainModelException;
import model.Metal;

/**
 * Command for creating a Metal object.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class MetalCreateCommand implements CreateCommandInterface {

  private String name;
  private double inventory;
  private int atomicNumber;
  private double atomicMass;
  private double acidAmount;

  /**
   * Constructor for MetalCreateCommand(String, double, int, double, double).
   * 
   * @param name,         the name of the Metal.
   * @param inventory,    the inventory of the Metal.
   * @param atomicNumber, the atomic number of the Metal.
   * @param atomicMass,   the atomic mass of the Metal.
   * @param acidAmount,   the amount of Acid needed to dissolve the Metal.
   */
  public MetalCreateCommand(String name, double inventory, int atomicNumber, double atomicMass, double acidAmount) {
    this.name = name;
    this.inventory = inventory;
    this.atomicNumber = atomicNumber;
    this.atomicMass = atomicMass;
    this.acidAmount = acidAmount;
  }

  /**
   * @see command.CreateCommandInterface#execute().
   */
  @Override
  public Metal execute() throws DomainModelException {
    try {
      if (name.split(" ").length > 1 || name.isBlank()) {
        throw new DomainModelException("Name is invalid. Must be one word AND not blank.");
      } else if (atomicNumber > atomicMass) {
        throw new DomainModelException("Atomic number cannot be larger than atomic mass.");
      } else if (inventory < 0) {
        throw new DomainModelException("Inventory is invalid. Must be >= 0.");
      } else if (acidAmount < 0) {
        throw new DomainModelException("Acid amount is invalid. Must be >= 0.");
      } else {
        return new MetalDataMapper().create(name, inventory, atomicNumber, atomicMass, acidAmount);
      }
    } catch (DomainModelException e) {
      throw new DomainModelException("Failed to create Metal.", e);
    }
  }

}
