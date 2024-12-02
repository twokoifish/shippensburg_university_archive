package command.element;

import model.ElementDataMapper;
import command.CreateCommandInterface;
import model.DomainModelException;
import model.Element;

/**
 * Command for creating an Element object.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class ElementCreateCommand implements CreateCommandInterface {

  private String name;
  private double inventory;
  private int atomicNumber;
  private double atomicMass;

  /**
   * Constructor for ElementCreateCommand(String, double, int, double).
   * 
   * @param name,         the name of the Element.
   * @param inventory,    the inventory of the Element.
   * @param atomicNumber, the atomic number of the Element.
   * @param atomicMass,   the atomic mass of the Element.
   */
  public ElementCreateCommand(String name, double inventory, int atomicNumber, double atomicMass) {
    this.name = name;
    this.inventory = inventory;
    this.atomicNumber = atomicNumber;
    this.atomicMass = atomicMass;
  }

  /**
   * @see command.CreateCommandInterface#execute().
   */
  @Override
  public Element execute() throws DomainModelException {
    try {
      if (name.split(" ").length > 1 || name.isBlank()) {
        throw new DomainModelException("Name is invalid. Must be one word AND not blank.");
      } else if (atomicNumber > atomicMass) {
        throw new DomainModelException("Atomic number cannot be larger than atomic mass.");
      } else if (inventory < 0) {
        throw new DomainModelException("Inventory is invalid. Must be >= 0.");
      } else {
        return new ElementDataMapper().create(name, inventory, atomicNumber, atomicMass);
      }
    } catch (DomainModelException e) {
      throw new DomainModelException("Failed to create Acid.", e);
    }
  }

}
