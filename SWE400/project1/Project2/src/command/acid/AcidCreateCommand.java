package command.acid;

import java.util.List;
import command.CreateCommandInterface;
import model.Acid;
import model.AcidDataMapper;
import model.Chemical;
import model.DomainModelException;
import model.Metal;

/**
 * Command for creating an Acid object.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class AcidCreateCommand implements CreateCommandInterface {

  private String name;
  private double inventory;
  private List<Metal> dissolves;
  private Chemical solute;

  /**
   * Constructor for AcidCreateCommand(String, double, List<Metal>, int).
   * 
   * @param name,      the name of the Acid.
   * @param inventory, the inventory of the Acid.
   * @param dissolves, the Metals dissolved by the Acid.
   * @param solute,    the ID of the solute for the Acid.
   */
  public AcidCreateCommand(String name, double inventory, List<Metal> dissolves, Chemical solute) {
    this.name = name;
    this.inventory = inventory;
    this.dissolves = dissolves;
    this.solute = solute;
  }

  /**
   * @see command.CreateCommandInterface#execute().
   */
  @Override
  public Acid execute() throws DomainModelException {
    try {
      if (name.split(" ").length < 2 || name.isBlank()) {
        throw new DomainModelException("Name is invalid. Must be >= 2 words.");
      } else if (inventory < 0) {
        throw new DomainModelException("Inventory is invalid. Must be >= 0.");
      } else {
        return new AcidDataMapper().create(name, inventory, dissolves, solute);
      }
    } catch (DomainModelException e) {
      throw new DomainModelException("Failed to create Acid.", e);
    }
  }

}
