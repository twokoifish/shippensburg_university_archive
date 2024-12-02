package command.metal;

import command.DeleteCommandInterface;
import model.DomainModelException;
import model.Metal;
import model.MetalDataMapper;

/**
 * Command for deleting Metal objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class MetalDeleteCommand implements DeleteCommandInterface {

  public Metal metal;

  /**
   * Constructor for MetalDeleteCommand(Metal).
   * 
   * @param metal, the Metal to delete.
   */
  public MetalDeleteCommand(Metal metal) {
    this.metal = metal;
  }

  /**
   * @see command.DeleteCommandInterface#execute().
   */
  @Override
  public void execute() throws DomainModelException {
    new MetalDataMapper().delete(metal);
  }

}
