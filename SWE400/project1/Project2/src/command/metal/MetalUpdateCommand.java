package command.metal;

import command.UpdateCommandInterface;
import model.DomainModelException;
import model.Metal;
import model.MetalDataMapper;

/**
 * Command for updating Metal objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class MetalUpdateCommand implements UpdateCommandInterface {

  private Metal metal;

  /**
   * Constructor for MetalUpdateCommand(Metal).
   * 
   * @param metal, the Metal to update.
   */
  public MetalUpdateCommand(Metal metal) {
    this.metal = metal;
  }

  /**
   * @see command.UpdateCommandInterface#execute().
   */
  @Override
  public void execute() throws DomainModelException {
    new MetalDataMapper().update(metal);
  }
}
