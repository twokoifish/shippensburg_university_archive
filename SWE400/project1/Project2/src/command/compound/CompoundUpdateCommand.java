package command.compound;

import command.UpdateCommandInterface;
import model.Compound;
import model.CompoundDataMapper;
import model.DomainModelException;

/**
 * Command for updating Compound objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class CompoundUpdateCommand implements UpdateCommandInterface {

  private Compound compound;

  /**
   * Constructor for CompoundUpdateCommand(Compound).
   * 
   * @param compound, the Compound to be updated.
   */
  public CompoundUpdateCommand(Compound compound) {
    this.compound = compound;
  }

  /**
   * @see command.UpdateCommandInterface#execute().
   */
  @Override
  public void execute() throws DomainModelException {
    new CompoundDataMapper().update(compound);
  }

}
