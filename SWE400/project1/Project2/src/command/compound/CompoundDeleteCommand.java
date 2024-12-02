package command.compound;

import command.DeleteCommandInterface;
import model.Compound;
import model.CompoundDataMapper;
import model.DomainModelException;

/**
 * Command for deleting Compound objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class CompoundDeleteCommand implements DeleteCommandInterface {

  private Compound compound;

  /**
   * Constructor for CompoundDeleteCommand(Compound).
   * 
   * @param compound, the Compound to delete.
   */
  public CompoundDeleteCommand(Compound compound) {
    this.compound = compound;
  }

  /**
   * @see command.DeleteCommandInterface#execute().
   */
  @Override
  public void execute() throws DomainModelException {
    new CompoundDataMapper().delete(compound);
  }

}
