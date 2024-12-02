package command.acid;

import command.DeleteCommandInterface;
import model.Acid;
import model.AcidDataMapper;
import model.DomainModelException;

/**
 * Command for deleting Acid objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class AcidDeleteCommand implements DeleteCommandInterface {

  private Acid acid;

  /**
   * Constructor for AcidDeleteCommand(Acid).
   * 
   * @param acid, the Acid to delete.
   */
  public AcidDeleteCommand(Acid acid) {
    this.acid = acid;
  }

  /**
   * @see command.DeleteCommandInterface#execute().
   */
  @Override
  public void execute() throws DomainModelException {
    new AcidDataMapper().delete(acid);
  }

}
