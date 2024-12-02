package command.acid;

import command.UpdateCommandInterface;
import model.Acid;
import model.AcidDataMapper;
import model.DomainModelException;

/**
 * Command for updating Acid objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class AcidUpdateCommand implements UpdateCommandInterface {

  private Acid acid;

  /**
   * Constructor for AcidUpdateCommand(Acid).
   * 
   * @param acid, the Acid to update.
   */
  public AcidUpdateCommand(Acid acid) {
    this.acid = acid;
  }

  /**
   * @see command.UpdateCommandInterface#execute().
   */
  @Override
  public void execute() throws DomainModelException {
    new AcidDataMapper().update(acid);
  }
}
