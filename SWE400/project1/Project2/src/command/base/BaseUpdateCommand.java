package command.base;

import command.UpdateCommandInterface;
import model.Base;
import model.BaseDataMapper;
import model.DomainModelException;

/**
 * Command for updating Acid objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class BaseUpdateCommand implements UpdateCommandInterface {

  private Base base;

  /**
   * Constructor for BaseUpdateCommand(Base).
   * 
   * @param base, the Base to update.
   */
  public BaseUpdateCommand(Base base) {
    this.base = base;
  }

  /**
   * @see command.UpdateCommandInterface#execute().
   */
  @Override
  public void execute() throws DomainModelException {
    new BaseDataMapper().update(base);
  }

}
