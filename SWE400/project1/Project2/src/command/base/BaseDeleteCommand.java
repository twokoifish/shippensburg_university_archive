package command.base;

import command.DeleteCommandInterface;
import model.Base;
import model.BaseDataMapper;
import model.DomainModelException;

/**
 * Command for deleting Base objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class BaseDeleteCommand implements DeleteCommandInterface {

  public Base base;

  /**
   * Constructor for BaseDeleteCommand(Base).
   * 
   * @param base, the Base to delete.
   */
  public BaseDeleteCommand(Base base) {
    this.base = base;
  }

  /**
   * @see command.DeleteCommandInterface#execute().
   */
  @Override
  public void execute() throws DomainModelException {
    new BaseDataMapper().delete(base);
  }

}
