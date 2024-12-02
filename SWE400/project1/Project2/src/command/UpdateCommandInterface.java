package command;

import model.DomainModelException;

/**
 * Command Interface for updating Chemical objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public interface UpdateCommandInterface {

  /**
   * Executes command, updates a Chemical object.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public void execute() throws DomainModelException;

}
