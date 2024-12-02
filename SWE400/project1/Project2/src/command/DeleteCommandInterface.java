package command;

import model.DomainModelException;

/**
 * Command Interface for deleting Chemical objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public interface DeleteCommandInterface {

  /**
   * Executes command, deletes a Chemical object.
   * 
   * @throws DomainModelException when things go wrong.
   */
  public void execute() throws DomainModelException;

}
