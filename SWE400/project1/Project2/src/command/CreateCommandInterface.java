package command;

import model.Chemical;
import model.DomainModelException;

/**
 * Command Interface for creating Chemical objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public interface CreateCommandInterface {

  /**
   * Executes command, creates a Chemical object.
   * 
   * @return a Chemical object.
   * @throws DomainModelException when things go wrong.
   */
  public Chemical execute() throws DomainModelException;

}
