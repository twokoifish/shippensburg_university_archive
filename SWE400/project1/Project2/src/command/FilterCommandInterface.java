package command;

import java.util.List;

import model.Chemical;
import model.DomainModelException;

/**
 * All commands implement the Command Class and implement the execute method
 * 
 * @author kim & isabella
 *
 */
public interface FilterCommandInterface {

  /**
   * Execute method for Commands.
   */
  public List<? extends Chemical> execute() throws DomainModelException;
}
