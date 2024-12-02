package command.element;

import command.DeleteCommandInterface;
import model.DomainModelException;
import model.Element;
import model.ElementDataMapper;

/**
 * Command for deleting Element objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class ElementDeleteCommand implements DeleteCommandInterface {

  private Element element;

  /**
   * Constructor for ElementDeleteCommand(Element).
   * 
   * @param element, the Element to delete.
   */
  public ElementDeleteCommand(Element element) {
    this.element = element;
  }

  /**
   * @see command.DeleteCommandInterface#execute().
   */
  @Override
  public void execute() throws DomainModelException {
    new ElementDataMapper().delete(element);
  }

}
