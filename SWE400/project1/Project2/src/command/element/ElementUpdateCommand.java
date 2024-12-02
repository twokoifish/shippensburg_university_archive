package command.element;

import command.UpdateCommandInterface;
import model.DomainModelException;
import model.Element;
import model.ElementDataMapper;

/**
 * Command for updating Element objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class ElementUpdateCommand implements UpdateCommandInterface {
  private Element element;

  /**
   * Constructor for ElementUpdateCommand(Element).
   * 
   * @param element, the Element to update.
   */
  public ElementUpdateCommand(Element element) {
    this.element = element;
  }

  /**
   * @see command.UpdateCommandInterface#execute().
   */
  @Override
  public void execute() throws DomainModelException {
    new ElementDataMapper().update(element);
  }

}
