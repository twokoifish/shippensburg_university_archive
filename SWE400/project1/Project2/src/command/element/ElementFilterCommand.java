package command.element;

import java.util.List;

import command.FilterCommandInterface;
import model.ElementDataMapper;
import model.Element;
import model.DomainModelException;

/**
 * Command for filtering Element objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 *
 */
public class ElementFilterCommand implements FilterCommandInterface {

  private String[] filter;

  /**
   * Constructor for FilterElementCommand(String).
   * 
   * @param filter, the filter to apply.
   */
  public ElementFilterCommand(String filter) {
    this.filter = filter.split("-");
  }

  /**
   * @see command.FilterCommandInterface#execute().
   */
  @Override
  public List<Element> execute() throws DomainModelException {
    switch (Integer.parseInt(filter[0])) {
    case 1:
      return new ElementDataMapper().filterByNameLike(filter[1]);
    case 2:
      return new ElementDataMapper().filterByInventory(Double.parseDouble(filter[1]));
    case 3:
      return new ElementDataMapper().filterByInventoryBetween(Double.parseDouble(filter[1]),
          Double.parseDouble(filter[2]));
    case 4:
      return new ElementDataMapper().filterByAtomicNumber(Integer.parseInt(filter[1]));
    case 5:
      return new ElementDataMapper().filterByAtomicMass(Double.parseDouble(filter[1]));
    case 6:
      return new ElementDataMapper().filterByAtomicMassBetween(Double.parseDouble(filter[1]),
          Double.parseDouble(filter[2]));
    case 7:
      return new ElementDataMapper().filterByPartOfCompound(Integer.parseInt(filter[1]));
    case 8:
      return new ElementDataMapper().filterByLowInventory();
    case 9:
      return new ElementDataMapper().getAll();
    default:
      throw new DomainModelException("Failed to execute filter for Element.");
    }
  }

}
