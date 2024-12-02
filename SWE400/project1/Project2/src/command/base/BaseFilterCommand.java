package command.base;

import java.util.List;

import command.FilterCommandInterface;
import model.BaseDataMapper;
import model.Base;
import model.DomainModelException;

/**
 * Command for filtering Base objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class BaseFilterCommand implements FilterCommandInterface {

  String[] filter;

  /**
   * Constructor for BaseFilterCommand(String).
   * 
   * @param filter, the filter to apply.
   */
  public BaseFilterCommand(String filter) {
    this.filter = filter.split("-");
  }

  /**
   * @see command.FilterCommandInterface#execute().
   */
  @Override
  public List<Base> execute() throws DomainModelException {
    switch (Integer.parseInt(filter[0])) {
    case 1:
      return new BaseDataMapper().filterByNameLike(filter[1]);
    case 2:
      return new BaseDataMapper().filterByInventory(Double.parseDouble(filter[1]));
    case 3:
      return new BaseDataMapper().filterByInventoryBetween(Double.parseDouble(filter[1]), Double.parseDouble(filter[2]));
    case 4:
      return new BaseDataMapper().filterBySolute(Integer.parseInt(filter[1]));
    case 5:
      return new BaseDataMapper().filterByLowInventory();
    case 6:
      return new BaseDataMapper().getAll();
    default:
      throw new DomainModelException("Failed to execute filter for Base.");
    }
  }

}
