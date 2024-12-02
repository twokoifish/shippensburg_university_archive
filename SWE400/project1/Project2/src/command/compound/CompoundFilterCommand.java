package command.compound;

import java.util.List;

import command.FilterCommandInterface;
import model.CompoundDataMapper;
import model.Compound;
import model.DomainModelException;

/**
 * Command for filtering Compound objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class CompoundFilterCommand implements FilterCommandInterface {

  String[] filter;

  /**
   * Constructor for FilterCompoundCommand(String).
   * 
   * @param filter, the filter to apply.
   */
  public CompoundFilterCommand(String filter) {
    this.filter = filter.split("-");
  }

  /**
   * @see command.FilterCommandInterface#execute().
   */
  @Override
  public List<Compound> execute() throws DomainModelException {
    switch (Integer.parseInt(filter[0])) {
    case 1:
      return new CompoundDataMapper().filterByNameLike(filter[1]);
    case 2:
      return new CompoundDataMapper().filterByInventory(Double.parseDouble(filter[1]));
    case 3:
      return new CompoundDataMapper().filterByInventoryBetween(Double.parseDouble(filter[1]),
          Double.parseDouble(filter[2]));
    case 4:
      return new CompoundDataMapper().filterByMadeOf(Integer.parseInt(filter[1]));
    case 5:
      return new CompoundDataMapper().filterByLowInventory();
    case 6:
      return new CompoundDataMapper().getAll();
    default:
      throw new DomainModelException("Failed to execute filter for Compound.");
    }
  }

}
