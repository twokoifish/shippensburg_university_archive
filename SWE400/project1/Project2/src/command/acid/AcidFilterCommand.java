package command.acid;

import java.util.List;

import command.FilterCommandInterface;
import model.AcidDataMapper;
import model.Acid;
import model.DomainModelException;

/**
 * Command for filtering Acid objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class AcidFilterCommand implements FilterCommandInterface {

  String[] filter;

  /**
   * Constructor for AcidFilterCommand(String).
   * 
   * @param filter, the filter to apply.
   */
  public AcidFilterCommand(String filter) {
    this.filter = filter.split("-");
  }

  /**
   * @see command.FilterCommandInterface#execute().
   */
  @Override
  public List<Acid> execute() throws DomainModelException {
    switch (Integer.parseInt(filter[0])) {
    case 6:
      return new AcidDataMapper().getAll();
    case 1:
      return new AcidDataMapper().filterByNameLike(filter[1]);
    case 2:
      return new AcidDataMapper().filterByInventory(Double.parseDouble(filter[1]));
    case 3:
      return new AcidDataMapper().filterByInventoryBetween(Double.parseDouble(filter[1]),
          Double.parseDouble(filter[2]));
    case 4:
      return new AcidDataMapper().filterBySolute(Integer.parseInt(filter[1]));
    case 5:
      return new AcidDataMapper().filterByLowInventory();
    default:
      throw new DomainModelException("Failed to execute filter for Acid.");
    }
  }

}
