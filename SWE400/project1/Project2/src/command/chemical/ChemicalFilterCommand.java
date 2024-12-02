package command.chemical;

import java.util.List;

import command.FilterCommandInterface;
import model.ChemicalDataMapper;
import model.Chemical;
import model.DomainModelException;

/**
 * Command for filtering Chemical objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class ChemicalFilterCommand implements FilterCommandInterface {

  String[] filter;

  /**
   * Constructor for ChemicalFilterCommand(String).
   * 
   * @param filter, the filter to apply.
   */
  public ChemicalFilterCommand(String filter) {
    this.filter = filter.split("-");
  }

  /**
   * @see command.FilterCommandInterface#execute().
   */
  @Override
  public List<? extends Chemical> execute() throws DomainModelException {
    switch (Integer.parseInt(filter[0])) {
    case 1:
      return new ChemicalDataMapper().filterByNameLike(filter[1]);
    case 2:
      return new ChemicalDataMapper().filterByInventory(Double.parseDouble(filter[1]));
    case 3:
      return new ChemicalDataMapper().filterByInventoryBetween(Double.parseDouble(filter[1]),
          Double.parseDouble(filter[2]));
    case 4:
      return new ChemicalDataMapper().getAll();
    default:
      throw new DomainModelException("Failed to execute filter for Chemical.");
    }
  }

}
