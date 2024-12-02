package command.metal;

import java.util.List;

import command.FilterCommandInterface;
import model.MetalDataMapper;
import model.DomainModelException;
import model.Metal;

/**
 * Command for filtering Metal objects.
 * 
 * @author andrewjanuszko, isabella boone, & kimberly o'neill
 */
public class MetalFilterCommand implements FilterCommandInterface {

  String[] filter;

  /**
   * Constructor for FilterMetalCommand(String).
   * 
   * @param filter, the filter to apply.
   */
  public MetalFilterCommand(String filter) {
    this.filter = filter.split("-");
  }

  /**
   * @see command.FilterCommandInterface#execute().
   */
  @Override
  public List<Metal> execute() throws DomainModelException {
    switch (Integer.parseInt(filter[0])) {
    case 1:
      return new MetalDataMapper().filterByNameLike(filter[1]);
    case 2:
      return new MetalDataMapper().filterByInventory(Double.parseDouble(filter[1]));
    case 3:
      return new MetalDataMapper().filterByInventoryBetween(Double.parseDouble(filter[1]),
          Double.parseDouble(filter[2]));
    case 4:
      return new MetalDataMapper().filterByAtomicNumber(Integer.parseInt(filter[1]));
    case 5:
      return new MetalDataMapper().filterByAtomicMass(Double.parseDouble(filter[1]));
    case 6:
      return new MetalDataMapper().filterByAtomicMassBetween(Double.parseDouble(filter[1]),
          Double.parseDouble(filter[2]));
    case 7:
      return new MetalDataMapper().filterByAcidAmount(Double.parseDouble(filter[1]));
    case 8:
      return new MetalDataMapper().filterByAcidAmountBetween(Double.parseDouble(filter[1]),
          Double.parseDouble(filter[2]));
    case 9:
      return new MetalDataMapper().filterByDissolvedBy(Integer.parseInt(filter[1]));
    case 10:
      return new MetalDataMapper().filterByPartOfCompound(Integer.parseInt(filter[1]));
    case 11:
      return new MetalDataMapper().filterByLowInventory();
    case 12:
      return new MetalDataMapper().getAll();
    default:
      throw new DomainModelException("Failed to execute filter for Metal.");
    }
  }

}