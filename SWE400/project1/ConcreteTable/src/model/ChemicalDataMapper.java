package model;

import java.util.ArrayList;
import java.util.List;

public class ChemicalDataMapper implements ChemicalDataMapperInterface{

  @Override
  public Chemical read(int id) throws DomainModelException {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public List<Chemical> getAll() throws DomainModelException {
    List<Chemical> all = new ArrayList<Chemical>();
    AcidDataMapper a = new AcidDataMapper();
    BaseDataMapper b = new BaseDataMapper();
    CompoundDataMapper c = new CompoundDataMapper();
    ElementDataMapper e = new ElementDataMapper();
    
    all.addAll(a.getAll());
    all.addAll(b.getAll());
    all.addAll(c.getAll());
    all.addAll(e.getAll());
    
    return all;
  }

  @Override
  public List<Chemical> filterByNameLike(String nameLike) throws DomainModelException {
    List<Chemical> filter = new ArrayList<Chemical>();
    AcidDataMapper a = new AcidDataMapper();
    BaseDataMapper b = new BaseDataMapper();
    CompoundDataMapper c = new CompoundDataMapper();
    ElementDataMapper e = new ElementDataMapper();
    
    filter.addAll(a.filterByNameLike(nameLike));
    filter.addAll(b.filterByNameLike(nameLike));
    filter.addAll(c.filterByNameLike(nameLike));
    filter.addAll(e.filterByNameLike(nameLike));
    
    return filter;
  }

  @Override
  public List<Chemical> filterByInventory(double inventory) throws DomainModelException {
    List<Chemical> filter = new ArrayList<Chemical>();
    AcidDataMapper a = new AcidDataMapper();
    BaseDataMapper b = new BaseDataMapper();
    CompoundDataMapper c = new CompoundDataMapper();
    ElementDataMapper e = new ElementDataMapper();
    
    filter.addAll(a.filterByInventory(inventory));
    filter.addAll(b.filterByInventory(inventory));
    filter.addAll(c.filterByInventory(inventory));
    filter.addAll(e.filterByInventory(inventory));
    
    return filter;
  }

  @Override
  public List<Chemical> filterByInventoryBetween(double min, double max) throws DomainModelException {
    List<Chemical> filter = new ArrayList<Chemical>();
    AcidDataMapper a = new AcidDataMapper();
    BaseDataMapper b = new BaseDataMapper();
    CompoundDataMapper c = new CompoundDataMapper();
    ElementDataMapper e = new ElementDataMapper();
    
    filter.addAll(a.filterByInventoryBetween(min, max));
    filter.addAll(b.filterByInventoryBetween(min, max));
    filter.addAll(c.filterByInventoryBetween(min, max));
    filter.addAll(e.filterByInventoryBetween(min, max));
    
    return filter;
  }

  @Override
  public List<Chemical> filterByLowInventory() throws DomainModelException {
    List<Chemical> filter = new ArrayList<Chemical>();
    AcidDataMapper a = new AcidDataMapper();
    BaseDataMapper b = new BaseDataMapper();
    CompoundDataMapper c = new CompoundDataMapper();
    ElementDataMapper e = new ElementDataMapper();
    
    filter.addAll(a.filterByLowInventory());
    filter.addAll(b.filterByLowInventory());
    filter.addAll(c.filterByLowInventory());
    filter.addAll(e.filterByLowInventory());
    
    return filter;
  }

}
