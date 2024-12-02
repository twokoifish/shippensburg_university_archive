package model;

public class ChemicalSoluteGhost extends Chemical{
  private String s;
  private Chemical c = null;
  public ChemicalSoluteGhost(int id, String soluteType) {
    super(id, null, -1);
    this.s = soluteType;
  }
  
  public String getName() {
      try {
        if (s.contains("Acid")) {
          AcidDataMapper m = new AcidDataMapper();
          c = m.read(this.getID());
          return c.getName();
        } else if (s.contains("Base")) {
          BaseDataMapper m = new BaseDataMapper();
          c = m.read(this.getID());
          return c.getName();
        } else if (s.contains("Compound")) {
          CompoundDataMapper m = new CompoundDataMapper();
          c = m.read(this.getID());
          return c.getName();
        } else if (s.contains("Element")) {
          ElementDataMapper m = new ElementDataMapper();
          c = m.read(this.getID());
          return c.getName();
        } else if (s.contains("Metal")) {
          BaseDataMapper m = new BaseDataMapper();
          c = m.read(this.getID());
          return c.getName();
        }
      }catch (DomainModelException e) {
        e.printStackTrace();
      }
    return null;
  }

  
  public double getInventory() {
    try {
      if (s.contains("Acid")) {
        AcidDataMapper m = new AcidDataMapper();
        c = m.read(this.getID());
        return c.getInventory();
      } else if (s.contains("Base")) {
        BaseDataMapper m = new BaseDataMapper();
        c = m.read(this.getID());
        return c.getInventory();
      } else if (s.contains("Compound")) {
        CompoundDataMapper m = new CompoundDataMapper();
        c = m.read(this.getID());
        return c.getInventory();
      } else if (s.contains("Element")) {
        ElementDataMapper m = new ElementDataMapper();
        c = m.read(this.getID());
        return c.getInventory();
      } else if (s.contains("Metal")) {
        BaseDataMapper m = new BaseDataMapper();
        c = m.read(this.getID());
        return c.getInventory();
      }
    }catch (DomainModelException e) {
      e.printStackTrace();
    }
    return -1;
  }
}
