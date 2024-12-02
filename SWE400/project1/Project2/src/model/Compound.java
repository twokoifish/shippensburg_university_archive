package model;

import java.util.List;

/**
 * Class for creating a Compound.
 * 
 * @author andrewjanuszko & morgan williams-burrell
 */
public class Compound extends Chemical {

  private List<Element> madeOf;

  /**
   * Constructor for creating a Compound.
   * 
   * @param id        the ID of the Compound.
   * @param name      the name of the Compound.
   * @param inventory the inventory of the Compound.
   * @param madeOf    the Elements that make up the Compound.
   */
  public Compound(int id, String name, double inventory, List<Element> madeOf) {
    super(id, name, inventory);
    setMadeOf(madeOf);
  }

  /**
   * Set the Elements that make up the Compound.
   * 
   * @param madeOf the Elements that make up the Compound.
   * @throws DomainModelException 
   */
  public void setMadeOf(List<Element> madeOf) {
    this.madeOf = madeOf;
  }

  /**
   * Get the Elements that make up the Compound.
   * 
   * @return the Elements that make up the Compound.
   */
  public List<Element> getMadeOf() {
    return madeOf;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((madeOf == null) ? 0 : madeOf.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    Compound other = (Compound) obj;
    if (madeOf == null) {
      if (other.madeOf != null)
        return false;
    } else if (!madeOf.equals(other.madeOf))
      return false;
    return true;
  }
}
