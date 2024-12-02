package datasource;

/**
 * Interface for a ChemicalRowDataGateway
 * 
 * @author andrewjanuszko
 */
public interface ChemicalRowDataGatewayInterface {

  /**
   * Deletes an instance of a Chemical from the table.
   * 
   * @throws DatabaseException when things go wrong.
   */
  public void delete() throws DatabaseException;

  /**
   * Updates the Chemical in the table.
   * 
   * @throws DatabaseException when things go wrong.
   */
  public void update() throws DatabaseException;

  /**
   * Get the ID of the Chemical.
   * 
   * @return the Chemical ID.
   */
  public int getID();

  /**
   * Get the type of the Chemical.
   * 
   * @return the type.
   */
  public int getType();

  /**
   * Get the name of the Chemical.
   * 
   * @return the name.
   */
  public String getName();

  /**
   * Get the habitat of the Chemical.
   * 
   * @return the habitat.
   */
  public double getInventory();

  /**
   * Get the atomic number of the Chemical.
   * 
   * @return the atomic number.
   */
  public int getAtomicNumber();

  /**
   * Get the atomic mass of the Chemical.
   * 
   * @return the atomic mass.
   */
  public double getAtomicMass();

  /**
   * Get the Acid that a Metal is dissolved by.
   * 
   * @return the ID of the Acid.
   */
  public int getDissolvedBy();

  /**
   * Get the amount of acid moles required to dissolve a metal.
   * 
   * @return the amount of acid moles required to dissolve a metal.
   */
  public double getAcidAmount();

  /**
   * Get the Chemical ID of the solute.
   * 
   * @return the Chemical ID.
   */
  public int getSolute();

  /**
   * Set the type of the Chemical.
   * 
   * @param type the type of the Chemical.
   */
  public void setType(int type);

  /**
   * Set the name of the Chemical.
   * 
   * @param name the name of the Chemical.
   */
  public void setName(String name);

  /**
   * Set the habitat of the chemical.
   * 
   * @param inhabits is the habitat of the chemical.
   */
  public void setInventory(double inventory);

  /**
   * Set the atomic number of the Chemical.
   * 
   * @param atomicNumber the atomic number of the Chemical.
   */
  public void setAtomicNumber(int atomicNumber);

  /**
   * Set the atomic mass of the Chemical.
   * 
   * @param atomicMass the atomic mass of the Chemical.
   */
  public void setAtomicMass(double atomicMass);

  /**
   * Set the Acid ID that a Metal is dissolved by.
   * 
   * @param dissolvedBy is the Acid ID that a Metal is dissolved by.
   */
  public void setDissolvedBy(int dissolvedBy);

  /**
   * Set the amount of acid moles required to dissolve a metal.
   * 
   * @param moles the amount of acid moles required to dissolve a metal.
   */
  public void setAcidAmount(double acidAmount);

  /**
   * Set the Chemical ID of the solute.
   * 
   * @param solute the Chemical ID of the solute.
   */
  public void setSolute(int solute);

}