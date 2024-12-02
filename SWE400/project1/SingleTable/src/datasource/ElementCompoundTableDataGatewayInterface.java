package datasource;

import dataDTO.ElementCompoundDTO;

/**
 * A table data gateway.
 * @author andrewjanuszko
 */
public interface ElementCompoundTableDataGatewayInterface {
  
  /**
   * Creates the table in the database.
   * @throws DatabaseException when things go wrong.
   */
  public void createTable() throws DatabaseException;
  
  /**
   * Drops the table in the database.
   * @throws DatabaseException when things go wrong.
   */
  public void dropTable() throws DatabaseException;
  
  /**
   * Create a row.
   * @param compoundID
   * @param elementID
   * @throws DatabaseException
   */
  public void create(int compoundID, int elementID) throws DatabaseException;
  
  /**
   * Updates a row in a table.
   * @param compoundID the id of the compound.
   * @param elementID the id of the element.
   * @throws DatabaseException when we cannot connect to the database.
   */
  public void update(int oldCompoundID, int oldElementID, int newCompoundID, int newElementID) throws DatabaseException;
  
  /**
   * Delete a row from the TDG.
   * @param compoundID
   * @param elementID
   * @throws DatabaseException
   */
  public void delete(int compoundID, int elementID) throws DatabaseException;
  
  /**
   * Gets all elements in a compound.
   * @param compoundID the compound.
   * @return all elements in a compound.
   * @throws DatabaseException when things go wrong.
   */
  public ElementCompoundDTO readElementsFromCompound(int compoundID) throws DatabaseException;
  
  /**
   * Get all compounds that contain an element.
   * @param elementID the element id.
   * @return all compounds that contain an element.
   * @throws DatabaseException when things go wrong.
   */
  public ElementCompoundDTO readCompoundsWithElement(int elementID) throws DatabaseException;
}
