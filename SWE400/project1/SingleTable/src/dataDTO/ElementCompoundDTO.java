package dataDTO;

import java.util.List;

/**
 * Holds an object from ElementCompound
 * 
 * @author andrewjanuszko
 *
 */
public final class ElementCompoundDTO {

  private final int id;
  private final List<ChemicalDTO> relations;

  /**
   * Creates an ElementCompoundDTO
   * 
   * @param id        the id.
   * @param relations the things it is related to.
   */
  public ElementCompoundDTO(int id, List<ChemicalDTO> relations) {
    this.id = id;
    this.relations = relations;
  }

  /**
   * Get the ID.
   * 
   * @return the ID.
   */
  public int getID() {
    return id;
  }

  /**
   * Get the things it is related to.
   * 
   * @return the things it is related to.
   */
  public List<ChemicalDTO> getRelations() {
    return relations;
  }

}
