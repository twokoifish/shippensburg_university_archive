package datasource;

import java.util.List;

/**
 * CompoundRDG
 * 
 * @author Isabella Boone, Kim O'Neill
 *
 */
public interface CompoundRDG {
  public CompoundDTO create(List<Integer> madeOf, String name, double inventory) throws Exception;

  public CompoundDTO read(int id) throws Exception;

  public CompoundDTO update(CompoundDTO compound);

  public void delete(int id);

  public CompoundDTO getCompound();
}
