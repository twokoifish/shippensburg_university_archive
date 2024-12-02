package model;

import java.util.IdentityHashMap;

public class IdentityMap {
  
  private static IdentityHashMap<Integer, Chemical> identityMap = null;
  
  public static synchronized IdentityHashMap<Integer, Chemical> getSingletonInstance() {
    if (identityMap == null) {
      new IdentityMap();
    }
    return identityMap;
  }
  
  private IdentityMap() {
    identityMap = new IdentityHashMap<>();
  }
  
  public boolean addChemical(int id, Chemical chemical) {
    if (checkDuplicate(id, chemical)) {
      return false;
    } else {
      identityMap.put(id, chemical);
      return true;
    }
  }
  
  private boolean checkDuplicate(int id, Chemical chemical) {
    for (int key : identityMap.keySet()) {
      if (key == id) {
        return true;
      }
    }
    for (Chemical value : identityMap.values()) {
      if (value.getID() == chemical.getID() || value.getName() == chemical.getName()) {
        return true;
      }
    }
    return false;
  }
  
  public Chemical getChemical(int id) {
    return identityMap.get(id);
  }
  
  
  
  
}
