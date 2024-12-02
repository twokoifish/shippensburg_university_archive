package model;

import java.util.HashMap;
import java.util.Map;

import model.Chemical;

public class IdentityMap<T extends Chemical> {
  private Map<Integer, T> map = new HashMap<Integer, T>();

  public void add(T arg) {
    map.put(arg.getID(), arg);
  }

  public void replace(T arg) {
    map.replace(arg.getID(), arg);
  }

  public void remove(T arg) {
    map.remove(arg.getID());
  }

  public T get(int key) {
    return map.get(key);
  }
  
  public void clear() {
    map.clear();
  }
}
