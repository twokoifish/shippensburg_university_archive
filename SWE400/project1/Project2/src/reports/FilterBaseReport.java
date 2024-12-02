package reports;

import java.util.List;

import model.Base;

public class FilterBaseReport implements Report {
  
  List<Base> bases;
  
  public FilterBaseReport(List<Base> bases) {
    this.bases = bases;
  }
  
  public List<Base> getFilterBasesReport() {
    return bases;
  }
}
