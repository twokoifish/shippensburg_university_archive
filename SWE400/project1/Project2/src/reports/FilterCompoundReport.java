package reports;

import java.util.List;

import model.Compound;

public class FilterCompoundReport implements Report {
  
  List<Compound> compounds;
  
  public FilterCompoundReport(List<Compound> compounds) {
    this.compounds = compounds;
  }
  
  public List<Compound> getFilterCompoundsReport() {
    return compounds;
  }
}
