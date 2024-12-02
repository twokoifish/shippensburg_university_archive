package reports;

import java.util.List;

import model.Metal;

public class FilterMetalReport implements Report {
List<Metal> metals;
  
  public FilterMetalReport(List<Metal> metals) {
    this.metals = metals;
  }
  
  public List<Metal> getFilterMetalsReport() {
    return metals;
  }
}
