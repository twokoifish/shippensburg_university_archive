package reports;

import java.util.List;

import model.Element;

public class FilterElementReport implements Report {
List<Element> elements;
  
  public FilterElementReport(List<Element> compounds) {
    this.elements = compounds;
  }
  
  public List<Element> getFilterElementsReport() {
    return elements;
  }
}
