package reports;

import java.util.List;

import model.*;

public class FilterAcidReport implements Report {

  List<Acid> acids;

  public FilterAcidReport(List<Acid> acids) {
    this.acids = acids;
  }

  public List<Acid> getFilterAcidsReport() {
    return acids;
  }
}
