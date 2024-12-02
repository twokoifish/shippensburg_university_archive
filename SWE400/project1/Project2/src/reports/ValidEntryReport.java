package reports;

public class ValidEntryReport implements Report {

  boolean isValid;
  public ValidEntryReport(boolean flag) {
    this.isValid = flag;
  }
  public boolean isValid() {
    return isValid;
  }
  
}
