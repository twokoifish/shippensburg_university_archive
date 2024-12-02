package datasource;

public class FileDoesNotExist extends Exception {
  /**
   * 
   */
  private static final long serialVersionUID = 6587281373899356956L;
  private String fileName; 

  public FileDoesNotExist(String fileName) {
    this.fileName = fileName; 
  }
  
  public String toString() {
    return super.toString() + " - File '" + fileName + "' could not be found.";
  }
}
