package datasource;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

/**
 * Holds a single file
 * 
 * @author Isabella Boone, Joel Gingrich 
 *
 */
public class FileGateway {
  public static String location = "./data/";
  private String fileName = null;
  private int version;
  private boolean isLocked = false;

  /**
   * Create a file gateway. 
   * 
   * @param fileName String, name of file (not including directory) 
   * @param version int (default starts at 1) 
   * @param isLocked boolean, whether or not the file is currently 
   *            being modified. (default is false) 
   */
  public FileGateway(String fileName, int version, boolean isLocked) {
    this.fileName = fileName;
    this.version = version;
    this.isLocked = isLocked;
  }
  
  /**
   * Set the file path
   * @param location path of the file
   */
  public void setLocation(String location) {
    FileGateway.location = location;
  }

  /**
   * Get the full file title, including directory. 
   * @return String of directory and file title together. 
   */
  public String getFullFileTitle() {
    return location + fileName; 
  }

  /**
   * Get the file name.
   * @return String fileName
   */
  public String getFileName() {
    return fileName;
  }

  /**
   * Get the file version.
   * @return int version
   */
  public int getVersion() {
    return version;
  }

  /**
   * Get status of isLocked
   * @return boolean whether or not file is locked
   */
  public boolean isLocked() {
    return isLocked;
  }

  /**
   * Set file version 
   * @param version int
   */
  public void setVersion(int version) {
    this.version = version;
  }

  /**
   * Set lock status for a file 
   * @param isLocked boolean
   */
  public void setLocked(boolean isLocked) {
    this.isLocked = isLocked;
  }

  /**
   * Read the file the gateway is currently holding. 
   * @return String of file contents 
   * @throws FileDoesNotExist if file could not be found
   */
  public String readFile() throws FileDoesNotExist {
    try {
      Scanner s = new Scanner(new File(getFullFileTitle()));
      StringBuffer contents = new StringBuffer("");
      while (s.hasNext()) {
        contents.append(s.nextLine() + "\n");
      }

      s.close();
      return contents.toString();
    } catch (FileNotFoundException e) {
      throw new FileDoesNotExist(fileName);
    }
  }
  
  /**
   * Get the path of the file
   * @return String path
   */
  public String getPath() {
    return location; 
  }

}
