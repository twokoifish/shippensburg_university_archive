package datasource;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * File map for all files being held locally on a server. 
 * 
 * @author Isabella Boone, Joel Gingrich 
 *
 */
public class FileMap {
  private static Map<String, FileGateway> map = new HashMap<String, FileGateway>();

  /**
   * Add a file to the map.
   * @param fileGateway FileGateway to add to the map
   */
  public static void add(FileGateway fileGateway) {
    map.put(fileGateway.getFileName(), fileGateway);
  }

  /**
   * Replace a FileGateway.
   * @param fileGateway FileGateway, replaces Gateway that shares file names. 
   */
  public static void replace(FileGateway fileGateway) {
    map.replace(fileGateway.getFileName(), fileGateway);
  }

  /**
   * Remove a FileGateway from the map.
   * @param fileGateway FileGateway to remove
   */
  public static void remove(FileGateway fileGateway) {
    map.remove(fileGateway.getFileName(), fileGateway);
  }

  /**
   * Get a FileGateway from the map using fileName as a key
   * @param fileName String of the FileGateway to get
   * @return FileGateway with fileName as its name 
   */
  public static FileGateway get(String fileName) {
    return map.get(fileName);
  }

  /**
   * Check if the map has a file inside it. 
   * @param fileName String name of the file to search for 
   * @return
   */
  public static boolean contains(String fileName) {
    if (map.containsKey(fileName) && map.get(fileName) != null) {
      return true;
    } else {
      return false;
    }
  }
  
  /**
   * Print all file names currently in the map. 
   */
  public static void printMap() {
    System.out.print("\nMap:  ");
    for(String m : map.keySet()) {
      System.out.print(m + ", ");
    }
  }
  
  /**
   * Read a file in the map
   * @param fileName
   * @return String of file contents
   * @throws FileDoesNotExist
   */
  public static String readFile(String fileName) throws FileDoesNotExist {
    try {
      return FileMap.get(fileName).readFile();
    } catch (FileDoesNotExist e) {
      System.out.println("FileMap.readFile(" + fileName + ")");
      throw new FileDoesNotExist(fileName);
    } 
  }
  
  /**
   * Write to a file given a file name and message
   * @param fileName String name of file
   * @param message String contents
   * @throws IOException when bad input
   * @throws FileDoesNotExist when bad file
   */
  public static void writeTo(String fileName, String message) throws IOException, FileDoesNotExist {
    try {
      // Get the old gateway and form the new file name. 
      FileGateway oldGateway = FileMap.get(fileName);
      String[] info = fileName.split("-"); 
      String newFileName = info[0] + "-" + (oldGateway.getVersion() + 1) + ".txt";
      
      // Create a file writer and append message to it
      FileWriter f = new FileWriter(oldGateway.getPath() + newFileName); 
      f.append(readFile(fileName));
      f.append(message);
      f.close(); 
      
      // Create the file so we can delete it
      File deleteOld = new File(FileMap.get(fileName).getFullFileTitle());
      
      // Remove file from system and map
      FileMap.remove(oldGateway);
      deleteOld.delete(); 
      
      // Handle the version numbers
      String[] tmp = info[1].split("\\.");
      int version = Integer.parseInt(tmp[0]) + 1;
      FileGateway newGateway = new FileGateway(newFileName, version, oldGateway.isLocked());
      
      // Add gateway to map
      FileMap.add(newGateway);
    } catch (IOException e) {
      throw new IOException(); 
    } catch (FileDoesNotExist e) {
      throw new FileDoesNotExist(fileName + " not found"); 
    }
  }
  
  /**
   * Creates a file locally and writes to it.
   * @param fileName Name.
   * @param message File contents.
   */
  public static void createAndWrite(String fileName, String message) {
    // Tear fileName apart to get the version number from it. 
    String[] info = fileName.split("-");
    String name = info[0]; // Name 
    int oldVersion = Integer.parseInt(info[1].split("\\.")[0]);

    try {
      // Create a new filewriter with the new version, write contents and close the writer
      FileWriter writer = new FileWriter("./data/" + name + "-" + (oldVersion + 1) + ".txt");
      info = message.split("-"); // We store version number as 1-message, so cut off the 1-
      writer.write(info[1]);
      writer.close();
      
    } catch (IOException e) {
      e.printStackTrace();
    } 
    
    // Create gateway and add it to map
    FileGateway newFile = new FileGateway(name + "-" + (oldVersion + 1) + ".txt", (oldVersion + 1), false);
    FileMap.add(newFile);
  }

  /**
   * Clear the map 
   */
  public static void clear() {
    map.clear();
  }
  
  /**
   * Initialize all local files for this server. 
   */ 
  public static void initializeMap() {
    FileMap.clear(); // Reset file map 
    System.out.println("Initializing all files....");
    File folder = new File("./data/");
    String path = "./data/";
    if (!folder.isDirectory()) {
      path = "./../data/";
      folder = new File("./../data/");
    }
    System.out.println("path : " + folder.getAbsolutePath());
    File[] listOfFiles = folder.listFiles();

    for (File f : listOfFiles) {
      // If the file is a text file
      if (f.isFile() && f.getName().contains(".txt")) {
        System.out.println("Adding " + f.getName() + " to map.");
        
        String[] info = f.getName().split("-");
        //String name = info[0];
        String[] tmp = info[1].split("\\.");
        int version = Integer.parseInt(tmp[0]);
        
        FileGateway fileGateway = new FileGateway(f.getName(), version, false);
        fileGateway.setLocation(path);
        FileMap.add(fileGateway);
      }
    }
    System.out.println("All files loaded.");
  }

  /**
   * Set the lock for the file
   * @param fileName String fileName
   */
  public static void lock(String fileName) {
    FileMap.get(fileName).setLocked(true);
  }
  
  /**
   * Return status of lock
   * @param fileName string to check lock of
   * @return boolean status of lock
   */
  public static boolean isLocked(String fileName) {
    return FileMap.get(fileName).isLocked();
  }

  public static void unlock(String fileName) {
    FileMap.get(fileName).setLocked(false);
  }

  /**
   * Get full file name when given a partial name. (Essentially finds file version
   * and extension) 
   * @param nameLike String similar name (ex, test will find test-1.txt) 
   * @return Full file name
   * @throws FileNotFoundException if file was not found
   */
  public static String getNameLike(String nameLike) throws FileNotFoundException {
    // Find file in directory 
    File folder = new File("./data/");
    if (!folder.isDirectory()) {
      folder = new File("./../data/");
    }

    // List of all files in the directory
    File[] listOfFiles = folder.listFiles();

    for (File f : listOfFiles) {
      if(f.getName().contains(nameLike) && f.isFile() && f.getName().contains(".txt")) {
        return f.getName();
      }
    }
    
    System.out.println("File not found with name similar to " + nameLike);
    throw new FileNotFoundException(nameLike);
  }

}
