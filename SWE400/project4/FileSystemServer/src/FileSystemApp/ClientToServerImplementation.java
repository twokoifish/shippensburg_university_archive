package FileSystemApp;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.omg.CORBA.ORB;
import datasource.FileDoesNotExist;
import datasource.FileMap;

/**
 * A client to server implementation for the server.
 * 
 * @author andrewjanuszko
 *
 */
public class ClientToServerImplementation extends ClientToServerPOA {

  private ORB orb;

  /**
   * Constructor for ClientToServerImplementation.
   * 
   * @param orb, the orb for the ClientToServerImplementation.
   */
  public ClientToServerImplementation(ORB orb) {
    setORB(orb);
  }

  /**
   * Set the orb for the ClientToServerImplementation.
   * 
   * @param orb, the orb for the ClientToServerImplementation.
   */
  public void setORB(ORB orb) {
    this.orb = orb;
  }

  /**
   * Returns the current instance of the orb.
   * 
   * @return the current instance of the orb.
   */
  public ORB getORB() {
    return orb;
  }

  /**
   * @see ClientToServerImplementationPOA#readFile(String).
   */
  @Override
  public String readLocalFile(String fileName) {
    if(isLocal(fileName)) {
      try {
        fileName = FileMap.getNameLike(fileName);
      } catch (FileNotFoundException e1) {
        e1.printStackTrace();
        System.out.println("Could not find file with name similar to " + fileName);
      }
    }
    
    // Check for file locally 
    if (FileMap.contains(fileName)) {
      try {
        return FileMap.readFile(fileName);
      } catch (FileDoesNotExist e) {
        return fileName + " does not exist.";
      } 
    } else {
      // If not found locally, check all other servers
      FileSystemServer.setUpOutgoingConnections();
      for (ServerToServer stsi  : FileSystemServer.getImplentations()) {
        if (stsi.fileExists(fileName)) {
          return stsi.fetchRemoteFile(fileName);
        }
      }
      return "The file does not exist.";
    }
  }
  
  private boolean isLocal(String fileName) {
    File folder = new File("./data/");
    
    if (!folder.isDirectory()) {
      folder = new File("./../data/");
    }
   
    File[] listOfFiles = folder.listFiles();

    for (File f : listOfFiles) {
      // If the file is a text file
      if (f.isFile() && f.getName().contains(".txt") && f.getName().contains(fileName)) {
        return true; 
      } 
    }
    return false;
  }
  
  /**
   * @see ClientToServerImplementationPOA#writeFile(String, String).
   */
  @Override
  public boolean writeLocalFile(String fileName, String message) {
    if(isLocal(fileName)) {
      try {
        fileName = FileMap.getNameLike(fileName);
      } catch (FileNotFoundException e1) {
        e1.printStackTrace();
        System.out.println("Could not find file with name similar to " + fileName);
      }
    }
    
    FileMap.printMap();
    System.out.print("\nStarting write... " + fileName + "  -> ");
    
    // If local
    if(FileMap.contains(fileName)) {
      if(!FileMap.isLocked(fileName)) {
        // If local and not locked
        FileMap.lock(fileName); // Lock to start write
        
        try {
          FileMap.writeTo(fileName, message);
          System.out.println("Wrote '" + message + "' to " + fileName);
          FileMap.unlock(FileMap.getNameLike(fileName.split("-")[0]));
          return true; 
        } catch (IOException | FileDoesNotExist e) {
          System.out.println("Write/unlock failed, file does not exist --- ");
          e.printStackTrace();
          return false;
        }
      } else {
        System.out.println("Cannot write to '" + fileName + ", file is locked");
        return false;
      }
    } else {
      // not local
      FileSystemServer.setUpOutgoingConnections(); // Set up connections to servers
      for (ServerToServer stsi  : FileSystemServer.getImplentations()) {
        // If file exists on server and is not locked
        if (stsi.fileExists(fileName) && (!stsi.checkLock(fileName))) {
          String contents = stsi.lockAndWrite(fileName);
          String[] info = contents.split("-");
          int versionNumber = Integer.parseInt(info[0]);
          fileName = fileName.concat("-" + versionNumber + ".txt");
            
          FileMap.createAndWrite(fileName, (contents + message));
          
          return true; 
        } else {
          System.out.println("File is does not exist or is locked");
        }
      }
      // If we got here, we could not write
      return false; 
    }
  }

  /**
   * @see ClientToServerImplementationPOA#shutdown().
   */
  @Override
  public void shutdown() {
    getORB().shutdown(false);
  }
  
}
