package FileSystemApp;

import java.io.File;
import java.io.FileNotFoundException;

import org.omg.CORBA.ORB;
import datasource.FileDoesNotExist;
import datasource.FileMap;

/**
 * A server to server implementation for the server.
 * 
 * @author andrewjanuszko
 *
 */
public class ServerToServerImplementation extends ServerToServerPOA {

  private ORB orb;

  /**
   * Constructor for ServerToServerImplementation.
   * 
   * @param orb, the orb for the ServerToServerImplementation.
   */
  public ServerToServerImplementation(ORB orb) {
    setORB(orb);
  }

  /**
   * Sets the orb for the ServerToServerImplementation.
   * 
   * @param orb, the orb for the ServerToServerImplementation.
   */
  public void setORB(ORB orb) {
    this.orb = orb;
  }

  /**
   * Returns the orb for the ServerToServerImplementation.
   * 
   * @return the orb for the ServerToServerImplementation.
   */
  public ORB getORB() {
    return orb;
  }

  /**
   * Lock a file and write to it
   * @param fileName String file name
   * @return String contents
   */
  @Override
  public String lockAndWrite(String fileName) {
    String contents;
    
    String versionNumber; 
    try {
      fileName = FileMap.getNameLike(fileName);
    } catch (FileNotFoundException e1) {
      e1.printStackTrace();
    }
    String[] info = fileName.split("-");
    versionNumber = info[1].split("\\.")[0] + "-";
    
    FileMap.lock(fileName);
    try {
      contents = FileMap.get(fileName).readFile();
      File f = new File(FileMap.get(fileName).getFullFileTitle());
      f.delete();
    
      FileMap.remove(FileMap.get(fileName));
      String result = versionNumber.concat(contents);
      return result;
    } catch (FileDoesNotExist e) {
      return null;
    } 
  }

  /**
   * Check if the file exists
   * @param file title
   * @return boolean whether or not the file exists
   */
  @Override
  public boolean fileExists(String title) {
    try {
      return FileMap.contains(FileMap.getNameLike(title));
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      return false; 
    }
  }

  /**
   * @see ServerToServerImplementation#shutdown().
   */
  @Override
  public void shutdown() {
    getORB().shutdown(false);
  }

  /**
   * Fetch a file remotely.  (this is the same as read) 
   * @param fileName String fileName to read
   * @return String contents
   */
  @Override
  public String fetchRemoteFile(String fileName) {
    try {
      fileName = FileMap.getNameLike(fileName);
    } catch (FileNotFoundException e1) {
      e1.printStackTrace();
    }
    
    if (FileMap.contains(fileName)) {
      try {
        System.out.println(FileMap.get(fileName).getFullFileTitle());
        return FileMap.get(fileName).readFile();
      } catch (FileDoesNotExist e) {
        System.out.println("File " + fileName + " does not exist. (ServerToServer.fetchRemoteFile()" );
      }
    }
    return "";
  }
 
  /**
   * Read a file on a remote server
   * @param String name of file to read
   * @return String contents of file
   */
  @Override
  public String readRemoteFile(String fileName) {
    if (FileMap.contains(fileName)) {
      try {
        return FileMap.get(fileName).readFile();
      } catch (FileDoesNotExist e) {
        System.out.println("File " + fileName + " does not exist. (ServerToServer.fetchRemoteFile()" );
      }
    }
    return "";
  }
  
  @Override
  public String sayOnline() {
    return "'Hey I am online!'";
  }

  @Override
  public void pingServers() {
    for (ServerToServer server : FileSystemServer.getImplentations()) {
      System.out.println(server.toString() + " says " + server.sayOnline());
    }
  }

  @Override
  public void killAllORB() {
    for (ORB orb : FileSystemServer.getORBs()) {
      orb.destroy();
    }
    FileSystemServer.setORBs(null);
  }

  /**
   * Check status of lock for a file
   * @param fileName String name of file
   * @return boolean status of lock on file
   */
  @Override
  public boolean checkLock(String fileName) {
    try { 
      return FileMap.isLocked(FileMap.getNameLike(fileName));
    } catch (FileNotFoundException e) {
      return false;
    }
  }
  
  

}
