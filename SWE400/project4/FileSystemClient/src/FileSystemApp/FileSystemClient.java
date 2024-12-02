package FileSystemApp;

import org.omg.CosNaming.*;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.CORBA.*;
import org.omg.CORBA.ORBPackage.InvalidName;
import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * A client that can read and write to a file.
 * 
 * @author andrewjanuszko
 */
public class FileSystemClient {

  private static String host;
  private static String initialPort;
  private static String port;
  private static ClientToServer clientToServerImplementation;

  /**
   * Main method for the FileSystemClient.
   * 
   * @param args, ignored for now.
   */
  public static void main(String args[]) {
    setHost("localhost");
    setInitialPort("1046");
    setPort("1047");
    setUpLocalConnection(getHost(), getInitialPort(), getPort());
    Scanner keyboard = new Scanner(System.in);
    State currentState = new StartState();
    while (currentState.getClass() != EndState.class) {
      currentState.printOptions();
      try {
        int option = keyboard.nextInt();
        currentState = currentState.processOption(option);
      } catch (InputMismatchException e) {
        System.out.println("Please enter the number of the option you'd like to select");
      }
      keyboard.nextLine();
    }
    keyboard.close();
    System.out.println("Whoosp");
    getImplementation().shutdown();
  }

  /**
   * Returns the current implementation.
   * 
   * @return the current implementation.
   */
  public static ClientToServer getImplementation() {
    return clientToServerImplementation;
  }

  /**
   * Set the host of the orb.
   * 
   * @param host, the host to look for.
   */
  private static void setHost(String host) {
    FileSystemClient.host = host;
  }

  /**
   * Set the initial port of the orb.
   * 
   * @param initialPort, the initial port to look for.
   */
  private static void setInitialPort(String initialPort) {
    FileSystemClient.initialPort = initialPort;
  }

  /**
   * Set the port of the orb.
   * 
   * @param port, the port to look for.
   */
  private static void setPort(String port) {
    FileSystemClient.port = port;
  }

  /**
   * Returns the host.
   * 
   * @return the host.
   */
  private static String getHost() {
    return host;
  }

  /**
   * Returns the initial port.
   * 
   * @return the initial port.
   */
  private static String getInitialPort() {
    return initialPort;
  }

  /**
   * Returns the secondary port.
   * 
   * @return the secondary port.
   */
  private static String getPort() {
    return port;
  }

  /**
   * Calls the readFile(String) on FileSystemServer.
   * 
   * @param fileName, the name of the file.
   * @return the contents of the file.
   */
  public static String readFile(String fileName) {
    System.out.println("Getting contents of file '" + fileName + "'.");
    return clientToServerImplementation.readLocalFile(fileName);
  }

  /**
   * Calls the writeFile(String, String) on FileSystemServer.
   * 
   * @param fileName, the name of the file.
   * @param message,  the message to be appended.
   * @return if the write was successful or not.
   */
  public static boolean writeFile(String fileName, String message) {
    System.out.println("Appending '" + message + "' to '" + fileName + "'.");
    return clientToServerImplementation.writeLocalFile(fileName, message);
  }

  /**
   * Set up connection for client to local server.
   * 
   * @param host,        the host server.
   * @param initialPort, the initial port.
   * @param port,        the secondary port.
   */
  public static void setUpLocalConnection(String host, String initialPort, String port) {
    String[] initialization = { "-ORBInitialHost", host, "-ORBInitialPort", initialPort, "-port", port };
    ORB remote = ORB.init(initialization, null);
    try {
      org.omg.CORBA.Object objectReferenceRemote = remote.resolve_initial_references("NameService");
      NamingContextExt ncRefRemote = NamingContextExtHelper.narrow(objectReferenceRemote);
      clientToServerImplementation = ClientToServerHelper
          .narrow(ncRefRemote.resolve_str("ClientToServer"));
    } catch (InvalidName | NotFound | CannotProceed | org.omg.CosNaming.NamingContextPackage.InvalidName e) {
      System.out.println("Failed to set up a local connection.");
      e.printStackTrace(System.out);
      System.exit(-1);
    }
  }

}
