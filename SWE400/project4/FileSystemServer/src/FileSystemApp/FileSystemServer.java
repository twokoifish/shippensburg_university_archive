package FileSystemApp;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.omg.CORBA.ORB;
import org.omg.CORBA.ORBPackage.InvalidName;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import datasource.FileMap;


/**
 * This is the class that runs on the server
 * 
 * @author merlin
 *
 */
public class FileSystemServer {
  static File configFile = new File("config");
  private static List<ORB> orbs;
  static List<ServerToServer> fileSystemList = new ArrayList<ServerToServer>();

  /**
   * @param args ignored
   */
  public static void main(String args[]) {
    try {
      String[] initialization = { "-ORBInitialPort", "1046", "-port", "1047" };
      ORB orb = ORB.init(initialization, null);

      POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
      rootpoa.the_POAManager().activate();
      
      
      /*
       * sets up client listener for this server.
       */
      ClientToServerImplementation ctsi = new ClientToServerImplementation(orb); 
      org.omg.CORBA.Object reference = rootpoa.servant_to_reference(ctsi);
      ClientToServer hreference = ClientToServerHelper.narrow(reference);
      org.omg.CORBA.Object objectReference = orb.resolve_initial_references("NameService");
      NamingContextExt namingcontextRef = NamingContextExtHelper.narrow(objectReference);
      NameComponent ctsipath[] = namingcontextRef.to_name("ClientToServer");
      namingcontextRef.rebind(ctsipath, hreference);
      System.out.println("ClientToServer listener is up and waiting...");

      /*
       * sets up the server listener for this server.
       */
      ServerToServerImplementation stsi = new ServerToServerImplementation(orb);
      org.omg.CORBA.Object stsiReference = rootpoa.servant_to_reference(stsi);
      ServerToServer stsiHRef = ServerToServerHelper.narrow(stsiReference);
      org.omg.CORBA.Object stsiobjectReference = orb.resolve_initial_references("NameService");
      NamingContextExt stsinamingcontextRef = NamingContextExtHelper.narrow(stsiobjectReference);
      NameComponent stsipath[] = stsinamingcontextRef.to_name("ServerToServer");
      namingcontextRef.rebind(stsipath, stsiHRef);
      System.out.println("ServerToServer listener is up and waiting...");

      FileMap.initializeMap();  
      
      orb.run();
    }

    catch (Exception e) {
      System.err.println("ERROR: " + e);
      e.printStackTrace(System.out);
    }

    System.out.println("FileSystemServer Exiting ...");

  }

  public static List<ServerToServer> getImplentations() {
    return fileSystemList;
  }

  public static List<ORB> getORBs() {
    return orbs;
  }

  public static void setORBs(List<ORB> orbs) {
    FileSystemServer.orbs = orbs;
  }
  /*
   * Sets up the connections to the other servers.  This allows the talking between servers.
   */
  public static void setUpOutgoingConnections() {
    try {
        Scanner scanner = new Scanner(new File("config"));
        while (scanner.hasNext()) {
             String host = scanner.nextLine();
             String[] initialization = { "-ORBInitialHost", host, "-ORBInitialPort", "1046", "-port", "1047" };
             ORB orb = ORB.init(initialization, null);
             org.omg.CORBA.Object objRefRemote = orb.resolve_initial_references("NameService");
             NamingContextExt ncRefRemote = NamingContextExtHelper.narrow(objRefRemote);
             ServerToServer fileSystemImplRemote = ServerToServerHelper
                 .narrow(ncRefRemote.resolve_str("ServerToServer"));
             fileSystemList.add(fileSystemImplRemote);
        }
        scanner.close();
    } catch (FileNotFoundException | InvalidName | NotFound | CannotProceed | org.omg.CosNaming.NamingContextPackage.InvalidName e) {
      System.out.println("Failed to set up a server to server connection.");
      e.printStackTrace(System.out);
      System.exit(-1);
    }   
  }

}
