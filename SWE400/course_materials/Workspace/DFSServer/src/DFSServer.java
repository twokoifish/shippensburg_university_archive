
import org.omg.CORBA.ORB;
import org.omg.CORBA.ORBPackage.InvalidName;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAPackage.ServantNotActive;
import org.omg.PortableServer.POAPackage.WrongPolicy;

import DFSApp.ClientToServer;
import DFSApp.ClientToServerHelper;
import DFSApp.ServerToServerHelper;

/**
 * This is the class that runs on the server
 * 
 * @author merlin
 *
 */
public class DFSServer
{

	/**
	 * @param args
	 *            ignored
	 */
	public static void main(String args[])
	{
		try
		{
			// create and initialize the ORB
			ORB orb = ORB.init(args, null);

			// get reference to rootpoa & activate the POAManager
			POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			rootpoa.the_POAManager().activate();

			// create servant that will listen to the local client and register it with the
			// ORB
			ClientToServerImpl clientToServerImpl = new ClientToServerImpl(orb);
			startUpListener(orb, rootpoa, clientToServerImpl, "ClientToServer",
					ClientToServerHelper.narrow(rootpoa.servant_to_reference(clientToServerImpl)));

			ServerToServerImpl serverToServerImpl = new ServerToServerImpl(orb);
			startUpListener(orb, rootpoa, serverToServerImpl, "ServerToServer",
					ServerToServerHelper.narrow(rootpoa.servant_to_reference(serverToServerImpl)));
			//TODO this is where we should set up the link to the other servers using code like DFSClient.setUpConnectionToLocalServer
			
			
			// wait for invocations from clients
			orb.run();
		}

		catch (Exception e)
		{
			System.err.println("ERROR: " + e);
			e.printStackTrace(System.out);
		}

		System.out.println("DFSServer Exiting ...");

	}

	private static void startUpListener(ORB orb, POA rootpoa, org.omg.PortableServer.Servant servantImpl,
			String serviceName, org.omg.CORBA.Object href) throws ServantNotActive, WrongPolicy, InvalidName,
			org.omg.CosNaming.NamingContextPackage.InvalidName, NotFound, CannotProceed
	{
		// get object reference from the servant
		// org.omg.CORBA.Object ref = ;
		// ClientToServer href = ;

		// get the root naming context
		// NameService invokes the name service
		org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
		// Use NamingContextExt which is part of the Interoperable
		// Naming Service (INS) specification.
		NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

		// bind the Object Reference in Naming
		NameComponent path[] = ncRef.to_name(serviceName);
		ncRef.rebind(path, href);

		System.out.println(serviceName + " ready and waiting ...");
	}
}
