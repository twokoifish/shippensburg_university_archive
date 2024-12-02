import org.omg.CORBA.ORB;

import DFSApp.ClientToServerPOA;
import DFSApp.ClientToServerPackage.FileNotFoundFailure;

/**
 * Implement the operations that can come from the local client
 * 
 * @author merlin
 *
 */
public class ClientToServerImpl extends ClientToServerPOA
{
	private ORB orb;

	/**
	 * @param orb
	 *            the ORB object that clients can connect to
	 */
	public ClientToServerImpl(ORB orb)
	{
		this.orb = orb;
	}

	/**
	 * The client wants to open a file for read. Use the FileManager to get the
	 * handle of it
	 * @throws FileNotFoundFailure 
	 * 
	 * @see DFSApp.ClientToServerOperations#openFileForRead(java.lang.String)
	 */
	@Override
	public int openFileForRead(String fileTitle) throws FileNotFoundFailure
	{
		int fileID;
		try
		{
			fileID = FileManager.getSingleton().openForRead(fileTitle);
			return fileID;
		} catch (DFSFileDoesntExist e)
		{
			throw new DFSApp.ClientToServerPackage.FileNotFoundFailure("Not here");
		}

	}

	/**
	 * The client wants data from a file that is open for read
	 * 
	 * @see DFSApp.ClientToServerOperations#readFromFile(int, int, int)
	 */
	@Override
	public String readFromFile(int fileID, int location, int length)
	{
		return FileManager.getSingleton().readFrom(fileID, location, length);
	}

	/**
	 * Just use this to test that the server answers
	 * 
	 * @see DFSApp.ClientToServerOperations#sayHello()
	 */
	@Override
	public String sayHello()
	{
		return "\nHello world !!\n";
	}

	/**
	 * The client doesn't want to talk to us any more
	 * 
	 * @see DFSApp.ClientToServerOperations#shutdown()
	 */
	@Override
	public void shutdown()
	{
		orb.shutdown(false);
	}

	@Override
	public void closeFile(int fileID)
	{
		FileManager.getSingleton().closeFile(fileID);
	}
}
