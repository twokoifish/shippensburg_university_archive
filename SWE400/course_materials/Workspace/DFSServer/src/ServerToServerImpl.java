import org.omg.CORBA.ORB;

import DFSApp.ServerToServerPOA;
import DFSApp.ServerToServerPackage.FileLockedForWriteFailure;
import DFSApp.ServerToServerPackage.FileNotFoundFailure;

/**
 * This is the code that handles requests from other servers
 * 
 * @author merlin
 *
 */
public class ServerToServerImpl extends ServerToServerPOA
{

	private ORB orb;

	/**
	 * @param orb
	 *            the ORB object that servers can connect to
	 */
	public ServerToServerImpl(ORB orb)
	{
		this.orb = orb;
	}

	/**
	 * @see DFSApp.ServerToServerOperations#lockFileForWrite(java.lang.String)
	 */
	@Override
	public void lockFileForWrite(String fileTitle)
	{
		// TODO Auto-generated method stub

	}

	/**
	 * If we have the file and it can be open, the text in that file is returned
	 * 
	 * @throws DFSApp.ServerToServerPackage.FileNotFoundFailure
	 *             if the file is not on this server
	 * @throws DFSApp.ServerToServerPackage.FileLockedForWriteFailure
	 *             if the file is on this server, but locked for write
	 * @see DFSApp.ServerToServerOperations#readFile(java.lang.String)
	 */
	@Override
	public String readFile(String fileTitle) throws DFSApp.ServerToServerPackage.FileNotFoundFailure,
			DFSApp.ServerToServerPackage.FileLockedForWriteFailure
	{
		// TODO this should look similar to how it looked in project 3
		return null;
	}

	/**
	 * @see DFSApp.ServerToServerOperations#readFileAndLockForWrite(java.lang.String)
	 */
	@Override
	public String readFileAndLockForWrite(String fileTitle) throws FileNotFoundFailure, FileLockedForWriteFailure
	{
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see DFSApp.ServerToServerOperations#shutdown()
	 */
	@Override
	public void shutdown()
	{
		// TODO this should look just like it looked in project 3

	}

}
