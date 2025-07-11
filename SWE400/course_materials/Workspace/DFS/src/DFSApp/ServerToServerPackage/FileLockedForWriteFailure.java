package DFSApp.ServerToServerPackage;


/**
* DFSApp/ServerToServerPackage/FileLockedForWriteFailure.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from DFS.idl
* Thursday, November 29, 2018 11:00:20 AM EST
*/

public final class FileLockedForWriteFailure extends org.omg.CORBA.UserException
{
  public String reason = null;

  public FileLockedForWriteFailure ()
  {
    super(FileLockedForWriteFailureHelper.id());
  } // ctor

  public FileLockedForWriteFailure (String _reason)
  {
    super(FileLockedForWriteFailureHelper.id());
    reason = _reason;
  } // ctor


  public FileLockedForWriteFailure (String $reason, String _reason)
  {
    super(FileLockedForWriteFailureHelper.id() + "  " + $reason);
    reason = _reason;
  } // ctor

} // class FileLockedForWriteFailure
