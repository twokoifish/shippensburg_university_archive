package DFSApp.ClientToServerPackage;

/**
* DFSApp/ClientToServerPackage/FileNotFoundFailureHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from DFS.idl
* Thursday, November 29, 2018 11:00:20 AM EST
*/

public final class FileNotFoundFailureHolder implements org.omg.CORBA.portable.Streamable
{
  public DFSApp.ClientToServerPackage.FileNotFoundFailure value = null;

  public FileNotFoundFailureHolder ()
  {
  }

  public FileNotFoundFailureHolder (DFSApp.ClientToServerPackage.FileNotFoundFailure initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = DFSApp.ClientToServerPackage.FileNotFoundFailureHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    DFSApp.ClientToServerPackage.FileNotFoundFailureHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return DFSApp.ClientToServerPackage.FileNotFoundFailureHelper.type ();
  }

}
