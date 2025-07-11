package DFSApp;


/**
* DFSApp/ServerToServerHelper.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from DFS.idl
* Thursday, November 29, 2018 11:00:20 AM EST
*/

abstract public class ServerToServerHelper
{
  private static String  _id = "IDL:DFSApp/ServerToServer:1.0";

  public static void insert (org.omg.CORBA.Any a, DFSApp.ServerToServer that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream ();
    a.type (type ());
    write (out, that);
    a.read_value (out.create_input_stream (), type ());
  }

  public static DFSApp.ServerToServer extract (org.omg.CORBA.Any a)
  {
    return read (a.create_input_stream ());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;
  synchronized public static org.omg.CORBA.TypeCode type ()
  {
    if (__typeCode == null)
    {
      __typeCode = org.omg.CORBA.ORB.init ().create_interface_tc (DFSApp.ServerToServerHelper.id (), "ServerToServer");
    }
    return __typeCode;
  }

  public static String id ()
  {
    return _id;
  }

  public static DFSApp.ServerToServer read (org.omg.CORBA.portable.InputStream istream)
  {
    return narrow (istream.read_Object (_ServerToServerStub.class));
  }

  public static void write (org.omg.CORBA.portable.OutputStream ostream, DFSApp.ServerToServer value)
  {
    ostream.write_Object ((org.omg.CORBA.Object) value);
  }

  public static DFSApp.ServerToServer narrow (org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof DFSApp.ServerToServer)
      return (DFSApp.ServerToServer)obj;
    else if (!obj._is_a (id ()))
      throw new org.omg.CORBA.BAD_PARAM ();
    else
    {
      org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl)obj)._get_delegate ();
      DFSApp._ServerToServerStub stub = new DFSApp._ServerToServerStub ();
      stub._set_delegate(delegate);
      return stub;
    }
  }

  public static DFSApp.ServerToServer unchecked_narrow (org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof DFSApp.ServerToServer)
      return (DFSApp.ServerToServer)obj;
    else
    {
      org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl)obj)._get_delegate ();
      DFSApp._ServerToServerStub stub = new DFSApp._ServerToServerStub ();
      stub._set_delegate(delegate);
      return stub;
    }
  }

}
