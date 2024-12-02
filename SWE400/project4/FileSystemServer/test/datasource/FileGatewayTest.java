package datasource;

import static org.junit.Assert.*;

import org.junit.Test;

public class FileGatewayTest {

  @Test
  public void testReadFile() throws FileDoesNotExist {
    FileGateway file = new FileGateway("test.txt",1, false);
    String result = file.readFile();
    assertEquals("test\n", result);
  }

}
