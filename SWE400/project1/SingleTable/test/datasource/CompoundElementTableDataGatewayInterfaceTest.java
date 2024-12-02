package datasource;

public class CompoundElementTableDataGatewayInterfaceTest extends CompoundElementTableDataGatewayTest {

	/**
	 * Gets a singleton.
	 */
	@Override
	protected ElementCompoundTableDataGateway getSingletonInstance() {
		return ElementCompoundTableDataGateway.getSingletonInstance();
	}

}
