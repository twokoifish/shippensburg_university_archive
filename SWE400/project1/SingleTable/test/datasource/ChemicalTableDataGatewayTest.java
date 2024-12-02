package datasource;

public class ChemicalTableDataGatewayTest extends ChemicalTableDataGatewayInterfaceTest {

	/**
	 * Gets a singleton.
	 */
	@Override
	public ChemicalTableDataGatewayInterface getGateway() {
		return ChemicalTableDataGateway.getSingletonInstance();
	}

}
