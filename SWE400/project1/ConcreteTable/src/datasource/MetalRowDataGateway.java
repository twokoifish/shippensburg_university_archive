package datasource;
/*
 * The Interface for MetalRowDataGateway
 */
public interface MetalRowDataGateway {

	public int getMetalID();
	
	public String getName();
	
	public double getInventory();
	
	public int getAtomicNumber();
	
	public double getAtomicMass();
	
	public double getAcidAmount();
	
	public int getDissolvedBy();
	
	public void setName(String s);
	
	public void setInventory(double i);
	
	public void setAtomicNumber(int i);
	
	public void setAtomicMass(double d);
	
	public void setAcidAmount(double d);
	
	public void setDissolvedBy(int i);
	
	public boolean persist();
	
	public boolean delete();
}
