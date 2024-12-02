package datasource;

public interface CompoundRowDataGateway {
	
	public int getCompoundID();
	
	public String getName();
	
	public double getInventory();
	
	public void setName(String n);
	
	public void setInventory(double i);
	
	public boolean persist();
	
	public boolean delete();
}
