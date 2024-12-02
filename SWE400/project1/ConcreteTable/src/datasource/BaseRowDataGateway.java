package datasource;

public interface BaseRowDataGateway {
	
	public int getBaseID();
	
	public String getName();
	
	public double getInventory();
	
	public int getSolute();
	
	public String getSoluteType();
	
	public void setName(String name);
	
	public void setInventory(double inventory);
	
	public void setSolute(int solute);
	
	public void setSoluteType(String soluteType);
	
	public boolean persist();
	
	public boolean delete();
}
