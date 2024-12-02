package datadto;

public class CompoundMadeOfDTO {
	
	private int compoundID;
	private int elementID;
	private int metalID;
	
	public CompoundMadeOfDTO(int compoundID, int elementID, int metalID) {
		this.compoundID = compoundID;
		this.elementID = elementID;
		this.metalID = metalID;
	}
	
	public int getCompoundID() {
		return compoundID;
	}
	
	public int getElementID() {
		return elementID;
	}
	
	public int getMetalID() {
    return metalID;
  }
	
	public void setMetalID(int metID) {
    metalID = metID;
  }
	
	public void setCompoundID(int comID) {
		compoundID = comID;
	}
	
	public void setElementID(int eleID) {
		elementID = eleID;
	}
}
