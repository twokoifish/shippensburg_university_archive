package dataENUM;

/**
 * An enum to hold all the chemical types.
 * @author andrewjanuszko
 */
public enum ChemicalEnum {

	ELEMENT (01),
	METAL (02),
	COMPOUND (03),
	BASE (04),
	ACID (05);
	
	private final int chemicalType;
	
	/**
	 * Constructor for type.
	 * @param chemicalType the chemical.
	 */
	ChemicalEnum(int chemicalType) {
		this.chemicalType = chemicalType;
	}
	
	public static ChemicalEnum getChemicalType(int type) {
	  for(ChemicalEnum chemicalEnum : ChemicalEnum.values()) {
	    if (chemicalEnum.chemicalType == type) {
	      return chemicalEnum;
	    }
	  }
    return null; 
	}
	
	/**
	 * Get the type of the chemical.
	 * @return the integer value of the chemical.
	 */
	public int getIntValue() {
		return this.chemicalType;
	}
	
}
