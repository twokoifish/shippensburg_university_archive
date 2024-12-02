package datadto;

public class MetalDTO extends ElementDTO {
  int id, dissolvedBy, atomicNumber; 
  String name;
  double inventory, atomicMass, acidAmount;
  
  public MetalDTO(int id, String name, double inventory, int atomicNumber, double atomicMass, double acidAmount, int dissolvedBy) {
    super(id, name, inventory, atomicNumber, atomicMass);
  }
  public double getAcidAmount() {
    return acidAmount;
  }

  public void setAcidAmount(double acidAmount) {
    this.acidAmount = acidAmount;
  }

  public int getDissolvedById() {
    return dissolvedBy; 
  }
 
  public void setDissolvedById(int dissolvedBy) {
    this.dissolvedBy = dissolvedBy;
  }
}
