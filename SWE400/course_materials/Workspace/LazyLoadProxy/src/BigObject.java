import java.math.BigInteger;

/**
 * A Big object can have a list of BigIntegers and a Tag that can be a REALLY
 * HUGE number. Each of these objects also has a unique ID
 * 
 * @author merlin
 *
 */
public interface BigObject
{

	int getID();

	void setIthElement(BigInteger x);

	BigInteger getIthElement(int i);

	boolean numberTagIsEven();
}
