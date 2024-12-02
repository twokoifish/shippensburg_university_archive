import java.math.BigInteger;

public class ConcreteBigObject implements BigObject
{

	 BigInteger[] dataSet;
	 BigInteger tag;
	int id;

	public ConcreteBigObject(int id, BigInteger tag, BigInteger[] data)
	{
		this.id = id;
		this.tag = tag;
		this.dataSet = data;
	}
	
	 ConcreteBigObject(int id)
	{
		this.id = id;
	}

	@Override
	public BigInteger getIthElement(int i)
	{
		return dataSet[i];
	}

	@Override
	public boolean numberTagIsEven()
	{
		System.out.println("Checking if the tag is even ");
		String tagString = tag.toString();
		int lastDigit = (int)(tagString.charAt(tagString.length()-1) - '0');
		return lastDigit % 2 == 0;
	}

	@Override
	public void setIthElement(BigInteger x)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public int getID()
	{
		return id;
	}

}
