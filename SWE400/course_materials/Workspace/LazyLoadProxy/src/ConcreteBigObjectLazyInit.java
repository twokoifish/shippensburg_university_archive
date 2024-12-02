import java.math.BigInteger;

public class ConcreteBigObjectLazyInit extends ConcreteBigObject
{

	private RandomDataGenerator loader;

	public ConcreteBigObjectLazyInit(int id, RandomDataGenerator loader)
	{
		super(id);
		this.loader = loader;
	}
	
	public int getID()
	{
		return id;
	}

	private BigInteger getTag()
	{
		if (tag == null)
		{
			tag = loader.getRandomTag();
		}
		return tag;
	}
	
	private BigInteger[] getData()
	{
		if (dataSet == null)
		{
			dataSet = loader.getRandomData();
		}
		return dataSet;
	}


	public BigInteger getIthElement(int i)
	{
		return getData()[i];
	}


	public boolean numberTagIsEven()
	{
		getTag();
		return super.numberTagIsEven();
	}
}
