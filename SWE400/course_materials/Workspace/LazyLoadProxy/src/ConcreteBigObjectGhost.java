import java.math.BigInteger;

public class ConcreteBigObjectGhost extends ConcreteBigObject
{

	private RandomDataGenerator loader;

	/**
	 * Non lazy load constructor
	 * @param id
	 * @param tag
	 * @param data
	 */
	public ConcreteBigObjectGhost(int id, BigInteger tag, BigInteger[] data)
	{
		super(id, tag, data);
	}

	public ConcreteBigObjectGhost(int id, RandomDataGenerator loader)
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
			getTheData();
		}
		return tag;
	}
	
	private BigInteger[] getData()
	{
		if (dataSet == null)
		{
			getTheData();
		}
		return dataSet;
	}

	private void getTheData()
	{
		tag = loader.getRandomTag();
		dataSet = loader.getRandomData();
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
