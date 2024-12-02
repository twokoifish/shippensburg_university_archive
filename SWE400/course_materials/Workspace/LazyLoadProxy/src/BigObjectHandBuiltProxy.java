import java.math.BigInteger;

public class BigObjectHandBuiltProxy implements BigObject
{

	private ConcreteBigObject obj;
	public BigObjectHandBuiltProxy(int id)
	{
		super();
		this.id = id;
	}

	private int id;
	
	@Override
	public int getID()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void setIthElement(BigInteger x)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public BigInteger getIthElement(int i)
	{
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	public boolean numberTagIsEven()
	{
		if (obj == null)
		{
			RandomDataGenerator gen = new RandomDataGenerator(id);
			obj = (ConcreteBigObject) gen.newObject();
		}
		return obj.numberTagIsEven();
	}

}
