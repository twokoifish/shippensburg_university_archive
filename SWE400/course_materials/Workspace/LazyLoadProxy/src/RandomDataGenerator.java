import java.math.BigInteger;
import java.util.Random;

public class RandomDataGenerator implements DataLoader
{

	Random rand = new Random();
	private int id;
	
	public RandomDataGenerator(int id)
	{
		this.id = id;
	}

	 BigInteger getRandomTag()
	{
		return new BigInteger(1000,rand);
	}

	 BigInteger[] getRandomData()
	{
		Random rand = new Random();
		BigInteger[] data = new BigInteger[rand.nextInt(10000)];
		for (int i=0;i<data.length; i++)
		{
			data[i] = new BigInteger(1000,rand);
		}
		return data;
	}

	@Override
	public Object newObject()
	{
		System.out.println("Creating the new object");
		return new ConcreteBigObject(id, getRandomTag(), getRandomData());
	}

	@Override
	public int getID()
	{
		return id;
	}
}
