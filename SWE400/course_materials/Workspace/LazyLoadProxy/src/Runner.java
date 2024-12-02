
public class Runner
{

	public static void main(String[] args)
	{
		BigObject x =(BigObject) LazyLoadProxy.newInstance(ConcreteBigObject.class, new RandomDataGenerator(32));
		BigObject y =(BigObject) LazyLoadProxy.newInstance(ConcreteBigObject.class, new RandomDataGenerator(42));
		System.out.println(x.numberTagIsEven());
		
		System.out.println();
		
		System.out.println(y);
		y.getIthElement(42);
		System.out.println(y);
	}

}
