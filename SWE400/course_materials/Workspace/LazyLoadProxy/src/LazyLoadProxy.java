import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class LazyLoadProxy implements java.lang.reflect.InvocationHandler
{

	private Object obj;
	private DataLoader dataGenerator;
	// preloaded Method objects for the methods in java.lang.Object
	private static Method hashCodeMethod;
	private static Method equalsMethod;
	private static Method toStringMethod;

	static
	{
		try
		{
			hashCodeMethod = Object.class.getMethod("hashCode");
			equalsMethod = Object.class.getMethod("equals", new Class[]
			{ Object.class });
			toStringMethod = Object.class.getMethod("toString");
		} catch (NoSuchMethodException e)
		{
			throw new NoSuchMethodError(e.getMessage());
		}
	}

	public static Object newInstance(Class<?> targetClass, DataLoader dataLoader)
	{
		return java.lang.reflect.Proxy.newProxyInstance(targetClass.getClassLoader(), targetClass.getInterfaces(),
				new LazyLoadProxy(dataLoader));
	}

	private LazyLoadProxy(DataLoader factory)
	{
		this.dataGenerator = factory;
	}

	public Object invoke(Object proxy, Method m, Object[] args) throws Throwable
	{
		Object result;

		Class<?> declaringClass = m.getDeclaringClass();

		if (declaringClass == Object.class)
		{
			if (m.equals(hashCodeMethod))
			{
				return proxyHashCode();
			} else if (m.equals(equalsMethod))
			{
				return proxyEquals(args[0]);
			} else if (m.equals(toStringMethod))
			{
				return proxyToString();
			} else
			{
				throw new InternalError("unexpected Object method dispatched: " + m);
			}
		} else
		{
			try
			{
				System.out.println("before method " + m.getName() + " on object with id " + dataGenerator.getID());
				if (obj == null)
				{
					obj = dataGenerator.newObject();
				}
				result = m.invoke(obj, args);
			} catch (InvocationTargetException e)
			{
				throw e.getTargetException();
			} catch (Exception e)
			{
				throw new RuntimeException("unexpected invocation exception: " + e.getMessage());
			} finally
			{
				System.out.println("after method " + m.getName());
			}
			return result;
		}
	}

	protected Integer proxyHashCode()
	{
		return System.identityHashCode(this);
	}

	protected Boolean proxyEquals( Object other)
	{
		return (this == other ? Boolean.TRUE : Boolean.FALSE);
	}

	protected String proxyToString()
	{
		if (obj == null)
		{
			return this.getClass().getName() + '@' + Integer.toHexString(this.hashCode());
		} else
		{
			return obj.toString();
		}
	}
}