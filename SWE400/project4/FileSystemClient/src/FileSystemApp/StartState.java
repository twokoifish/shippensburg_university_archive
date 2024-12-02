package FileSystemApp;

/**
 * This is the state that the machine starts in.  In other words, it is the highest level menu
 * @author merlin
 *
 */
public class StartState extends State
{

	private final MenuOption[] menuOptions = {
	  new MenuOptionForAction("Read from file", new ReadAction(), StartState.class),
	  new MenuOptionForAction("Write to file", new WriteAction(), StartState.class),
	  new MenuOptionForMenu("Exit", EndState.class)
	};

	/**
	 * 
	 */
	public StartState()
	{
		super.loadMenu(menuOptions);
	}

}
