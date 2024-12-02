package FileSystemApp;

/**
 * This type of menu option causes the machine to move to a new state.
 * @author merlin
 *
 */
public class MenuOptionForMenu extends MenuOption
{

  public MenuOptionForMenu(String menuOptionText, Class<? extends State> nextStateClass)
  {
      super.menuOptionText = menuOptionText;
      super.nextStateClass = nextStateClass;
  }

}
