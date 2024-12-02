package FileSystemApp;

public enum MenuStateEnum
{
	START_MENU(new StartState()),
	END_STATE(new EndState());
	
	private State state;

	MenuStateEnum(State s)
	{
		this.state = s;
	}
	
	public State getState()
	{
		return state;
	}
}
