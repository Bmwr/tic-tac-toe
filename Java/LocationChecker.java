package tic-tac-toe;

public class LocationChecker
{
	private static final int MinimumPoint = 0;

	private static final int MaximumPoint = 2;

	public static boolean checkLocation(final int point)
	{
		if (point < MinimumPoint || point > MaximumPoint)
		{ return false; }

		return true;
	}
}
