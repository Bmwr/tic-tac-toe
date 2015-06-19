package tic-tac-toe;

public class Board
{
	private static final int defaultBoardSize = 3;

	public static int[][] gameBoard;

	public Board()
	{
		gameBoard = new int[defaultBoardSize][defaultBoardSize];
	}
}
