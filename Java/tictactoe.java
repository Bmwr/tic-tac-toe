import java.util.Scanner;
import java.io.IOException;
import javafx.application.Application;

public class tictactoe{

  static void startGame(String[][] Board){
    String player1 = " X ";
    String player2 = " O ";
    boolean win       = false;
    boolean fullBoard = false;
    int round         = 1;
    int row           = -1;
    int column        = -1;

    while( win == false && fullBoard == false ){
      printBoard(Board);
      Scanner scan = new Scanner(System.in);

      if ( round == 1 || round % 2 != 0 ){
        System.out.println("Player 1's turn.\n");
        System.out.print("Enter the row.\n");
        row = scan.nextInt();
        System.out.println("Enter the column.");
        column = scan.nextInt();
        Board = setCoordinate(row, column, Board, player1, scan);
      }
      else if ( round % 2 == 0 ){
        System.out.println("Player 2's turn.\n");
        System.out.print("Enter the row.\n");
        row = scan.nextInt();
        System.out.println("Enter the column.\n");
        column = scan.nextInt();
        Board = setCoordinate(row, column, Board, player2, scan);
      }
      round += 1;
      win = checkWin(Board);
      fullBoard = checkCat(Board);
    }
    EndGame(Board,win, fullBoard, round-1);
  }

  private static void EndGame(String[][] board, boolean win, boolean fullBoard, int round){
    printBoard(board);
    if ( fullBoard == true && win == false ){
      System.out.println("Cat");
      System.exit(0);
    }
    else{
      if ( win == true && ( round % 2 != 0 ) ){
        System.out.println("Player 1 Wins!");
        System.exit(0);
      }
      else if ( win == true && ( round % 2 == 0 ) ){
        System.out.println("Player 2 Wins!");
        System.exit(0);
      }
    }
  }

  private static boolean checkCat(String[][] board){
    for (int i = 0; i < board.length; i++){
      for( int j = 0; j < board.length; j++){
        if (board[i][j].equals(" ")){ return false; }
      }
    }
    return true;
  }

  private static boolean checkWin(String[][] board){
    if ( rowWin(board) || colWin(board) || diagWin(board) ){ return true; }
    return false;
  }

  private static boolean rowWin(String[][] board){
    for ( int i = 0; i < board.length; i++){
      if( (board[i][0].equals(board[i][1]) && board[i][0].equals(board[i][2]))
           && ( !board[i][0].equals(" ") )){
         return true; }
    }
    return false;
  }

  private static boolean colWin(String[][] board){
    for ( int j = 0; j < board.length; j++){
      if ( ( board[0][j].equals(board[1][j] ) && board[0][j].equals(board[2][j]) )
           && ( !board[0][j].equals(" ") ) ){
        return true; }
    }
    return false;
  }

  private static boolean diagWin(String[][] board){
    if (((board[0][0].equals(board[1][1]) && board[0][0].equals(board[2][2])) ||
         (board[0][2].equals(board[1][1]) && board[0][2].equals(board[2][0])) ) &&
         (!board[1][1].equals(" ")) ){
           return true;
         }
    return false;
  }

  private static String[][] setCoordinate(int row, int column, String[][] board, String piece, Scanner scan){
    if ( !board[row][column].equals(" ") ){
      System.out.println("Error. Spot already taken.");
      System.out.print("Enter the row.\n");
      row = scan.nextInt();
      System.out.println("Enter the column.\n");
      column = scan.nextInt();
      setCoordinate(row, column, board, piece, scan);
    }
    else{
      board[row][column] = piece;
    }
    return board;
  }

  private static String[][] initBoard(){
    String[][] board = new String[3][3];

    for (int i = 0; i < board.length; i++){
      for (int j = 0; j < board.length; j++){
        board[i][j] = " ";
      }
    }
    return board;
  }

  private static void printBoard(String[][] Board){
    System.out.println("  | 0 | 1 | 2 |");
    for (int i = 0; i < Board.length; i++){
      System.out.print(i + " | ");
      for (int j = 0; j < Board.length; j++){
        System.out.print(Board[i][j] + " | ");
      }
      System.out.println("\n");
    }
  }

  public static void main(String[] args){

    String[][] board = initBoard();
    // Window.start(board);
    Application.launch(Window.class, args);

  }
}
