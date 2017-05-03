import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

public class Window extends Application {
  /**
   * Creates a window, visual display of the game.
   */
   @Override
  public void start(Stage primaryStage){

    Button startButton = new Button("START GAME");
    Button exitButton  = new Button("Exit");

    StackPane root = new StackPane();
    root.getChildren().add(startButton);
    root.getChildren().add(exitButton);

    Scene scene = new Scene(root, 300, 250);

    primaryStage.setTitle("Tic Tac Toe");
    primaryStage.setScene(scene);
    primaryStage.show();

    // startButton.addActionListener(new
    //   ActionListener()
    //   {
    //     public void actionPerformed(ActionEvent event){
    //       tictactoe.startGame(board);
    //     }
    //   });

    // exitButton.addActionListener(new ActionListener(){
    //   public void actionPerformed(ActionEvent event){
    //     System.exit(0);
    //   }
    // });

    // frame.add(buttonPanel);
  }

}
