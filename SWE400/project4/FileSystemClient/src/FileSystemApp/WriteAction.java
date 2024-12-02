package FileSystemApp;

import java.util.Scanner; 

public class WriteAction extends MenuAction {
  private String fileName;
  private String message;

  @Override
  public void execute() {
    System.out.println("\nWrite State\n");
    System.out.print("File name: ");
    Scanner keyboard = new Scanner(System.in); 
    this.fileName = keyboard.nextLine();
    System.out.print("New file contents: ");
    this.message = keyboard.nextLine();
    if (!FileSystemClient.writeFile(fileName, message)) {
      System.out.println("An error occured.");
    } else {
      System.out.println("Everything went as planned.");
    }
  } 
  
}
