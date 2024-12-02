package FileSystemApp;

import java.util.Scanner;

public class ReadAction extends MenuAction {
  
  private String fileName;
  
  @Override
  public void execute() {
    System.out.println("\n Read State \n");
    System.out.print("File name: ");
    Scanner keyboard = new Scanner(System.in);
    this.fileName = keyboard.nextLine();
    System.out.println(FileSystemClient.readFile(fileName));
  }
  
}
