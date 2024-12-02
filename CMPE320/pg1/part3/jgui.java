import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.concurrent.*;

public class jgui extends JFrame {

    public static void main(String args[]) {
        jgui g = new jgui();
        g.setPreferredSize(new Dimension(300, 300));
        g.setMinimumSize(new Dimension(200, 200));
        g.setVisible(true);
        ;

        ExecutorService pool =Executors.newSingleThreadExecutor();

        JButton button = new JButton("Click me.");
        button.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                pool.submit(new Runnable() {
                    public void run( ) {
                        try {
                            Thread.sleep(10000);
                            System.out.println("Done.");
                        } catch (InterruptedException e1) {
                            // TODO Auto-generated catch block
                            e1.printStackTrace();
                        }    
                    }
                });
            }
        });
        g.add(button);
    }

}
