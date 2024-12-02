package presentation;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;

public class FailureFrame extends JFrame{
	
	JButton closeButton = new JButton("Close");
	JLabel text;
	
	public FailureFrame(String message) {
		setLayout(new BorderLayout());
	    setBackground(Color.BLACK);
	    setText(message);
	    setButton();
	    setResizable(true);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		pack();
		setVisible(true);
	}
	
	public void setText(String message) {
		text = new JLabel(message);
		add(text, BorderLayout.CENTER);
	}
	
	public void setButton() {
		closeButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});
		
		add(closeButton, BorderLayout.SOUTH);
	}
	
}
