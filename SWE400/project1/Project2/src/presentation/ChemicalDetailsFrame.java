package presentation;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JFrame;
import javax.swing.JTextField;

import model.Chemical;

public class ChemicalDetailsFrame extends JFrame{
	GridBagConstraints gbc = new GridBagConstraints();
	Chemical chemical = null;
	
	public ChemicalDetailsFrame(Chemical c) {
		chemical = c;
		setLayout(new GridBagLayout());
    	setBackground(Color.BLACK);
    	setUp();
    	setSize(300, 450);
    	setResizable(true);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		setVisible(true);
	}
	
	private void setUp() {
		JTextField name = new JTextField(chemical.getName());
		JTextField inventory = new JTextField("" + chemical.getInventory());
			
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		add(name, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 1;
		add(inventory,gbc);
	}
}
