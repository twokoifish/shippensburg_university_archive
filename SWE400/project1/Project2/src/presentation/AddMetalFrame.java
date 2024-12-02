package presentation;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;

import command.metal.MetalCreateCommand;
import model.DomainModelException;

public class AddMetalFrame extends JFrame{
	GridBagConstraints gbc = new GridBagConstraints(); 
	int height = 450, width = 300;
	
	public AddMetalFrame() {
		setLayout(new GridBagLayout());
	    setBackground(Color.BLACK);
	    setUp();
	    setSize(width, height);
	    setResizable(true);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		setVisible(true);
	}
	
	public void setUp() {
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		/*
		gbc.gridx = 0;
		gbc.gridy = 0;
		JLabel idLabel = new JLabel("Id: ");
		add(idLabel,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 0;
		JTextField jtfId = new JTextField("Id");
		add(jtfId,gbc);
		*/
		gbc.gridx = 0;
		gbc.gridy = 0;
		JLabel nameLabel = new JLabel("Name: ");
		add(nameLabel,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 0;
		JTextField jtfName = new JTextField("Name");
		add(jtfName,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 1;
		JLabel inventoryLabel = new JLabel("Inventory: ");
		add(inventoryLabel,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 1;
		JTextField jtfInventory = new JTextField("Inventory");
		add(jtfInventory,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 2;
		JLabel atmNumLabel = new JLabel("Atomic Number: ");
		add(atmNumLabel,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 2;
		JTextField jtfAtmNum = new JTextField("Number");
		add(jtfAtmNum,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		JLabel atmMassLabel = new JLabel("Atomic Mass: ");
		add(atmMassLabel,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 3;
		JTextField jtfAtmMass = new JTextField("Mass");
		add(jtfAtmMass,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 4;
		JLabel dissolvedByLabel = new JLabel("Dissolved By: ");
		add(dissolvedByLabel,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 4;
		JTextField jtfDissolvedBy = new JTextField("Amount");
		add(jtfDissolvedBy,gbc);
		
		JButton add = new JButton("Add");
		add.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				
				double inventory, atmMass, dissolvedBy;
				int solute, atmNum;
				String name;
				List<Integer> ids = new ArrayList<Integer>();
				try {
					atmMass = Double.parseDouble(jtfAtmMass.getText());
					atmNum = Integer.parseInt(jtfAtmNum.getText());
					inventory = Double.parseDouble(jtfInventory.getText());
					name = jtfName.getText();
					dissolvedBy = Double.parseDouble(jtfDissolvedBy.getText());
					
					dispose();
					try {
						new MetalCreateCommand(name, inventory, atmNum, atmMass, dissolvedBy).execute();
					} catch (DomainModelException e1) {
						e1.printStackTrace();
					}
				} catch (NumberFormatException e1) {
					new FailureFrame("Failed to create Metal");
				}
						
			}
		});
		
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.gridwidth = 2;
		
		add(add, gbc);
	}
}
