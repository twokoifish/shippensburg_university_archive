package presentation;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;

import command.metal.MetalUpdateCommand;
import model.DomainModelException;
import model.Metal;
import model.Metal;


public class MetalDetailsFrame extends JFrame{
	Metal metal;
	GridBagConstraints gbc = new GridBagConstraints();
	JButton updateButton = new JButton("Update");
	JTextField jtfName;
	JTextField jtfInventory;
	JTextField jtfAtomicNumber;
	JTextField jtfAtomicMass;
	JTextField jtfDissolvedBy;
	
	public MetalDetailsFrame(Metal a) {
		metal = a;
		setLayout(new GridBagLayout());
    	setBackground(Color.BLACK);
    	setUp();
    	setSize(300, 450);
    	setResizable(true);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		setVisible(true);
	}
	
	public void setUp() {
		jtfName = new JTextField(metal.getName());
		jtfInventory = new JTextField("" + metal.getInventory());
		jtfAtomicNumber = new JTextField("" + metal.getAtomicNumber());
		jtfAtomicMass = new JTextField("" + metal.getAtomicMass());
		jtfDissolvedBy = new JTextField("" + metal.getAcidAmount());
		
		gbc.weightx = 1;
		gbc.weighty = 1;
		gbc.fill = GridBagConstraints.BOTH;
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		add(jtfName,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 1;
		add(jtfInventory, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 2;
		add(jtfAtomicNumber,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		add(jtfAtomicMass,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 4;
		add(jtfDissolvedBy,gbc);
		
		updateButton.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				metal.setName(jtfName.getText());
				metal.setInventory(Double.parseDouble(jtfInventory.getText()));
				metal.setAtomicNumber(Integer.parseInt(jtfAtomicNumber.getText()));
				metal.setAtomicMass(Double.parseDouble(jtfAtomicMass.getText()));
				metal.setAcidAmount(Double.parseDouble(jtfDissolvedBy.getText()));
				try {
					new MetalUpdateCommand(metal).execute();
				} catch (DomainModelException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				dispose();
			}
		
		});
		
		gbc.gridx = 0;
		gbc.gridy = 5;
		add(updateButton,gbc);
	}
}
