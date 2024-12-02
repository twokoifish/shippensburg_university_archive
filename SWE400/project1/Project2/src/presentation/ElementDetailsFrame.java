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

import command.element.ElementUpdateCommand;
import model.DomainModelException;
import model.Element;
import model.Element;


public class ElementDetailsFrame extends JFrame{
	Element element;
	GridBagConstraints gbc = new GridBagConstraints();
	JButton updateButton = new JButton("Update");
	JTextField jtfName;
	JTextField jtfInventory;
	JTextField jtfAtomicNumber;
	JTextField jtfAtomicMass;
	
	public ElementDetailsFrame(Element a) {
		element = a;
		setLayout(new GridBagLayout());
    	setBackground(Color.BLACK);
    	setUp();
    	setSize(300, 450);
    	setResizable(true);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		setVisible(true);
	}
	
	public void setUp() {
		jtfName = new JTextField(element.getName());
		jtfInventory = new JTextField("" + element.getInventory());
		jtfAtomicNumber = new JTextField("" + element.getAtomicNumber());
		jtfAtomicMass = new JTextField("" + element.getAtomicMass());
		
		
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
		
		updateButton.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				element.setName(jtfName.getText());
				element.setInventory(Double.parseDouble(jtfInventory.getText()));
				element.setAtomicNumber(Integer.parseInt(jtfAtomicNumber.getText()));
				element.setAtomicMass(Double.parseDouble(jtfAtomicMass.getText()));
				
				try {
					new ElementUpdateCommand(element).execute();
				} catch (DomainModelException e1) {
					
				}
				dispose();
			}
		
		});
		
		gbc.gridx = 0;
		gbc.gridy = 4;
		add(updateButton,gbc);
	}
}
