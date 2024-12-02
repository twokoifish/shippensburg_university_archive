package presentation;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

public class FilterChemicalFrame extends JFrame{
	
	GridBagConstraints gbc = new GridBagConstraints();
	int filterType = 4;
	String filter;
	JButton filterButton = new JButton("Filter");
	JButton clearButton = new JButton("Clear Filter");
	JTextField jtfName;
	JTextField jtfInventory;
	JRadioButton nameFilter = new JRadioButton();        //filterType 1
	JRadioButton inventoryFilter = new JRadioButton();   //filterType 2
	JRadioButton inventoryRangeFilter = new JRadioButton();
	JTextField jtfInventoryRange1;
	JTextField jtfInventoryRange2;
	  
	public FilterChemicalFrame() {
		setLayout(new GridBagLayout());
    	setBackground(Color.BLACK);
    	setUp();
    	setSize(300, 450);
    	setResizable(true);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		setVisible(true);
	}
	
	public void setUp() {
		jtfName = new JTextField();
		jtfInventory = new JTextField();
		jtfInventoryRange1 = new JTextField();
		jtfInventoryRange2 = new JTextField();
		nameFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 1;
		      }
		});
		
		inventoryFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 2;
		      }
		});
		inventoryRangeFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 3;
		      }
		});
		filterButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});
		
		clearButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				filterType = 4;
				dispose();
			}
		});
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		add(nameFilter, gbc);
		gbc.gridx = 0;
		gbc.gridy = 1;
		add(inventoryFilter, gbc);
		gbc.gridx = 0;
		gbc.gridy = 2;
		add(inventoryRangeFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 0;
		add(new JLabel("Name Filter"), gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 1;
		add(new JLabel("Inventory Filter"), gbc);
		gbc.gridx = 1;
		gbc.gridy = 2;
		add(new JLabel("Inventory Range Filter"), gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 2;
		add(jtfInventoryRange1, gbc);
		
		gbc.gridx = 3;
		gbc.gridy = 2;
		add(jtfInventoryRange2, gbc);
		
		gbc.gridwidth = 2;
		gbc.gridx = 2;
		gbc.gridy = 0;
		add(jtfName,gbc);
		gbc.gridx = 2;
		gbc.gridy = 1;
		add(jtfInventory,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		add(filterButton,gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 3;
		add(clearButton,gbc);
		
	}
	
	public String getFilter() {
		switch (filterType) {
			case 1:
				filter = "1" + "-" +  jtfName.getText();
				break;
			case 2:
				filter = "" + filterType + "-" + Double.parseDouble(jtfInventory.getText());
				break;
			case 3:
				filter = "3" + "-"+ Double.parseDouble(jtfInventoryRange1.getText()) + "-" + Double.parseDouble(jtfInventoryRange2.getText());
				break;
			case 4:
				filter = "4";
				break;
		}
		System.out.println(filter);
		return filter;
	}
}
