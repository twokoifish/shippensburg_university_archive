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

public class FilterMetalFrame extends JFrame{

	int filterType;        
	JRadioButton nameFilter = new JRadioButton();        //filterType 1
	JRadioButton inventoryFilter = new JRadioButton();   //filterType 2
	JRadioButton inventoryRangeFilter = new JRadioButton();   //filterType 3
	JRadioButton numberFilter = new JRadioButton();   //filterType 4
	JRadioButton massFilter = new JRadioButton();   //filterType 5
	JRadioButton massBetweenFilter = new JRadioButton();
	JRadioButton partOfFilter = new JRadioButton();
	JRadioButton lowInvFilter = new JRadioButton();
	JRadioButton acidAmountFilter = new JRadioButton();
	JRadioButton acidAmountBetweenFilter = new JRadioButton();
	JRadioButton dissolvedByFilter = new JRadioButton();
	GridBagConstraints gbc = new GridBagConstraints();
	JButton filterButton = new JButton("Filter");
	JButton clearButton = new JButton("Clear Filter");
	JTextField jtfName;
	JTextField jtfSolute;
	JTextField jtfInventory;
	JTextField jtfAtomicNum;
	JTextField jtfAtomicMass;
	JTextField jtfInventoryRange1;
	JTextField jtfInventoryRange2;
	JTextField jtfMassRange1;
	JTextField jtfMassRange2;
	JTextField jtfPartOf;
	JTextField jtfAcidAmount;
	JTextField jtfAcidAmountRange1;
	JTextField jtfAcidAmountRange2;
	JTextField jtfDissolvedBy;
	
	public FilterMetalFrame() {
		setLayout(new GridBagLayout());
    	setBackground(Color.BLACK);
    	setUp();
    	setSize(300, 450);
    	setResizable(true);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		setVisible(true);
	}
	
	public void setUp() {
		gbc.weightx = 1;
		gbc.weighty = 1;
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.BOTH;

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
		numberFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 4;
		      }
		});
		massFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 5;
		      }
		});
		massBetweenFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 6;
		      }
		});
		
		massBetweenFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 7;
		      }
		});
		acidAmountBetweenFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 8;
		      }
		});
		massBetweenFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 9;
		      }
		});
		
		partOfFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 10;
		      }
		});
		lowInvFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 11;
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
				filterType = 12;
				dispose();
			}
		});
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		add(nameFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 0;
		JLabel nameLabel = new JLabel("Name Filter: ");
		add(nameLabel,gbc);

		gbc.gridx = 0;
		gbc.gridy = 1;
		add(inventoryFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 1;
		JLabel inventoryLabel = new JLabel("Inventory Filter: ");
		add(inventoryLabel,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 2;
		add(inventoryRangeFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 2;
		JLabel inventoryRangeLabel = new JLabel("Inventory Range Filter: ");
		add(inventoryRangeLabel,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		add(numberFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 3;
		JLabel numberLabel = new JLabel("Atomic Number Filter: ");
		add(numberLabel,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 4;
		add(massFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 4;
		JLabel massLabel = new JLabel("Mass Filter: ");
		add(massLabel,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 5;
		add(massBetweenFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 5;
		JLabel massBetweenLabel = new JLabel("Mass Between Filter: ");
		add(massBetweenLabel,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 6;
		add(partOfFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 6;
		JLabel partOfLabel = new JLabel("Part Of Filter: ");
		add(partOfLabel,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 7;
		add(acidAmountFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 7;
		JLabel acidAmountLabel = new JLabel("Acid Amount Filter: ");
		add(acidAmountLabel,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 8;
		add(acidAmountBetweenFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 8;
		JLabel acidAmountBetweenLabel = new JLabel("Acid Amount Between Filter: ");
		add(acidAmountBetweenLabel,gbc);
		
		
		gbc.gridx = 0;
		gbc.gridy = 9;
		add(dissolvedByFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 9;
		JLabel dissolvedByLabel = new JLabel("Dissovled By Filter: ");
		add(dissolvedByLabel,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 10;
		add(lowInvFilter, gbc);
		
		
		gbc.gridwidth = 2;
		
		gbc.gridx = 2;
		gbc.gridy = 0;
		jtfName = new JTextField("Name");
		add(jtfName,gbc);

		

		gbc.gridx = 2;
		gbc.gridy = 1;
		jtfInventory = new JTextField("Inventory");
		add(jtfInventory,gbc);
		
		gbc.gridwidth = 1;
		
		gbc.gridx = 2;
		gbc.gridy = 2;
		jtfInventoryRange1 = new JTextField("Range 1");
		add(jtfInventoryRange1,gbc);
		
		gbc.gridx = 3;
		gbc.gridy = 2;
		jtfInventoryRange2 = new JTextField("Range 2");
		add(jtfInventoryRange2,gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 5;
		jtfMassRange1 = new JTextField("Range 1");
		add(jtfMassRange1,gbc);
		
		gbc.gridx = 3;
		gbc.gridy = 5;
		jtfMassRange2 = new JTextField("Range 2");
		add(jtfMassRange2,gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 8;
		jtfAcidAmountRange1 = new JTextField("Range 1");
		add(jtfAcidAmountRange1,gbc);
		
		gbc.gridx = 3;
		gbc.gridy = 8;
		jtfAcidAmountRange2 = new JTextField("Range 2");
		add(jtfAcidAmountRange2,gbc);
		
		gbc.gridwidth = 2;
		
		gbc.gridx = 2;
		gbc.gridy = 3;
		jtfAtomicNum = new JTextField("Atomic Number");
		add(jtfAtomicNum,gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 4;
		jtfAtomicMass = new JTextField("AtomicMass");
		add(jtfAtomicMass,gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 6;
		jtfPartOf = new JTextField("PartOf");
		add(jtfPartOf,gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 7;
		jtfAcidAmount = new JTextField("Amount");
		add(jtfAcidAmount,gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 9;
		jtfDissolvedBy = new JTextField("Dissolved By");
		add(jtfDissolvedBy ,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 10;
		JLabel lowInvLabel = new JLabel("Filter By Low Inventory");
		add(lowInvLabel, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 11;
		
		add(filterButton, gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 11;
		
		add(clearButton, gbc);
		
	}
	
	public String getFilter() {
		String filter = "" + filterType;
		try {
			switch(filterType) {
				case 12:
					filter = "" + 12;
					break;
				case 1:
					filter = filter + "-" + jtfName.getText();
					break;
				case 2:
					filter = filter + "-" + Double.parseDouble(jtfInventory.getText());
					break;
				case 3:
					filter = filter + "-" + Double.parseDouble(jtfInventoryRange1.getText()) + "-" + Double.parseDouble(jtfInventoryRange2.getText());
					break;
				case 4:
					filter = filter + "-" + Integer.parseInt(jtfAtomicNum.getText());
					break;
				case 5:
					filter = filter + "-" + Integer.parseInt(jtfAtomicMass.getText());
					break;
				case 6:
					filter = filter + "-" + Double.parseDouble(jtfMassRange1.getText()) + "-" + Double.parseDouble(jtfMassRange2.getText());
					break;
				case 7:
					filter = filter + "-" + Double.parseDouble(jtfAcidAmount.getText());
					break;
				case 8:
					filter = filter + "-" + Double.parseDouble(jtfAcidAmountRange1.getText() + "-" + Double.parseDouble(jtfAcidAmountRange2.getText()));
					break;
				case 9:
					filter = filter + "-" + Integer.parseInt(jtfDissolvedBy.getText());
					break;
				case 10:
					filter = filter + "-" + Integer.parseInt(jtfPartOf.getText());
					break;
				case 11:
					filter = "11";
					break;
			}
		} catch(NumberFormatException e) {
			new FailureFrame("Could not Filter Metal");
			return "" + 0;
		}
		return filter;
	}
}
