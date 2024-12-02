package presentation;

import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import command.compound.CompoundFilterCommand;
import model.Compound;
import model.DomainModelException;

public class FilterElementFrame extends JFrame {

	int filterType;        
	JRadioButton nameFilter = new JRadioButton();        //filterType 1
	JRadioButton inventoryFilter = new JRadioButton();   //filterType 2
	JRadioButton inventoryRangeFilter = new JRadioButton();   //filterType 3
	JRadioButton numberFilter = new JRadioButton();   //filterType 4
	JRadioButton massFilter = new JRadioButton();   //filterType 5
	JRadioButton massBetweenFilter = new JRadioButton();
	JRadioButton partOfFilter = new JRadioButton();
	JRadioButton lowInvFilter = new JRadioButton();
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
	//JTextField jtfPartOf;
	JScrollPane compounds = new JScrollPane();
	Compound selectedCompound;
	JLabel selected;
	
	public FilterElementFrame() {
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
		partOfFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 7;
		      }
		});
		lowInvFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 8;
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
				filterType = 9;
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
		//jtfPartOf = new JTextField("PartOf");
		compounds.setViewportView(buildLabels());
		add(compounds,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 7;
		JLabel lowInvLabel = new JLabel("Filter By Low Inventory");
		add(lowInvLabel, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 8;
		
		add(filterButton, gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 8;
		
		add(clearButton, gbc);
		
	}
	
	public String getFilter() {
		String filter = "" + filterType;
		try {
			switch(filterType) {
				case 9:
					filter = "" + 9;
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
					filter = filter + "-" + selectedCompound.getID();
					break;
				case 8:
					filter = "8";
					break;
			}
		} catch(NumberFormatException e) {
			new FailureFrame("Could not Filter Element");
			return "" + 0;
		}
		return filter;
	}
	
	private JPanel buildLabels() {
		JPanel labels = new JPanel();
		List<Compound> compoundList = new ArrayList<Compound>();
		try {
			compoundList = new CompoundFilterCommand("6").execute();
		} catch (DomainModelException e1) {
			
			e1.printStackTrace();
		}
		
		labels.setLayout(new GridLayout(compoundList.size(), 1));
		
		for(int i = 0; i < compoundList.size(); i++) {
		      final int x = i;
		      final Compound c = compoundList.get(x);
		      JLabel label = new JLabel(buildHtml(compoundList.get(i)));
		      label.setOpaque(true);
		      label.setBackground(new Color(30, 30, 30));
		      label.addMouseListener( new MouseAdapter() {
		          @Override
		          public void mouseClicked(MouseEvent e) {
		              removeSelectedBackground();
		              label.setBackground(new Color(234, 201, 55));
		              selected = label;
		              selectedCompound = c;
		          }
		      }); 
		      labels.add(label, i, 0);
		    }

		return labels;
	}
    
	private String buildHtml(Compound compound) {
		return "<html><p style=\"color:white;\">" + compound.getName() + "</p></html>";
	}
	
	private void removeSelectedBackground() {
	    if(selected != null)
	      selected.setBackground(new Color(30, 30, 30));
	  }
	

}
