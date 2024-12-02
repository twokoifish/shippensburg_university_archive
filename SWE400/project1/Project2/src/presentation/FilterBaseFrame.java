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

import command.chemical.ChemicalFilterCommand;
import model.Chemical;

public class FilterBaseFrame extends JFrame{
	int filterType = 6;        
	JRadioButton nameFilter = new JRadioButton();        //filterType 1
	JRadioButton soluteFilter = new JRadioButton();      //filterType 2
	JRadioButton inventoryFilter = new JRadioButton();   //filterType 3
	JRadioButton inventoryRangeFilter = new JRadioButton();   //filterType 4
	JRadioButton lowInvFilter = new JRadioButton();
	GridBagConstraints gbc = new GridBagConstraints();
	JButton filterButton = new JButton("Filter");
	JButton clearButton = new JButton("Clear Filter");
	JTextField jtfName;
	JTextField jtfSolute;
	JTextField jtfInventory;
	JTextField jtfInventoryRange1;
	JTextField jtfInventoryRange2;
	JScrollPane chemicals = new JScrollPane();
	Chemical selectedChemical = null;
	JLabel selected;
	
	
	
	public FilterBaseFrame() {
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
		soluteFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 4;
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
		lowInvFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 5;
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
				filterType = 6;
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
		add(soluteFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 1;
		JLabel soluteLabel = new JLabel("Solute Filter: ");
		add(soluteLabel,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 2;
		add(inventoryFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 2;
		JLabel inventoryLabel = new JLabel("Inventory Filter: ");
		add(inventoryLabel,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		add(inventoryRangeFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 3;
		JLabel inventoryRangeLabel = new JLabel("Inventory Range Filter: ");
		add(inventoryRangeLabel,gbc);
		
		
		gbc.gridwidth = 2;
		
		gbc.gridx = 2;
		gbc.gridy = 0;
		jtfName = new JTextField("Name");
		add(jtfName,gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 1;
		//jtfSolute = new JTextField("Solute");
		chemicals.setViewportView(buildLabels());
		chemicals.setVerticalScrollBar(chemicals.createVerticalScrollBar());
		add(chemicals,gbc);
		

		gbc.gridx = 2;
		gbc.gridy = 2;
		jtfInventory = new JTextField("Inventory");
		add(jtfInventory,gbc);
		
		gbc.gridwidth = 1;
		
		gbc.gridx = 2;
		gbc.gridy = 3;
		
		jtfInventoryRange1 = new JTextField("Range 1");
		add(jtfInventoryRange1,gbc);
		
		gbc.gridx = 3;
		gbc.gridy = 3;
		
		jtfInventoryRange2 = new JTextField("Range 2");
		add(jtfInventoryRange2,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 4;
		add(lowInvFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 4;
		JLabel lowInvLabel = new JLabel("Filter By Low Inventory");
		add(lowInvLabel,gbc);
		
		gbc.gridwidth = 2;
		
		gbc.gridx = 0;
		gbc.gridy = 5;
		
		add(filterButton, gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 5;
		
		add(clearButton, gbc);
		
	}
	
	public String getFilter() {
		String filter = "" + filterType;
		try {
			switch(filterType) {
				case 6:
					filter = "" + 6;
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
					filter = filter + "-" + selectedChemical.getID();
					break;
				case 5:
					filter = "5";
					break;
			}
		} catch(NumberFormatException e) {
			new FailureFrame("Could not Filter Base");
			return "" + 0;
		}
		return filter;
	}
	
	private JPanel buildLabels() {
		JPanel labels = new JPanel();
		List<Chemical> baseList = new ArrayList<Chemical>();
		try {
			baseList = (List<Chemical>) new ChemicalFilterCommand("4").execute();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		labels.setLayout(new GridLayout(baseList.size(), 1));
		
		for(int i = 0; i < baseList.size(); i++) {
		      final int x = i;
		      final Chemical b = baseList.get(x);
		      JLabel label = new JLabel(buildHtml(baseList.get(i)));
		      label.setOpaque(true);
		      label.setBackground(new Color(30, 30, 30));
		      label.addMouseListener( new MouseAdapter() {
		          @Override
		          public void mouseClicked(MouseEvent e) {
		        	  if(selectedChemical != null)
		        		  selected.setBackground(new Color(30, 30, 30));
		              label.setBackground(new Color(234, 201, 55));
		              selected = label;
		              selectedChemical = b;
		          }
		      }); 
		      labels.add(label, i, 0);
		    }

		return labels;
	}
    
	private String buildHtml(Chemical base) {
		return "<html><p style=\"color:white;\">" + base.getName() + "</p></html>";
	}
}
