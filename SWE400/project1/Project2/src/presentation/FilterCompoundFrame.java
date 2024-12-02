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

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import command.element.ElementFilterCommand;
import model.Compound;
import model.DomainModelException;
import model.Element;

public class FilterCompoundFrame extends JFrame{

	int filterType;        
	JRadioButton nameFilter = new JRadioButton();        //filterType 1
	JRadioButton inventoryFilter = new JRadioButton();   //filterType 2
	JRadioButton inventoryRangeFilter = new JRadioButton();   //filterType 3
	JRadioButton madeOfFilter = new JRadioButton();
	JRadioButton lowInvFilter = new JRadioButton();
	GridBagConstraints gbc = new GridBagConstraints();
	JButton filterButton = new JButton("Filter");
	JButton clearButton = new JButton("Clear Filter");
	JTextField jtfName;
	JTextField jtfInventory;
	JTextField jtfInventoryRange1;
	JTextField jtfInventoryRange2;
	JScrollPane elements = new JScrollPane();
	Element selectedElement;
	JLabel selected;
	
	public FilterCompoundFrame() {
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
		madeOfFilter.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
		        filterType = 4;
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
		add(madeOfFilter, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 3;
		JLabel madeOfLabel = new JLabel("Made Of: ");
		add(madeOfLabel,gbc);
		
		gbc.gridwidth = 2;
		
		gbc.gridx = 2;
		gbc.gridy = 0;
		jtfName = new JTextField("Name");
		add(jtfName,gbc);

		gbc.gridx = 2;
		gbc.gridy = 1;
		jtfInventory = new JTextField("Inventory");
		add(jtfInventory,gbc);
		gbc.gridx = 2;
		gbc.gridy = 3;
		elements.setViewportView(buildLabels());
		add(elements,gbc);
		
		gbc.gridwidth = 1;
		gbc.gridx = 2;
		gbc.gridy = 2;
		jtfInventoryRange1 = new JTextField("Range 1");
		add(jtfInventoryRange1,gbc);
		
		gbc.gridx = 3;
		gbc.gridy = 2;
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
				case 0:
					filter = "" + 0;
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
					filter = filter + "-" + selectedElement.getID();
					break;
				case 5:
					filter = "5";
			}
		} catch(NumberFormatException e) {
			new FailureFrame("Could not Filter Compound");
			return "" + 0;
		}
		return filter;
	}
	private void removeSelectedBackground() {
	    if(selected != null)
	      selected.setBackground(new Color(30, 30, 30));
	  }
	

	private JPanel buildLabels() {
		JPanel labels = new JPanel();
		List<Element> elementList = new ArrayList<Element>();
		try {
			elementList = new ElementFilterCommand("9").execute();
		} catch (DomainModelException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		labels.setLayout(new GridLayout(elementList.size(), 1));
		
		for(int i = 0; i < elementList.size(); i++) {
		      final int x = i;
		      final Element ele = elementList.get(x);
		      JLabel label = new JLabel(buildHtml(elementList.get(i)));
		      label.setOpaque(true);
		      label.setBackground(new Color(30, 30, 30));
		      label.addMouseListener( new MouseAdapter() {
		          @Override
		          public void mouseClicked(MouseEvent e) {
		              removeSelectedBackground();
		              label.setBackground(new Color(234, 201, 55));
		              selected = label;
		              selectedElement = ele;
		          }
		      }); 
		      labels.add(label, i, 0);
		    }

		return labels;
	}
    
	private String buildHtml(Element element) {
		return "<html><p style=\"color:white;\">" + element.getName() + "</p></html>";
	}

}
