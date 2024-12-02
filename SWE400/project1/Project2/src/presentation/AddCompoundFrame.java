package presentation;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import command.compound.CompoundCreateCommand;
import command.element.ElementFilterCommand;
import model.DomainModelException;
import model.Element;
import model.Element;

public class AddCompoundFrame extends JFrame{
	GridBagConstraints gbc = new GridBagConstraints(); 
	int height = 450, width = 300;
	List<Element> selectedElements = new ArrayList<Element>();
	JScrollPane elements = new JScrollPane();
	
	public AddCompoundFrame() {
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
		JLabel MadeOfLabel = new JLabel("Made Of: ");
		add(MadeOfLabel,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 2;
		elements.add(elements.createVerticalScrollBar());
		elements.setViewportView(buildLabels());
		elements.setSize(this.height/2, 12);
		add(elements, gbc);
		
		JButton add = new JButton("Add");
		add.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				double inventory;
				int solute;
				String name;
				
				try {
			
					inventory = Double.parseDouble(jtfInventory.getText());
					name = jtfName.getText();
					
			
					try {
						new CompoundCreateCommand(name, inventory, selectedElements).execute();
					} catch (DomainModelException e1) {
						e1.printStackTrace();
					}

					dispose();
				} catch (NumberFormatException e1) {
					new FailureFrame("Failed to create Compound");
				}
						
			}
		});
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.gridwidth = 2;
		
		add(add, gbc);
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
		      final Element m = elementList.get(i);
		      JLabel label = new JLabel(buildHtml(m));
		      label.setOpaque(true);
		      label.setBackground(new Color(30, 30, 30));
		      label.addMouseListener( new MouseAdapter() {
		          @Override
		          public void mouseClicked(MouseEvent e) {
		              if(selectedElements.contains(m)) {
		            	  selectedElements.remove(m);
		            	  label.setBackground(new Color(30, 30, 30));
		              }
		              else {
		            	  selectedElements.add(m);
		            	  label.setBackground(new Color(234, 201, 55));
		              }
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
