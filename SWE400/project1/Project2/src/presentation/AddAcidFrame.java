package presentation;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Point;
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
import command.acid.AcidCreateCommand;
import model.Base;
import model.Chemical;
import model.DomainModelException;
import model.Metal;
import command.metal.MetalFilterCommand;
import command.base.BaseFilterCommand;
import command.chemical.ChemicalFilterCommand;

public class AddAcidFrame extends JFrame{
	
	GridBagConstraints gbc = new GridBagConstraints(); 
	int height = 450, width = 300;
	List<Metal> selectedMetals = new ArrayList<Metal>();
	Chemical selectedBase = null;
	JScrollPane metals = new JScrollPane();
	JScrollPane bases = new JScrollPane();
	JLabel selectedLabel = null;
	
	public AddAcidFrame() {
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
		gbc.gridheight = 1;
		
		
		
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
		JLabel soluteLabel = new JLabel("Solute: ");
		add(soluteLabel,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 2;
		bases.add(bases.createVerticalScrollBar());
		bases.setViewportView(buildLabelsBase());
		add(bases, gbc);
		
		JButton add = new JButton("Add");
		add.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				
				
				try {
					
					double inventory = Double.parseDouble(jtfInventory.getText());
					
					String name = jtfName.getText();
						try {
							new AcidCreateCommand(name, inventory, selectedMetals, selectedBase).execute();
						} catch (DomainModelException e1) {
							e1.printStackTrace();
						}
						
					dispose();
				} catch (NumberFormatException e1) {
					new FailureFrame("Failed to create Acid");
				}

			}
		});
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		
		JLabel idLabel = new JLabel("Dissolves:");
		add(idLabel,gbc);
		metals.add(metals.createVerticalScrollBar());
		metals.setViewportView(buildLabelsMetal());
		
		gbc.gridx = 1;
		gbc.gridy = 3;
		add(metals,gbc);

		gbc.gridwidth = 2;
		gbc.gridx = 0;
		gbc.gridy = 5;
		
		add(add, gbc);
	}
	private JPanel buildLabelsMetal() {
		JPanel labels = new JPanel();
		List<Metal> metalList = new ArrayList<Metal>();
		try {
			metalList = new MetalFilterCommand("12").execute();
		} catch (DomainModelException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		labels.setLayout(new GridLayout(metalList.size(), 1));
		
		for(int i = 0; i < metalList.size(); i++) {
		      final int x = i;
		      final Metal m = metalList.get(i);
		      JLabel label = new JLabel(buildHtml(m));
		      label.setOpaque(true);
		      label.setBackground(new Color(30, 30, 30));
		      label.addMouseListener( new MouseAdapter() {
		          @Override
		          public void mouseClicked(MouseEvent e) {
		              if(selectedMetals.contains(m)) {
		            	  selectedMetals.remove(m);
		            	  label.setBackground(new Color(30, 30, 30));
		              }
		              else {
		            	  selectedMetals.add(m);
		            	  label.setBackground(new Color(234, 201, 55));
		              }
		          }
		      }); 
		      labels.add(label, i, 0);
		    }

		return labels;
	}
    
	private String buildHtml(Metal metal) {
		return "<html><p style=\"color:white;\">" + metal.getName() + "</p></html>";
	}
	private JPanel buildLabelsBase() {
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
		        	  if(selectedBase != null)
		        		  selectedLabel.setBackground(new Color(30, 30, 30));
		              label.setBackground(new Color(234, 201, 55));
		              selectedLabel = label;
		              selectedBase = b;
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
