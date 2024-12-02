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

import command.base.BaseCreateCommand;
import command.chemical.ChemicalFilterCommand;
import model.Acid;
import model.Chemical;
import model.DomainModelException;

public class AddBaseFrame extends JFrame{
	GridBagConstraints gbc = new GridBagConstraints(); 
	int height = 450, width = 300;
	Chemical selectedAcid = null;
	JLabel selected = null;
	JScrollPane acids = new JScrollPane();
	
	public AddBaseFrame() {
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
		JLabel soluteLabel = new JLabel("Solute: ");
		add(soluteLabel,gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 2;
		acids.setViewportView(buildLabels());
		acids.setVerticalScrollBar(acids.createVerticalScrollBar());
		add(acids,gbc);
		
		JButton add = new JButton("Add");
		add.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				int id;
				double inventory;
				int solute;
				String name;
				try {
					
					inventory = Double.parseDouble(jtfInventory.getText());
					
					name = jtfName.getText();
					
					dispose();
					try {
						new BaseCreateCommand(name, inventory, selectedAcid).execute();
					} catch (DomainModelException e1) {
						e1.printStackTrace();
					}
				} catch (NumberFormatException e1) {
					new FailureFrame("Failed to create Base");
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
	      List<Chemical> acidList = new ArrayList<Chemical>();
	      try {
	        acidList =  (List<Chemical>) new ChemicalFilterCommand("4").execute();
	      } catch (Exception e) {
	        // TODO Auto-generated catch block
	        e.printStackTrace();
	      }
	      
	      labels.setLayout(new GridLayout(acidList.size(), 1));
	      
	      for(int i = 0; i < acidList.size(); i++) {
	            final int x = i;
	            final Chemical m = acidList.get(i);
	            JLabel label = new JLabel(buildHtml(acidList.get(i)));
	            label.setOpaque(true);
	            
	            
	            label.setBackground(new Color(30, 30, 30));
	            label.addMouseListener( new MouseAdapter() {
	                @Override
	                public void mouseClicked(MouseEvent e) {
	                    removeSelectedBackground();
	                    label.setBackground(new Color(234, 201, 55));
	                    selected = label;
	                    selectedAcid = m;
	                }
	            }); 
	            labels.add(label, i, 0);
	          }

	      return labels;
	  }
	  
	  private String buildHtml(Chemical acid) {
	      return "<html><p style=\"color:white;\">" + acid.getName() + "</p></html>";
	  }
	  
	  private void removeSelectedBackground() {
	      if(selected != null)
	        selected.setBackground(new Color(30, 30, 30));
	    }
}
