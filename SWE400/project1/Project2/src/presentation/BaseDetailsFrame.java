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


import command.base.BaseUpdateCommand;
import command.chemical.ChemicalFilterCommand;
import model.Acid;
import model.Base;
import model.Chemical;
import model.DomainModelException;
import model.Metal;

public class BaseDetailsFrame extends JFrame{

	Base base;
	GridBagConstraints gbc = new GridBagConstraints();
	Chemical selectedAcid = null;
	JTextField jtfName;
	JTextField jtfInventory;
	JLabel selected;
	JScrollPane acids = new JScrollPane();
	JButton updateButton = new JButton("Update");
	
	
	public BaseDetailsFrame(Base a) {
		base = a;
		setLayout(new GridBagLayout());
    	setBackground(Color.BLACK);
    	setUp();
    	setSize(300, 450);
    	setResizable(true);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		setVisible(true);
	}
	
	public void setUp() {
		jtfName = new JTextField(base.getName());
		jtfInventory = new JTextField("" + base.getInventory());
		
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		add(jtfName,gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 1;
		add(jtfInventory, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 2;
		acids.setViewportView(buildLabels());
		acids.setVerticalScrollBar(acids.createVerticalScrollBar());
		add(acids,gbc);
		
		updateButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				update();
				dispose();
			}
		});
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		add(updateButton, gbc);
	}
	
	private JPanel buildLabels() {
		  Chemical a = base.getSolute();
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
	            
	            if(a.getID() == m.getID()) {
	            	label.setBackground(new Color(234, 201, 55));
	            	selectedAcid = m;
	            	selected = label;
	            } else {
	            	label.setBackground(new Color(30, 30, 30));
	            }
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
	  
	  private void update() {
		  try {
				base.setName(jtfName.getText());
				base.setInventory(Double.parseDouble(jtfInventory.getText()));
				base.setSolute(selectedAcid);
				
				new BaseUpdateCommand(base).execute();
			} catch (NumberFormatException | DomainModelException e) {
				new FailureFrame("Could not update Base");
			}
	  }

}
