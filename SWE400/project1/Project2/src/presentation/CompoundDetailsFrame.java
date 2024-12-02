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

import command.compound.CompoundUpdateCommand;
import command.element.ElementFilterCommand;
import model.Compound;
import model.DomainModelException;
import model.Element;
import model.Metal;

public class CompoundDetailsFrame extends JFrame{
	Compound compound;
	GridBagConstraints gbc = new GridBagConstraints();
	JScrollPane elements = new JScrollPane();
	List<Element> selectedElements = new ArrayList<Element>();
	List<Integer> madeOf = new ArrayList<Integer>();
	JTextField jtfName;
	JTextField jtfInventory;
	JButton update = new JButton("Update");
	
	public CompoundDetailsFrame(Compound a) {
		compound = a;
		//selectedElements = compound.getMadeOf();
		compound.getMadeOf().forEach(x -> madeOf.add(x.getID()));
		
		setLayout(new GridBagLayout());
    	setBackground(Color.BLACK);
    	
    	setSize(300, 450);
    	setResizable(true);
    	setUp();
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		setVisible(true);
	}
	
	public void setUp() {
		//JLabel name = new JLabel(compound.getName());
		//JLabel inventory = new JLabel("" + compound.getInventory());
		
		
		//JLabel madeOf = new JLabel();
		/*
		List<Element> elementList = compound.getMadeOf();
		if(elementList != null)
			elementList.forEach(x -> madeOf.setText(madeOf.getText() + " " + x.getName()));
		else 
			madeOf.setText("nothing");
		*/
		gbc.weightx = 1;
		gbc.weighty = 1;
		gbc.fill = GridBagConstraints.BOTH;
		jtfName = new JTextField(compound.getName());
		gbc.gridx = 0;
		gbc.gridy = 0;
		add(jtfName,gbc);
		
		jtfInventory = new JTextField("" + compound.getInventory());
		gbc.gridx = 0;
		gbc.gridy = 1;
		add(jtfInventory, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		elements.add(elements.createVerticalScrollBar());
		elements.setViewportView(buildLabels());
		add(elements, gbc);
		
		update.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				update();
			}
			
		});
		
		gbc.gridx = 0;
		gbc.gridy = 4;
		add(update,gbc);
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
		      for(int j = 0; j < madeOf.size(); j++) {
		      	if(m.getID() == madeOf.get(j)) {
		      		selectedElements.add(m);
		      	}
		      }
		      if(selectedElements.contains(m))
		    	  label.setBackground(new Color(234, 201, 55)); 
		      else
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
	
	private void update() {
		try {
			compound.setName(jtfName.getText());
			compound.setInventory(Double.parseDouble(jtfInventory.getText()));
			compound.setMadeOf(selectedElements);
			new CompoundUpdateCommand(compound).execute();
		} catch (NumberFormatException | DomainModelException e) {
			new FailureFrame("Could not update Compound");
		}
		dispose();
	}
}
