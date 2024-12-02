package presentation;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import command.element.*;
import model.DomainModelException;
import model.Element;


public class ElementPanel extends JPanel{

	JScrollPane elements = new JScrollPane(); 
	GridBagConstraints gbc = new GridBagConstraints(); 
	JButton addButton = new JButton("Add");
	JButton deleteButton = new JButton("Delete");
	JButton filterButton = new JButton("Filter");
	JButton detailsButton = new JButton("Details");
	JLabel selected = null;
	Element selectedElement = null;
	Color labelColor = new Color(30,30,30);

	List<Element> elementList = new ArrayList<Element>();
	String filter = "9";
	
	public ElementPanel() {
		this.setLayout(new GridBagLayout());
		addScrollPane();
		setButtons();
	}

	private void addScrollPane() {
		elements.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		elements.add(elements.createVerticalScrollBar());
		
		elements.setViewportView(buildLabels());
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1;
		gbc.weighty = Integer.MAX_VALUE;
		gbc.fill = GridBagConstraints.BOTH;
		add(elements,gbc);
	}
	
	private JLabel Labels() {
		JLabel label = new JLabel();
		label.setBackground(Color.WHITE);
	    label.setOpaque(true);
	    label.setPreferredSize(new Dimension(100,20));
		return label;
	}
	
	private void setButtons() {
		addButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        addElement();
		      }
		    });
		deleteButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        deleteElement();
		      }
		    });
		filterButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        filterElement();
		        
		      }
		    });
		detailsButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        getDetailsElement();
		      }
		    });
		JPanel buttons = new JPanel(new GridBagLayout());
		gbc.fill = GridBagConstraints.HORIZONTAL;
	    gbc.weightx = 0;
	    gbc.gridx = 0;
	    gbc.gridy = 0;
	    buttons.add(addButton, gbc);
	    
	    gbc.gridx = 1;
	    buttons.add(deleteButton, gbc);
	    
	    gbc.gridx = 2;
	    buttons.add(filterButton, gbc);
	    
	    gbc.gridx = 3;
	    buttons.add(detailsButton, gbc);
	    
	    gbc.gridx = 0;
	    gbc.gridy = 1;
	    gbc.anchor = GridBagConstraints.SOUTHWEST;
	    gbc.weighty = 1;
	    buttons.setBackground(Color.GRAY);
	    
	    add(buttons, gbc);

	}
	
	private void addElement() {
		new AddElementFrame().addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent arg0) {
				elements.setViewportView(buildLabels());
			}
		});
	}
	
	private void deleteElement() {
		if(selected != null) {
			try {
				new ElementDeleteCommand(selectedElement).execute();
				elements.setViewportView(buildLabels());
			} catch (DomainModelException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	private void filterElement() {
		
			//brings up new window elementd on selected element
			FilterElementFrame fef = new FilterElementFrame();
			fef.addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosed(WindowEvent arg0) {
					filter = fef.getFilter();
					elements.setViewportView(buildLabels());
				}
			});
		
	}
	
	private void getDetailsElement() {
		if(selected != null) {
			new ElementDetailsFrame(selectedElement).addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosed(WindowEvent arg0) {
					elements.setViewportView(buildLabels());
				}
			});
		}
	}
	
	private void removeSelectedBackground() {
	    if(selected != null)
	      selected.setBackground(labelColor);
	  }
	

	private JPanel buildLabels() {
		JPanel labels = new JPanel();
	
		try {
			elementList = new ElementFilterCommand(filter).execute();
		} catch (DomainModelException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		labels.setLayout(new GridLayout(elementList.size(), 1));
		
		for(int i = 0; i < elementList.size(); i++) {
		      final int x = i;
		      JLabel label = new JLabel(buildHtml(elementList.get(i)));
		      label.setOpaque(true);
		      label.setBackground(new Color(30, 30, 30));
		      label.addMouseListener( new MouseAdapter() {
		          @Override
		          public void mouseClicked(MouseEvent e) {
		              removeSelectedBackground();
		              label.setBackground(new Color(234, 201, 55));
		              selected = label;
		              selectedElement = elementList.get(x);
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
