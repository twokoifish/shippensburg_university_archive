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
import java.awt.event.WindowListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import model.DomainModelException;
import model.Metal;
import command.metal.*;

public class MetalPanel extends JPanel{

	JScrollPane metals = new JScrollPane(); 
	GridBagConstraints gbc = new GridBagConstraints(); 
	JButton addButton = new JButton("Add");
	JButton deleteButton = new JButton("Delete");
	JButton filterButton = new JButton("Filter");
	JButton detailsButton = new JButton("Details");
	JLabel selected = null;
	Metal selectedMetal = null;
	String filter = "12";
	Color labelColor = new Color(30,30,30);
	List<Metal> metalList = new ArrayList<Metal>();
	
	public MetalPanel() {
		this.setLayout(new GridBagLayout());
		addScrollPane();
		setButtons();
	}

	private void addScrollPane() {
		metals.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		metals.add(metals.createVerticalScrollBar());
		
		metals.setViewportView(buildLabels());
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1;
		gbc.weighty = Integer.MAX_VALUE;
		gbc.fill = GridBagConstraints.BOTH;
		add(metals,gbc);
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
		        addMetal();
		      }
		    });
		deleteButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        deleteMetal();
		      }
		    });
		filterButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        filterMetal();
		      }
		    });
		detailsButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        getDetailsMetal();
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
	
	private void addMetal() {
		new AddMetalFrame().addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent arg0) {
				metals.setViewportView(buildLabels());
			}
		});
	}
	
	private void deleteMetal()  {
		if(selected != null) {
			try {
				new MetalDeleteCommand(selectedMetal).execute();
				metals.setViewportView(buildLabels());
			} catch (DomainModelException e) {
				
			}
		}
	}
	
	private void filterMetal() {
		
			//brings up new window based on selected metal
			new FilterMetalFrame().addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosed(WindowEvent arg0) {
					metals.setViewportView(buildLabels());
				}
			});
			
		
	}
	
	private void getDetailsMetal() {
		if(selected != null) {
			new MetalDetailsFrame(selectedMetal).addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosed(WindowEvent arg0) {
					metals.setViewportView(buildLabels());
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
			metalList = new MetalFilterCommand(filter).execute();
		} catch (DomainModelException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		labels.setLayout(new GridLayout(metalList.size(), 1));
		
		for(int i = 0; i < metalList.size(); i++) {
		      final int x = i;
		      JLabel label = new JLabel(buildHtml(metalList.get(i)));
		      label.setOpaque(true);
		      label.setBackground(new Color(30, 30, 30));
		      label.addMouseListener( new MouseAdapter() {
		          @Override
		          public void mouseClicked(MouseEvent e) {
		              removeSelectedBackground();
		              label.setBackground(new Color(234, 201, 55));
		              selected = label;
		              selectedMetal = metalList.get(x);
		          }
		      }); 
		      labels.add(label, i, 0);
		    }

		return labels;
	}
    
	private String buildHtml(Metal metal) {
		return "<html><p style=\"color:white;\">" + metal.getName() + "</p></html>";
	}


}
