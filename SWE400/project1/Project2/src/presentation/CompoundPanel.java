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

import command.compound.*;
import model.Compound;
import model.DomainModelException;


public class CompoundPanel extends JPanel{

	JScrollPane compounds = new JScrollPane(); 
	GridBagConstraints gbc = new GridBagConstraints(); 
	JButton addButton = new JButton("Add");
	JButton deleteButton = new JButton("Delete");
	JButton filterButton = new JButton("Filter");
	JButton detailsButton = new JButton("Details");
	JLabel selected = null;
	Compound selectedCompound = null;
	Color labelColor = new Color(30,30,30);
	List<Compound> compoundList = new ArrayList<Compound>();
	String filter = "6";
	
	public CompoundPanel() {
		this.setLayout(new GridBagLayout());
		addScrollPane();
		setButtons();
	}

	private void addScrollPane() {
		compounds.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		compounds.add(compounds.createVerticalScrollBar());
		
		compounds.setViewportView(buildLabels());
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1;
		gbc.weighty = Integer.MAX_VALUE;
		gbc.fill = GridBagConstraints.BOTH;
		add(compounds,gbc);
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
		        addCompound();
		      }
		    });
		deleteButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        deleteCompound();
		      }
		    });
		filterButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        filterCompound();
		      }
		    });
		detailsButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        getDetailsCompound();
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
	
	private void addCompound() {
		new AddCompoundFrame().addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent arg0) {
				compounds.setViewportView(buildLabels());
			}
		});
	}
	
	private void deleteCompound() {
		if(selected != null) {
			try {
				new CompoundDeleteCommand(selectedCompound).execute();
				compounds.setViewportView(buildLabels());
			} catch (DomainModelException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	private void filterCompound() {
		FilterCompoundFrame faf = new FilterCompoundFrame();
        faf.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosed(WindowEvent arg0) {
                filter = faf.getFilter();
                System.out.println(filter);
                compounds.setViewportView(buildLabels());
            }
        });
		
	}
	
	private void getDetailsCompound() {
		if(selected != null) {
			new CompoundDetailsFrame(selectedCompound).addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosed(WindowEvent arg0) {
					compounds.setViewportView(buildLabels());
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
			compoundList = new CompoundFilterCommand(filter).execute();
		} catch (DomainModelException e1) {
			
			e1.printStackTrace();
		}
		
		labels.setLayout(new GridLayout(compoundList.size(), 1));
		
		for(int i = 0; i < compoundList.size(); i++) {
		      final int x = i;
		      JLabel label = new JLabel(buildHtml(compoundList.get(i)));
		      label.setOpaque(true);
		      label.setBackground(new Color(30, 30, 30));
		      label.addMouseListener( new MouseAdapter() {
		          @Override
		          public void mouseClicked(MouseEvent e) {
		              removeSelectedBackground();
		              label.setBackground(new Color(234, 201, 55));
		              selected = label;
		              selectedCompound = compoundList.get(x);
		          }
		      }); 
		      labels.add(label, i, 0);
		    }

		return labels;
	}
    
	private String buildHtml(Compound compound) {
		return "<html><p style=\"color:white;\">" + compound.getName() + "</p></html>";
	}

}
