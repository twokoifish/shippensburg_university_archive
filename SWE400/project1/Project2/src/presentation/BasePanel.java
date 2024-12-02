package presentation;

import java.awt.BorderLayout;
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
import javax.swing.SwingConstants;

import model.DomainModelException;
import model.Base;
import command.base.*;


public class BasePanel extends JPanel{
	
	JScrollPane bases = new JScrollPane(); 
	GridBagConstraints gbc = new GridBagConstraints(); 
	JButton addButton = new JButton("Add");
	JButton deleteButton = new JButton("Delete");
	JButton filterButton = new JButton("Filter");
	JButton detailsButton = new JButton("Details");
	JLabel selected = null;
	Base selectedBase = null;
	
	Color labelColor = new Color(30,30,30);
	List<Base> baseList = new ArrayList<Base>();
	String filter = "6";
	
	public BasePanel() {
		this.setLayout(new GridBagLayout());
		addScrollPane();
		setButtons();
	}
	
	private JLabel Labels() {
		JLabel label = new JLabel();
		label.setBackground(Color.WHITE);
	    label.setOpaque(true);
	    label.setPreferredSize(new Dimension(100,20));
		return label;
	}
	
	private void addScrollPane() {
		bases.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		bases.add(bases.createVerticalScrollBar());
		
		bases.setViewportView(buildLabels());
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1;
		gbc.weighty = Integer.MAX_VALUE;
		gbc.fill = GridBagConstraints.BOTH;
		add(bases,gbc);
	}
	
	private void setButtons() {
		addButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        addBase();
		      }
		    });
		deleteButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        deleteBase();
		      }
		    });
		filterButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        filterBase();
		      }
		    });
		detailsButton.addActionListener( new ActionListener() {
		      @Override
		      public void actionPerformed(ActionEvent ae) {
		        getDetailsBase();
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
	
	private void addBase() {
		new AddBaseFrame().addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent arg0) {
				bases.setViewportView(buildLabels());
			}
		});
	}
	
	private void deleteBase() {
		if(selected != null) {
			try {
				new BaseDeleteCommand(selectedBase).execute();
				bases.setViewportView(buildLabels());
			} catch (DomainModelException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	private void filterBase() {
			//brings up new window based on selected base
		FilterBaseFrame fbf = new FilterBaseFrame();
			fbf.addWindowListener(new WindowAdapter() {
				@Override
				public void windowClosed(WindowEvent arg0) {
					filter = fbf.getFilter();
					bases.setViewportView(buildLabels());
				}
			});
	}
	
	private void getDetailsBase() {
		if(selected == null) {
			return;
		}
		new BaseDetailsFrame(selectedBase);
	}
	
	private void removeSelectedBackground() {
	    if(selected != null)
	      selected.setBackground(labelColor);
	  }
	
	private JPanel buildLabels() {
		JPanel labels = new JPanel();
		try {
			baseList = new BaseFilterCommand(filter).execute();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		labels.setLayout(new GridLayout(baseList.size(), 1));
		
		for(int i = 0; i < baseList.size(); i++) {
		      final int x = i;
		      JLabel label = new JLabel(buildHtml(baseList.get(i)));
		      label.setOpaque(true);
		      label.setBackground(new Color(30, 30, 30));
		      label.addMouseListener( new MouseAdapter() {
		          @Override
		          public void mouseClicked(MouseEvent e) {
		              removeSelectedBackground();
		              label.setBackground(new Color(234, 201, 55));
		              selected = label;
		              selectedBase = baseList.get(x);
		          }
		      }); 
		      labels.add(label, i, 0);
		    }

		return labels;
	}
    
	private String buildHtml(Base base) {
		return "<html><p style=\"color:white;\">" + base.getName() + "</p></html>";
	}
}
