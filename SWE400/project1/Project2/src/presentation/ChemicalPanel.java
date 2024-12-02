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
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;

import command.chemical.ChemicalFilterCommand;
import model.Acid;
import model.BaseDataMapper;
import model.Chemical;

public class ChemicalPanel extends JPanel{
	
  JScrollPane chemicals = new JScrollPane();
  GridBagConstraints gbc = new GridBagConstraints();
  JButton filterButton = new JButton("Filter");
  JButton detailsButton = new JButton("Details");
  Chemical selectedChemical;
  JLabel selected = null;
  Color labelColor = new Color(30, 30, 30);
  List<Chemical> chemicallList;
  String filter = "4";
  
  
  public ChemicalPanel() {
    this.setLayout(new GridBagLayout());
    addScrollPane();
    setButtons();
  }

  private void addScrollPane() {
    chemicals.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    chemicals.add(chemicals.createVerticalScrollBar());

    chemicals.setViewportView(buildLabels());
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 1;
    gbc.weighty = Integer.MAX_VALUE;
    gbc.fill = GridBagConstraints.BOTH;
    add(chemicals, gbc);
  }

  private JLabel Labels() {
    JLabel label = new JLabel();
    label.setBackground(Color.WHITE);
    label.setOpaque(true);
    label.setPreferredSize(new Dimension(100, 20));
    return label;
  }

  private void setButtons() {
    filterButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent ae) {
        filterChemical();
      }
    });
    detailsButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent ae) {
        getDetailChemical();
      }
    });
    JPanel buttons = new JPanel(new GridBagLayout());
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.weightx = 0;
    gbc.gridy = 0;

    gbc.gridx = 0;
    buttons.add(filterButton, gbc);

    gbc.gridx = 1;
    buttons.add(detailsButton, gbc);

    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.anchor = GridBagConstraints.SOUTHWEST;
    gbc.weighty = 1;
    buttons.setBackground(Color.GRAY);

    add(buttons, gbc);

  }

  private void filterChemical() {
   
      FilterChemicalFrame fcf = new FilterChemicalFrame();
    	fcf.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosed(WindowEvent arg0) {
        	filter = fcf.getFilter();
        	chemicals.setViewportView(buildLabels());
        }
      });
    
  }

  private void getDetailChemical() {
    if (selected != null) {
       new ChemicalDetailsFrame(selectedChemical);
    }
  }

  private void removeSelectedBackground() {
    if (selected != null)
      selected.setBackground(labelColor);
  }

  private JPanel buildLabels() {
	  JPanel labels = new JPanel();
		List<Chemical> baseList = new ArrayList<Chemical>();
		try {
			baseList = (List<Chemical>) new ChemicalFilterCommand(filter).execute();
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
		        	  if(selectedChemical != null)
		        		  selected.setBackground(new Color(30, 30, 30));
		              label.setBackground(new Color(234, 201, 55));
		              selected = label;
		              selectedChemical = b;
		          }
		      }); 
		      labels.add(label, i, 0);
		    }

		return labels;
  }
  
  private String buildHtml(Chemical chem) {
		return "<html><p style=\"color:white;\">" + chem.getName() + "</p></html>";
	}
}
