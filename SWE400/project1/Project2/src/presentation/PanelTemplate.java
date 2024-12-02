package presentation;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.List;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import model.Chemical;

public abstract class PanelTemplate extends JPanel {

  /**
   * 
   */
  private static final long serialVersionUID = 6990570366176146252L;
  private JScrollPane scrollPane = new JScrollPane();
  private GridBagConstraints gridBagConstraints = new GridBagConstraints();
  private JLabel selectedLabel = null;
  private Chemical selectedChemical = null;
  private List<Color> panelColors = List.of(new Color(30,30,30), new Color(234, 201, 55));
  private List<? extends Chemical> chemicals;
  private static String defaultFilter = "1";
  
  
  public PanelTemplate(List<? extends Chemical> chemicals) {
      this.setLayout(new GridBagLayout());
      this.chemicals = chemicals;
      addScrollPane();
      createButtons();
  }
  
  public GridBagConstraints getGridBagConstraints() {
    return gridBagConstraints;
  }
  
  public Chemical getSelectedChemical() {
    return selectedChemical;
  }
  
  private void setSelectedChemical(Chemical selectedChemical) {
    this.selectedChemical = selectedChemical;
  }
  
  public static String getDefaultFilter() {
    return defaultFilter;
  }
  
  private Color getColorSettings(int variant) {
    return panelColors.get(variant);
  }
  
  public abstract void add();
  public abstract void delete();
  public abstract void filter();
  public abstract void details();
  
  private void addScrollPane() {
    scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    scrollPane.add(scrollPane.createVerticalScrollBar());
      
    scrollPane.setViewportView(buildLabels());
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.weightx = 1;
    gridBagConstraints.weighty = Integer.MAX_VALUE;
    gridBagConstraints.fill = GridBagConstraints.BOTH;
    add(scrollPane, gridBagConstraints);
  }
  
  public abstract void createButtons();
  
  private void removeSelectedBackground() {
      if(selectedLabel != null)
        selectedLabel.setBackground(getColorSettings(0));
    }
  
  private JPanel buildLabels() {
      JPanel chemicalPanel = new JPanel();
      
      chemicalPanel.setLayout(new GridLayout(chemicals.size(), 1));
      for (Chemical chemical : chemicals) {
        JLabel chemicalLabel = new JLabel(buildLabelHTML(chemical));
        chemicalLabel.setOpaque(true);
        chemicalLabel.setBackground(getColorSettings(0));
        chemicalLabel.addMouseListener( new MouseAdapter() {
          public void mouseClicked(MouseEvent e) {
            removeSelectedBackground();
            chemicalLabel.setBackground(getColorSettings(1));
            selectedLabel = chemicalLabel;
            setSelectedChemical(chemical);
          }
        });
        chemicalPanel.add(chemicalLabel, chemicals.get(chemicals.indexOf(chemical)), 0);
      }
      return chemicalPanel;
  }
  
  private String buildLabelHTML(Chemical chemical) {
      return "<html><p style=\"color:white;\">" + chemical.getName() + "</p></html>";
  }

}
