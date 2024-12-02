package presentation;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

public class FirstWindow extends JFrame {

  JPanel currentDisplay = null;
  JPanel buttons = null;
  GridBagConstraints gbc = new GridBagConstraints();
  JButton chemicalButton = new JButton("Chemical");
  JButton acidButton = new JButton("Acid");
  JButton baseButton = new JButton("Base");
  JButton compoundButton = new JButton("Compound");
  JButton elementButton = new JButton("Element");
  JButton metalButton = new JButton("Metal");

  public FirstWindow() {
    setLayout(new GridBagLayout());
    setBackground(Color.BLACK);
    setSize(500, 200);
    setResizable(true);
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    setButtons();
    setVisible(true);
  }

  private void setButtons() {
    chemicalButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent ae) {
        setChemicalView();
      }
    });
    acidButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent ae) {
        setAcidView();
      }
    });
    baseButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent ae) {
        setBaseView();
      }

    });
    compoundButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent ae) {
        setCompoundView();
      }

    });
    elementButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent ae) {
        setElementView();
      }

    });
    metalButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent ae) {
        setMetalView();
      }

    });
    buttons = new JPanel(new GridBagLayout());

    gbc.weightx = 1;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.gridx = 0;
    gbc.gridy = 0;
    buttons.add(chemicalButton, gbc);

    gbc.gridx = 1;
    gbc.gridy = 0;
    buttons.add(acidButton, gbc);

    gbc.gridx = 2;
    gbc.gridy = 0;
    buttons.add(baseButton, gbc);

    gbc.gridx = 3;
    gbc.gridy = 0;
    buttons.add(compoundButton, gbc);

    gbc.gridx = 4;
    gbc.gridy = 0;
    buttons.add(elementButton, gbc);

    gbc.gridx = 5;
    gbc.gridy = 0;
    buttons.add(metalButton, gbc);

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.anchor = GridBagConstraints.NORTHWEST;
    gbc.weighty = 1;
    buttons.setBackground(Color.GRAY);

    add(buttons, gbc);

  }

  private void setChemicalView() {
    if (currentDisplay != null)
      remove(currentDisplay);

    currentDisplay = new ChemicalPanel();
    System.out.println("Chemical View");
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weighty = 100000;
    gbc.fill = GridBagConstraints.BOTH;

    add(currentDisplay, gbc);
    SwingUtilities.updateComponentTreeUI(this);
  }

  private void setAcidView() {
    if (currentDisplay != null)
      remove(currentDisplay);

    currentDisplay = new AcidPanel();
    System.out.println("Acid View");
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weighty = 100000;
    gbc.fill = GridBagConstraints.BOTH;

    add(currentDisplay, gbc);
    SwingUtilities.updateComponentTreeUI(this);
  }

  private void setBaseView() {
    if (currentDisplay != null)
      remove(currentDisplay);

    currentDisplay = new BasePanel();
    System.out.println("Base View");
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weighty = 100000;
    gbc.fill = GridBagConstraints.BOTH;

    add(currentDisplay, gbc);
    SwingUtilities.updateComponentTreeUI(this);

  }

  private void setCompoundView() {
    if (currentDisplay != null)
      remove(currentDisplay);

    currentDisplay = new CompoundPanel();
    System.out.println("Compound View");
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weighty = 100000;
    gbc.fill = GridBagConstraints.BOTH;

    add(currentDisplay, gbc);
    SwingUtilities.updateComponentTreeUI(this);
  }

  private void setElementView() {
    if (currentDisplay != null)
      remove(currentDisplay);

    currentDisplay = new ElementPanel();
    System.out.println("Element View");
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weighty = 100000;
    gbc.fill = GridBagConstraints.BOTH;

    add(currentDisplay, gbc);
    SwingUtilities.updateComponentTreeUI(this);
  }

  private void setMetalView() {
    if (currentDisplay != null)
      remove(currentDisplay);

    currentDisplay = new MetalPanel();
    System.out.println("Metal View");
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weighty = 100000;
    gbc.fill = GridBagConstraints.BOTH;

    add(currentDisplay, gbc);
    SwingUtilities.updateComponentTreeUI(this);
  }
}
