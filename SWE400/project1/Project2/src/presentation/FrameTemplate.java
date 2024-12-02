package presentation;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import javax.swing.JFrame;

/**
 * 
 * @author andrewjanuszko
 *
 */
public abstract class FrameTemplate extends JFrame {
  
  /**
   * serialVersionUID.
   */
  private static final long serialVersionUID = 556209478722069517L;
  private GridBagConstraints gridBagConstraints;
  private int width;
  private int height;

  /**
   * 
   */
  public FrameTemplate() {
    setWidth(300);
    setHeight(450);
    setGridBagConstraints(); 
    setLayout(new GridBagLayout());
    setBackground(Color.BLACK);
    setUp();
    setSize(getWidth(), getHeight());
    setResizable(true);
    setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    setVisible(true);
  }
  
  /**
   * 
   */
  public abstract void setUp();

  /**
   * 
   */
  private void setGridBagConstraints() {
    this.gridBagConstraints = new GridBagConstraints(); 
  }
  
  /**
   * 
   * @param width
   */
  public void setWidth(int width) {
    this.width = width;
  }
  
  /**
   * 
   * @param height
   */
  public void setHeight(int height) {
    this.height = height;
  }
  
  /**
   * 
   */
  public int getWidth() {
    return width;
  }
  
  /**
   * 
   */
  public int getHeight() {
    return height;
  }
  
  /**
   * 
   * @return
   */
  public GridBagConstraints getGridBagConstraints() {
    return gridBagConstraints;
  }
}
