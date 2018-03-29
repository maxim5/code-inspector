/*
 * Interface.java
 *
 * Created on January 14, 2010, 1:38 PM
 */

package com.deitloff.gui;
import com.deitloff.zoology.Animal;
import java.util.ArrayList;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;

public class Interface extends javax.swing.JFrame
{
    private ArrayList<JCheckBox> _animalOptions = new ArrayList<JCheckBox>();
    private ArrayList<Integer> _selectedAnimals = new ArrayList<Integer>();
    private int _selectedAnimalIndex = 0;
    private ArrayList<ImageIcon> _rightArrowIcons = new ArrayList<ImageIcon>();
    private ArrayList<ImageIcon> _leftArrowIcons = new ArrayList<ImageIcon>();
    private Barnyard _barnyard;
            
    public Interface(Barnyard barnyard)
    {
        javax.swing.ImageIcon icon =
                new javax.swing.ImageIcon("src/com/deitloff/gui/farmIcon16.png", "yay");
        setIconImage(icon.getImage());
        initComponents();
        _barnyard = barnyard;
        getContentPane().setBackground(new java.awt.Color(166,255,90));
        setBounds(0, 0, 26 + this.jScrollPane2.getWidth(), 371);
        _rightArrowIcons.add(new ImageIcon("src/com/deitloff/gui/rightArrowGreen.png"));
        _rightArrowIcons.add(new ImageIcon("src/com/deitloff/gui/rightArrowBlue.png"));
        _rightArrowIcons.add(new ImageIcon("src/com/deitloff/gui/rightArrowRed.png"));
        arrowRight.setVisible(false);
        _leftArrowIcons.add(new ImageIcon("src/com/deitloff/gui/leftArrowGreen.png"));
        _leftArrowIcons.add(new ImageIcon("src/com/deitloff/gui/leftArrowBlue.png"));
        _leftArrowIcons.add(new ImageIcon("src/com/deitloff/gui/leftArrowRed.png"));
        arrowLeft.setVisible(false);
    }
    
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jRadioButton3 = new javax.swing.JRadioButton();
        jRadioButton2 = new javax.swing.JRadioButton();
        buttonGroup1 = new javax.swing.ButtonGroup();
        buttonGroup2 = new javax.swing.ButtonGroup();
        jFrame1 = new javax.swing.JFrame();
        jDialog1 = new javax.swing.JDialog();
        panelPicture = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        btnSingSong = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        jPanel1 = new javax.swing.JPanel();
        btnReloadAnimals = new javax.swing.JButton();
        btnSelectAll = new javax.swing.JButton();
        btnSelectNone = new javax.swing.JButton();
        pictureBox = new javax.swing.JLabel();
        labelCurrentAnimal = new javax.swing.JLabel();
        arrowRight = new javax.swing.JLabel();
        arrowLeft = new javax.swing.JLabel();
        mnuMainMenu = new javax.swing.JMenuBar();
        mnuProgram = new javax.swing.JMenu();
        mnuHelp = new javax.swing.JMenu();
        mnuHelpItem = new javax.swing.JMenuItem();
        jSeparator1 = new javax.swing.JSeparator();
        mnuProjectSpecifications = new javax.swing.JMenuItem();
        mnuClassHierarchy = new javax.swing.JMenuItem();
        mnuAbout = new javax.swing.JMenuItem();

        jRadioButton3.setText("jRadioButton3");
        jRadioButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton3ActionPerformed(evt);
            }
        });

        jRadioButton2.setText("jRadioButton2");

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("Ol' MacDonald");
        setBackground(new java.awt.Color(166, 255, 90));
        setMinimumSize(new java.awt.Dimension(395, 290));
        setResizable(false);
        getContentPane().setLayout(new org.netbeans.lib.awtextra.AbsoluteLayout());

        jScrollPane1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 235, 0), 2));

        jTextArea1.setColumns(20);
        jTextArea1.setEditable(false);
        jTextArea1.setFont(new java.awt.Font("Arial Narrow", 0, 14)); // NOI18N
        jTextArea1.setRows(5);
        jTextArea1.setAutoscrolls(false);
        jTextArea1.setBorder(null);
        jTextArea1.setEnabled(false);
        jScrollPane1.setViewportView(jTextArea1);

        getContentPane().add(jScrollPane1, new org.netbeans.lib.awtextra.AbsoluteConstraints(10, 120, 250, 190));

        btnSingSong.setText("Sing-a-song!");
        btnSingSong.setEnabled(false);
        btnSingSong.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                createTheSong(evt);
            }
        });
        getContentPane().add(btnSingSong, new org.netbeans.lib.awtextra.AbsoluteConstraints(280, 280, 100, 20));

        jPanel1.setBackground(new java.awt.Color(255, 255, 255));
        jPanel1.setAutoscrolls(true);
        jPanel1.setLayout(new java.awt.GridLayout(0, 4));

        btnReloadAnimals.setText("jButton1");
        btnReloadAnimals.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnReloadAnimalsActionPerformed(evt);
            }
        });
        jPanel1.add(btnReloadAnimals);

        jScrollPane2.setViewportView(jPanel1);

        getContentPane().add(jScrollPane2, new org.netbeans.lib.awtextra.AbsoluteConstraints(10, 10, 380, 70));

        btnSelectAll.setText("[select all]");
        btnSelectAll.setEnabled(false);
        btnSelectAll.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnSelectAllActionPerformed(evt);
            }
        });
        getContentPane().add(btnSelectAll, new org.netbeans.lib.awtextra.AbsoluteConstraints(10, 90, 120, -1));

        btnSelectNone.setText("[clear selections]");
        btnSelectNone.setEnabled(false);
        btnSelectNone.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnSelectNoneActionPerformed(evt);
            }
        });
        getContentPane().add(btnSelectNone, new org.netbeans.lib.awtextra.AbsoluteConstraints(270, 90, 120, -1));

        pictureBox.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        getContentPane().add(pictureBox, new org.netbeans.lib.awtextra.AbsoluteConstraints(300, 160, 48, 48));

        labelCurrentAnimal.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        getContentPane().add(labelCurrentAnimal, new org.netbeans.lib.awtextra.AbsoluteConstraints(294, 210, 60, 14));

        arrowRight.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                enterRightArrow(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                exitRightArrow(evt);
            }
            public void mousePressed(java.awt.event.MouseEvent evt) {
                clickRightArrow(evt);
            }
            public void mouseReleased(java.awt.event.MouseEvent evt) {
                releaseRightArrow(evt);
            }
        });
        getContentPane().add(arrowRight, new org.netbeans.lib.awtextra.AbsoluteConstraints(360, 180, 16, 16));

        arrowLeft.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                enterLeftArrow(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                exitLeftArrow(evt);
            }
            public void mousePressed(java.awt.event.MouseEvent evt) {
                clickLeftArrow(evt);
            }
            public void mouseReleased(java.awt.event.MouseEvent evt) {
                releaseLeftArrow(evt);
            }
        });
        getContentPane().add(arrowLeft, new org.netbeans.lib.awtextra.AbsoluteConstraints(270, 180, 16, 16));

        mnuProgram.setText("Program");
        mnuMainMenu.add(mnuProgram);

        mnuHelp.setText("Help");

        mnuHelpItem.setText("Help");
        mnuHelp.add(mnuHelpItem);
        mnuHelp.add(jSeparator1);

        mnuProjectSpecifications.setText("Project Specifications");
        mnuProjectSpecifications.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                displayProjectSpecifications(evt);
            }
            public void mousePressed(java.awt.event.MouseEvent evt) {
                displayProjectSpecifications(evt);
            }
        });
        mnuHelp.add(mnuProjectSpecifications);

        mnuClassHierarchy.setText("Class Hierarchy");
        mnuClassHierarchy.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                displayClassHierarchy(evt);
            }
            public void mousePressed(java.awt.event.MouseEvent evt) {
                displayClassHierarchy(evt);
            }
        });
        mnuHelp.add(mnuClassHierarchy);

        mnuAbout.setText("About");
        mnuAbout.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                displayAboutBox(evt);
            }
            public void mousePressed(java.awt.event.MouseEvent evt) {
                displayAboutBox(evt);
            }
        });
        mnuHelp.add(mnuAbout);

        mnuMainMenu.add(mnuHelp);

        setJMenuBar(mnuMainMenu);

        pack();
    }// </editor-fold>//GEN-END:initComponents
    
    public void addAnimalOption(Animal animal)
    {
        if (btnReloadAnimals.isVisible())
            AnimalLoaded();
         JCheckBox livestockCheck = new JCheckBox(animal.whatAmI());
         jPanel1.add(livestockCheck);
         _animalOptions.add(livestockCheck);
         livestockCheck.setVisible(true);
         livestockCheck.validate();
         jPanel1.validate();
    }
    
    private void AnimalLoaded()
    {
        btnReloadAnimals.setVisible(false);
        jPanel1.remove(btnReloadAnimals);
        jTextArea1.setEnabled(true);
        btnSelectAll.setEnabled(true);
        btnSelectNone.setEnabled(true);
        btnSingSong.setEnabled(true);
    }
    
private void jRadioButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton3ActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_jRadioButton3ActionPerformed

private void btnSelectNoneActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSelectNoneActionPerformed
    for (JCheckBox box : _animalOptions)
        box.setSelected(false);
}//GEN-LAST:event_btnSelectNoneActionPerformed

private void btnSelectAllActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSelectAllActionPerformed
    for (JCheckBox box : _animalOptions)
        box.setSelected(true);
}//GEN-LAST:event_btnSelectAllActionPerformed

private void btnReloadAnimalsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnReloadAnimalsActionPerformed
    AnimalLoaded();
}//GEN-LAST:event_btnReloadAnimalsActionPerformed

private void createTheSong(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_createTheSong
    String song = "";
    int animalIndex = 0;
    _selectedAnimalIndex = 0;
    _selectedAnimals.clear();
    for (JCheckBox box : _animalOptions)
    {
        if (box.isSelected())
        {
            _selectedAnimals.add(animalIndex);
            song += _barnyard.createSongVerse(animalIndex);
        }
        animalIndex++;
    }
    jTextArea1.setText(song);
    if (_selectedAnimals.size() <= 0)
    {
        labelCurrentAnimal.setVisible(false);
        pictureBox.setVisible(false);
        arrowRight.setVisible(false);
        arrowLeft.setVisible(false);
        return;
    }
    ChangeAnimal(0);
}//GEN-LAST:event_createTheSong

private void displayAboutBox(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_displayAboutBox
    DialogAbout dAbout = new DialogAbout();
    dAbout.setVisible(true);
}//GEN-LAST:event_displayAboutBox

private void displayClassHierarchy(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_displayClassHierarchy

}//GEN-LAST:event_displayClassHierarchy

private void displayProjectSpecifications(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_displayProjectSpecifications
    DialogProjectSpec dProj = new DialogProjectSpec();
    dProj.setVisible(true);
}//GEN-LAST:event_displayProjectSpecifications

private boolean _hoveringRightArrow = false;
private boolean _rightArrowClickActivatedOnce = false;
private void clickRightArrow(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_clickRightArrow
    if (_rightArrowClickActivatedOnce)
        return;
    if (ChangeAnimal(_selectedAnimalIndex + 1))
    {
        _selectedAnimalIndex++;
        _rightArrowClickActivatedOnce = true;
    }
}//GEN-LAST:event_clickRightArrow

private void releaseRightArrow(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_releaseRightArrow
    _rightArrowClickActivatedOnce = false;
}//GEN-LAST:event_releaseRightArrow

private void enterRightArrow(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_enterRightArrow
    if (!arrowRight.isVisible())
        return;
    _hoveringRightArrow = true;
    if (_selectedAnimalIndex < _selectedAnimals.size() - 1)
        arrowRight.setIcon(_rightArrowIcons.get(1));
    else
        arrowRight.setIcon(_rightArrowIcons.get(2));
}//GEN-LAST:event_enterRightArrow

private void exitRightArrow(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_exitRightArrow
    if (!arrowRight.isVisible())
        return;
    _hoveringRightArrow = false;
    if (_selectedAnimalIndex < _selectedAnimals.size() - 1)
        arrowRight.setIcon(_rightArrowIcons.get(0));
    else
        arrowRight.setIcon(_rightArrowIcons.get(2));
}//GEN-LAST:event_exitRightArrow

private boolean _hoveringLeftArrow = false;
private boolean _leftArrowClickActivatedOnce = false;
private void enterLeftArrow(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_enterLeftArrow
    if (!arrowLeft.isVisible())
        return;
    _hoveringLeftArrow = true;
    if (_selectedAnimalIndex > 0)
        arrowLeft.setIcon(_leftArrowIcons.get(1));
    else
        arrowLeft.setIcon(_leftArrowIcons.get(2));
}//GEN-LAST:event_enterLeftArrow

private void exitLeftArrow(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_exitLeftArrow
    if (!arrowLeft.isVisible())
        return;
    _hoveringLeftArrow = false;
    if (_selectedAnimalIndex > 0)
        arrowLeft.setIcon(_leftArrowIcons.get(0));
    else
        arrowLeft.setIcon(_leftArrowIcons.get(2));
}//GEN-LAST:event_exitLeftArrow

private void clickLeftArrow(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_clickLeftArrow
    if (_leftArrowClickActivatedOnce)
        return;
    if (ChangeAnimal(_selectedAnimalIndex - 1))
    {
        _selectedAnimalIndex--;
        _leftArrowClickActivatedOnce = true;
    }
}//GEN-LAST:event_clickLeftArrow

private void releaseLeftArrow(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_releaseLeftArrow
    _leftArrowClickActivatedOnce = false;
}//GEN-LAST:event_releaseLeftArrow

private boolean ChangeAnimal(int toAnimal)
{
    if (toAnimal >= _selectedAnimals.size() || toAnimal < 0)
        return false;
    if (!labelCurrentAnimal.isVisible())
        labelCurrentAnimal.setVisible(true);
    labelCurrentAnimal.setText(_barnyard.getAnimalName(_selectedAnimals.get(toAnimal)));
    if (!pictureBox.isVisible())
        pictureBox.setVisible(true);
    pictureBox.setIcon(_barnyard.getAnimalPicture(_selectedAnimals.get(toAnimal)));
    if (!arrowRight.isVisible())
        arrowRight.setVisible(true);
    if (!arrowLeft.isVisible())
        arrowLeft.setVisible(true);
    if (toAnimal < _selectedAnimals.size() - 1)
    {
        if (_hoveringRightArrow)
            arrowRight.setIcon(_rightArrowIcons.get(1));
        else
            arrowRight.setIcon(_rightArrowIcons.get(0));
    } else
        arrowRight.setIcon(_rightArrowIcons.get(2));
    if (toAnimal > 0)
    {
        if (_hoveringLeftArrow)
            arrowLeft.setIcon(_leftArrowIcons.get(1));
        else
            arrowLeft.setIcon(_leftArrowIcons.get(0));
    } else
        arrowLeft.setIcon(_leftArrowIcons.get(2));
    return true;
}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel arrowLeft;
    private javax.swing.JLabel arrowRight;
    private javax.swing.JButton btnReloadAnimals;
    private javax.swing.JButton btnSelectAll;
    private javax.swing.JButton btnSelectNone;
    private javax.swing.JButton btnSingSong;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.JDialog jDialog1;
    private javax.swing.JFrame jFrame1;
    public javax.swing.JPanel jPanel1;
    private javax.swing.JRadioButton jRadioButton2;
    private javax.swing.JRadioButton jRadioButton3;
    private javax.swing.JScrollPane jScrollPane1;
    public javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JLabel labelCurrentAnimal;
    private javax.swing.JMenuItem mnuAbout;
    private javax.swing.JMenuItem mnuClassHierarchy;
    private javax.swing.JMenu mnuHelp;
    private javax.swing.JMenuItem mnuHelpItem;
    private javax.swing.JMenuBar mnuMainMenu;
    private javax.swing.JMenu mnuProgram;
    private javax.swing.JMenuItem mnuProjectSpecifications;
    private javax.swing.JPanel panelPicture;
    private javax.swing.JLabel pictureBox;
    // End of variables declaration//GEN-END:variables

}
