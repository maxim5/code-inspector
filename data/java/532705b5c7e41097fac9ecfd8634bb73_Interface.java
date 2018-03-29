/*
 * Interface.java
 * GUI interface for visualizer
 * Lok Wong
 * Pharos Lab
 * Created: June 11, 2012 9:55 AM
 * Last Modified: August 10, 2012 10:56 PM
 */

package visualizer;

import javax.swing.*;
import javax.swing.plaf.basic.BasicSliderUI;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Hashtable;

public class Interface extends JFrame {
	
	// Progress slider elements
	public static JSlider progress;
	// timeIncrements used to divide progress slider into number of pixels wide under full screen
	private final static int timeIncrements = Toolkit.getDefaultToolkit().getScreenSize().width;
	
	// Control elements
	public static JButton open, play, stop, rewind, fforward;
	private static ImageIcon openImg, playImg, pauseImg, stopImg, rewindImg, fforwardImg;
	private static boolean isPlaying = false;
	private static JFileChooser chooser;
	public static JSlider speedControl;
	private static JLabel speedLabel;
	
	// Time counter elements
	private static JLabel timeElapsed;
	private static long time = 0;					// in ms
	private static String totalTime = new String();
	private static long endTime;
	private static Timer timer;
	private final static int updateCountRate = 310;	// in ms; must be multiple of 10 for time to be always accurate
	private static double countFactor = 1;			// 1 for play, >1 for fast forward, etc.
	private static boolean isTimerStart = false;
	
	private static int frameWidth = 700, frameHeight = 700, panelHeight;
	static Animation animation;
	private static boolean isAnimationChanged; 
	
	public static void main(String args[]){
		Interface gui = new Interface();
		gui.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		gui.setSize(frameWidth+15, frameHeight+panelHeight+35);	// Constants take into account Windows window border
		gui.setVisible(true);
		gui.setTitle("Pharos Lab Visualizer");
		gui.getContentPane().setBounds(0, 0, frameWidth, frameHeight);
		
/*		// Code for command-line input
		for(int i = 0; i < args.length; i++){
			if(args[i].equals("-expdir") && i+1 != args.length){
				animation = new Animation(args[++i]);
				animation.setLocation(0, 0);
				gui.add(animation);
				endTime = animation.endTime;
				// Set start at "00:00 / endTime" with endTime in terms of minutes and seconds with leading zeros
				totalTime = ("" + (endTime/60000 < 10 ? 0 : "") + (endTime/60000) + ':' + (endTime%60000/1000 < 10 ? 0 : "")
						+ endTime%60000/1000);
				timeElapsed.setText("00:00 / " + totalTime);
				break;
			}
		}*/
		while(true){
			if(isAnimationChanged && animation.endTime != 0){
				gui.remove(animation);
				animation.setBounds(0, 0, 700, 700);
				gui.add(animation);
		
				endTime = animation.endTime;
				// Set start at "00:00 / endTime" with endTime in terms of minutes and seconds with leading zeros
				totalTime = ("" + (endTime/60000 < 10 ? 0 : "") + (endTime/60000) + ':' + (endTime%60000/1000 < 10 ? 0 : "")
						+ endTime%60000/1000);
				timeElapsed.setText("00:00 / " + totalTime);
				isAnimationChanged = false;
				
				play.setEnabled(true);
				stop.setEnabled(true);
				rewind.setEnabled(true);
				fforward.setEnabled(true);
			} else{ System.out.print(""); }
		}
	}
	
	public Interface(){
		setLayout(null);
		UIManager.put("Label.font", new Font("SansSerif", Font.PLAIN, 12));
		isAnimationChanged = false;
		
		UIDefaults def = UIManager.getDefaults();
		ImageIcon thumb = new ImageIcon(getClass().getResource("img/slider_thumb.png"));
		def.put("Slider.horizontalThumbIcon", thumb);
		progress = new JSlider(0, timeIncrements, 0);
		progress.setBounds(0, frameHeight, frameWidth, thumb.getIconHeight());
		add(progress);
		TimeChange timeChangeEvent = new TimeChange();
		progress.addMouseListener(timeChangeEvent);
		
		openImg = new ImageIcon(getClass().getResource("img/open.png"));
		open = new JButton(openImg);
		open.setBounds(5, progress.getY()+progress.getHeight()+5, 35, 35);
		add(open);
		ChooseFolder chooseEvent = new ChooseFolder();
		open.addActionListener(chooseEvent);
		// Get JFileChooser to open at PharosVisualizer folder
		String folder = new File(getClass().getResource(".").getPath()).getParentFile().getParentFile().toString();
		chooser = new JFileChooser(folder.replace('\\', '/').replaceAll("%20", " "));
		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		open.setToolTipText("Choose folder to load log files");
		
		playImg = new ImageIcon(getClass().getResource("img/play.png"));
		pauseImg = new ImageIcon(getClass().getResource("img/pause.png"));
		play = new JButton(playImg);
		play.setBounds(open.getX()+open.getWidth()+10, progress.getY()+progress.getHeight()+5, 35, 35);
		add(play);
		Play playEvent = new Play();
		play.addActionListener(playEvent);
		play.setToolTipText("Play");
		play.setEnabled(false);

		stopImg = new ImageIcon(getClass().getResource("img/stop.png"));
		stop = new JButton(stopImg);
		stop.setBounds(play.getX()+play.getWidth()+5, progress.getY()+progress.getHeight()+5, 35, 35);
		add(stop);
		Stop stopEvent = new Stop();
		stop.addActionListener(stopEvent);
		stop.setToolTipText("Stop");
		stop.setEnabled(false);

		rewindImg = new ImageIcon(getClass().getResource("img/rewind.png"));
		rewind = new JButton(rewindImg);
		rewind.setBounds(stop.getX()+stop.getWidth()+10, progress.getY()+progress.getHeight()+5, 35, 35);
		add(rewind);
		Rewind rewindEvent = new Rewind();
		rewind.addActionListener(rewindEvent);
		rewind.setToolTipText("Rewind (-1x)");
		rewind.setEnabled(false);

		fforwardImg = new ImageIcon(getClass().getResource("img/fforward.png"));
		fforward = new JButton(fforwardImg);
		fforward.setBounds(rewind.getX()+rewind.getWidth()+5, progress.getY()+progress.getHeight()+5, 35, 35);
		add(fforward);	
		FForward fforwardEvent = new FForward();
		fforward.addActionListener(fforwardEvent);
		fforward.setToolTipText("Fast Forward (2x)");
		fforward.setEnabled(false);

		panelHeight = progress.getHeight()+play.getHeight()+10;

		speedControl = new JSlider(-100, 100, 0);
		speedControl.setUI(new BasicSliderUI(speedControl));
		speedControl.setFocusable(false);
		speedControl.setBounds(fforward.getX()+fforward.getWidth()+10,
				progress.getY()+progress.getHeight()+(panelHeight-progress.getHeight()-44)/2,
				250, 44);
		speedControl.setMajorTickSpacing(10);
		speedControl.setPaintTicks(true);
		Hashtable<Integer, JLabel> table = new Hashtable<Integer, JLabel>();
		table.put(-100, new JLabel("-10"));
		table.put(-50, new JLabel("-5"));
		table.put(0, new JLabel("0"));
		table.put(50, new JLabel("5"));
		table.put(100, new JLabel("10"));
		speedControl.setLabelTable(table);
		speedControl.setPaintLabels(true);
		add(speedControl);
		SpeedChange speedChangeEvent = new SpeedChange();
		speedControl.addMouseListener(speedChangeEvent);
		
		speedLabel = new JLabel();
		speedLabel.setBounds(speedControl.getX()+speedControl.getWidth()+5,
				progress.getY()+progress.getHeight()+(panelHeight-progress.getHeight()-21)/2, 80, 21);
		speedLabel.setFont(new Font("SansSerif", Font.PLAIN, 12));
		speedLabel.setText("Speed: " + (double)speedControl.getValue()/10 + "x");
		add(speedLabel);
		
		timeElapsed = new JLabel("00:00 / 00:00");
		timeElapsed.setBounds(frameWidth-thumb.getIconWidth()/2-151,
				progress.getY()+progress.getHeight()+(panelHeight-progress.getHeight()-21)/2, 150, 21);
		timeElapsed.setHorizontalAlignment(SwingConstants.RIGHT);
		add(timeElapsed);
	}
	
	private static class TimeChange extends MouseAdapter{
		private int begXpos;
		private int min = UIManager.getDefaults().getIcon("Slider.horizontalThumbIcon").getIconWidth()/2;
		private int max = progress.getWidth()-min-1;
		private boolean outOfBoundsFlag;

		public void mouseReleased(MouseEvent timeChangeEvent){
			try {
				// Time jump only works either when a user clicks somewhere on the slider or drags the knob
				if(/*timeChangeEvent.getButton() == 1 && */(!outOfBoundsFlag || (outOfBoundsFlag && progress.getMousePosition().x == begXpos))){
					int x;
					if(timeChangeEvent.getX() < min){ x = min; }
					else if(timeChangeEvent.getX() >= max){ x = max-1; }
					else{ x = timeChangeEvent.getX(); }
					progress.setValue((int)((double)(x-min)*timeIncrements/(max-min-1)));
					time = (long)((double)(x-min)*animation.endTime/(max-min-1));
					timeElapsed.setText("" + (time/60000 < 10 ? 0 : "") + time/60000 + ':' + (time%60000/1000 < 10 ? 0 : "")
							+ time%60000/1000 + " / " + totalTime);
					animation.setTime((double)(x-min)/(max-min-1));
				}
			} catch (Exception e) {}
		}
		
		public void mousePressed(MouseEvent timeChangeEvent){
			int adjValue = (int)(((double)progress.getValue())*(max-min-1)/timeIncrements+min);  
			if(progress.getMousePosition().x <= adjValue-min || progress.getMousePosition().x > adjValue+min){
				this.begXpos = progress.getMousePosition().x;
				outOfBoundsFlag = true;
			} else{ outOfBoundsFlag = false; }
		}
	}
	
	private static class ChooseFolder implements ActionListener{
		public void actionPerformed(ActionEvent chooseEvent){
			int x = chooser.showOpenDialog(null);
			if(x == JFileChooser.APPROVE_OPTION){
				if(chooser.getSelectedFile().canRead()){
					if(animation != null){
						animation.removeAll();
						animation.destroy();
					}
					animation = new Animation(chooser.getSelectedFile().getPath());
					animation.repaint();
					isAnimationChanged = true;
				} else{ JOptionPane.showMessageDialog(null, "Folder does not exist."); }
			}
		}
	}
	
	private static class Play implements ActionListener{
		public void actionPerformed(ActionEvent playEvent){
			if(!isPlaying){
				// Play pressed
				Count countEvent = new Count();
				animation.start();
				play.setIcon(pauseImg);
				isPlaying = true;
				countFactor = 1;
				if(timer == null){ timer = new Timer(updateCountRate, countEvent); }
				play.addActionListener(countEvent);
				timer.start();
				isTimerStart = true;
				speedControl.setValue(10);
				speedLabel.setText("Speed: 1.0x");
				play.setToolTipText("Pause");
				rewind.setToolTipText("Rewind (-1x)");
				rewind.setEnabled(true);
				fforward.setToolTipText("Fast Forward (2x)");
				fforward.setEnabled(true);
			} else{
				// Pause pressed
				try { animation.pause(); }
				catch (Exception e) {}
				play.setIcon(playImg);
				isPlaying = false;
				countFactor = 0;
				speedControl.setValue(0);
				speedLabel.setText("Speed: 0.0x");
				play.setToolTipText("Play");
			}
		}
	}
	
	private static class Stop implements ActionListener{
		public void actionPerformed(ActionEvent stopEvent){
			animation.stop();
			play.setIcon(playImg);
			isPlaying = false;
			if(timer != null){ timer.stop(); }
			isTimerStart = false;
			timeElapsed.setText("00:00 / " + totalTime);
			progress.setValue(0);
			countFactor = 0;
			time = 0;
			speedControl.setValue(0);
			speedLabel.setText("Speed: 0.0x");
			play.setToolTipText("Play");
			rewind.setToolTipText("Rewind (-1x)");
			rewind.setEnabled(true);
			fforward.setToolTipText("Fast Forward (2x)");
			fforward.setEnabled(true);
		}
	}
	
	private static class Rewind implements ActionListener{
		public void actionPerformed(ActionEvent rewindEvent){
			animation.rewind();
			play.setIcon(playImg);
			isPlaying = false;
			play.setToolTipText("Play");
			if(!isTimerStart){
				Count countEvent = new Count();
				timer = new Timer(updateCountRate, countEvent);
				rewind.addActionListener(countEvent);
				timer.start();
				isTimerStart = true;
			}
			if(speedControl.getValue() > -10){
				countFactor = -1;
				speedControl.setValue(-10);
				animation.setSpeed(-1);
				speedLabel.setText("Speed: -1.0x");
				rewind.setToolTipText("Rewind (-2x)");
				rewind.setEnabled(true);
			} else if(speedControl.getValue() > -20 && speedControl.getValue() <= -10){
				countFactor = -2;
				speedControl.setValue(-20);
				animation.setSpeed(-2);
				speedLabel.setText("Speed: -2.0x");
				rewind.setToolTipText("Rewind (-5x)");
				rewind.setEnabled(true);
			} else if(speedControl.getValue() > -50 && speedControl.getValue() <= -20){
				countFactor = -5;
				speedControl.setValue(-50);
				animation.setSpeed(-5);
				speedLabel.setText("Speed: -5.0x");
				rewind.setToolTipText("Rewind (-10x)");
				rewind.setEnabled(true);
			} else if(speedControl.getValue() > -100 && speedControl.getValue() <= -50){
				countFactor = -10;
				speedControl.setValue(-100);
				animation.setSpeed(-10);
				speedLabel.setText("Speed: -10.0x");
				rewind.setToolTipText("Rewind (maxed)");
				rewind.setEnabled(false);
			}
			fforward.setToolTipText("Fast Forward (2x)");
			fforward.setEnabled(true);
		}
	}
	
	private static class FForward implements ActionListener{
		public void actionPerformed(ActionEvent fforwardEvent){
			animation.fforward();
			play.setIcon(playImg);
			isPlaying = false;
			play.setToolTipText("Play");
			if(!isTimerStart){
				Count countEvent = new Count();
				timer = new Timer(updateCountRate, countEvent);
				fforward.addActionListener(countEvent);
				timer.start();
				isTimerStart = true;
			}
			if(speedControl.getValue() < 20){
				countFactor = 2;
				speedControl.setValue(20);
				animation.setSpeed(2);
				speedLabel.setText("Speed: 2.0x");
				fforward.setToolTipText("Fast Forward (5x)");
				fforward.setEnabled(true);
			} else if(speedControl.getValue() >= 20 && speedControl.getValue() < 50){
				countFactor = 5;
				speedControl.setValue(50);
				animation.setSpeed(5);
				speedLabel.setText("Speed: 5.0x");
				fforward.setToolTipText("Fast Forward (10x)");
				fforward.setEnabled(true);
			} else if(speedControl.getValue() >= 50 && speedControl.getValue() < 100){
				countFactor = 10;
				speedControl.setValue(100);
				animation.setSpeed(10);
				speedLabel.setText("Speed: 10.0x");
				fforward.setToolTipText("Fast Forward (maxed)");
				fforward.setEnabled(false);
			}
			rewind.setToolTipText("Rewind (-1x)");
			rewind.setEnabled(true);
		}
	}
	
	private static class SpeedChange extends MouseAdapter{
		private int begXpos;
		private int min = 10;
		private int max = speedControl.getWidth()-min;
		private boolean outOfBoundsFlag;

		public void mouseReleased(MouseEvent speedChangeEvent){
			try {
				if(/*speedChangeEvent.getButton() == 1 && */(!outOfBoundsFlag || (outOfBoundsFlag && speedControl.getMousePosition().x == begXpos))){
					int x;
					if(speedChangeEvent.getX() < min){ x = min; }
					else if(speedChangeEvent.getX() >= max){ x = max; }
					else{ x = speedChangeEvent.getX(); }
					speedControl.setValue((int)((double)(x-min)*200/(max-min)-100));
					countFactor = (double)speedControl.getValue()/10;
					animation.setSpeed((double)speedControl.getValue()/10);
					speedLabel.setText("Speed: " + (double)speedControl.getValue()/10 + "x");
				}
			} catch (Exception e) {}
			if(speedControl.getValue() == -100){
				rewind.setToolTipText("Rewind (maxed)");
				rewind.setEnabled(false);
				fforward.setToolTipText("Fast Forward (2x)");
				fforward.setEnabled(true);
			} else if(speedControl.getValue() > -100 && speedControl.getValue() <= -50){
				rewind.setToolTipText("Rewind (-10x)");
				rewind.setEnabled(true);
				fforward.setToolTipText("Fast Forward (2x)");
				fforward.setEnabled(true);
			} else if(speedControl.getValue() > -50 && speedControl.getValue() <= -20){
				rewind.setToolTipText("Rewind (-5x)");
				rewind.setEnabled(true);
				fforward.setToolTipText("Fast Forward (2x)");
				fforward.setEnabled(true);
			} else if(speedControl.getValue() > -20 && speedControl.getValue() <= -10){
				rewind.setToolTipText("Rewind (-2x)");
				rewind.setEnabled(true);
				fforward.setToolTipText("Fast Forward (2x)");
				fforward.setEnabled(true);
			} else if(speedControl.getValue() > -10 && speedControl.getValue() < 20){
				rewind.setToolTipText("Rewind (-1x)");
				rewind.setEnabled(true);
				fforward.setToolTipText("Fast Forward (2x)");
				fforward.setEnabled(true);
			} else if(speedControl.getValue() >= 20 && speedControl.getValue() < 50){
				rewind.setToolTipText("Rewind (-1x)");
				rewind.setEnabled(true);
				fforward.setToolTipText("Fast Forward (5x)");
				fforward.setEnabled(true);
			} else if(speedControl.getValue() >= 50 && speedControl.getValue() < 100){
				rewind.setToolTipText("Rewind (-1x)");
				rewind.setEnabled(true);
				fforward.setToolTipText("Fast Forward (10x)");
				fforward.setEnabled(true);
			} else if(speedControl.getValue() == 100){
				rewind.setToolTipText("Rewind (-1x)");
				rewind.setEnabled(true);
				fforward.setToolTipText("Fast Forward (maxed)");
				fforward.setEnabled(false);
			}
			play.setIcon(playImg);
			isPlaying = false;
			play.setToolTipText("Play");
		}
		
		public void mousePressed(MouseEvent speedChangeEvent){
			int adjValue = (int)((double)(speedControl.getValue()+100)*(max-min)/(speedControl.getMaximum()-speedControl.getMinimum())+min);
			if(speedControl.getMousePosition().x <= adjValue-min || speedControl.getMousePosition().x > adjValue+min){
				this.begXpos = speedControl.getMousePosition().x;
				outOfBoundsFlag = true;
			} else{ outOfBoundsFlag = false; }
		}
	}
	
	private static class Count implements ActionListener{
		public void actionPerformed(ActionEvent countEvent){
			if((time <= endTime-(updateCountRate*countFactor) && countFactor >= 0)
					|| (time >= Math.abs(updateCountRate*countFactor) && countFactor < 0)){
				time += updateCountRate*countFactor;
				timeElapsed.setText("" + (time/60000 < 10 ? 0 : "") + time/60000 + ':' + (time%60000/1000 < 10 ? 0 : "")
				+ time%60000/1000 + " / " + totalTime);
			} else if(time < Math.abs(updateCountRate*countFactor) && countFactor < 0){
				timeElapsed.setText("00:00 / " + totalTime);
				timer.stop();
			} else if(time > endTime-(updateCountRate*countFactor) && countFactor >= 0){
				timeElapsed.setText("" + totalTime + " / " + totalTime);
				timer.stop();				
			}
			progress.setValue((int)(time*timeIncrements/endTime));
		}
	}
}
