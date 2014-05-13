import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;

public class GUI extends UI {

	private class Update implements Runnable{
		public void run(){
			while(true){
				updateTimeline();
				try{
					Thread.sleep(500);
				} catch(InterruptedException e){}
			}
		}
	}

	private AudioManager manager;

	private String adress;
	private int inPort, outPort;

	private JButton playButton;
	private JButton nextButton;
	private JButton toggleShuffle;
	private JList songList;
	private JSlider timeline;

	private boolean playButt = true;
	private boolean shuffelBool = true;

	public GUI(){
		try{
			manager = new AudioManager("bitninja.se",1341,1340);
		} catch(Exception e){System.out.println("AudioManager failed!!!");};
	}

	public void start() {
	//communicator = new Communicator(adress,inPort,outPort);

		JFrame frame = new JFrame("erlStream");
		frame.setSize(800,600);
		frame.setResizable(false);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel mainPanel = new JPanel();
		((FlowLayout)mainPanel.getLayout()).setVgap(0);
		((FlowLayout)mainPanel.getLayout()).setHgap(0);

		JPanel panelLeft = new JPanel();
		((FlowLayout)panelLeft.getLayout()).setVgap(0);
		((FlowLayout)panelLeft.getLayout()).setHgap(0);

		JPanel panelRigth = new JPanel();
		((FlowLayout)panelRigth.getLayout()).setVgap(0);
		((FlowLayout)panelRigth.getLayout()).setHgap(0);


		Thread update = new Thread(new Update());
		update.start();

	//playButton
		playButton = new JButton (new ImageIcon("play.png"));
		playButton.setBorder(null);
		//playButton.addActionListener(this);
		playButton.setOpaque(false);
		playButton.setContentAreaFilled(false);
		playButton.setBorderPainted(false);
		playButton.addMouseListener(new MouseAdapter(){
			public void mousePressed(MouseEvent e){
				if(playButt){
					playButton.setIcon(new ImageIcon("pressedPlay.png"));
				} else {
					playButton.setIcon(new ImageIcon("pressedPause.png"));
				}
			}
			public void mouseClicked(MouseEvent e){
				try{
					if(playButt){
						playButton.setIcon(new ImageIcon("pause.png"));
						playButt = false;
						manager.play();
					} else {
						playButton.setIcon(new ImageIcon("play.png"));	
						playButt = true;
						manager.pause();
					}
				} catch(Exception a){System.out.println("Failed!!!");}			    
			}
		});

	//Slider
		timeline = new JSlider(JSlider.HORIZONTAL,0,100,0);
		
		
	//NextButton
		nextButton = new JButton (new ImageIcon("next.png"));
		nextButton.setBorder(null);
		nextButton.setOpaque(false);
		nextButton.setContentAreaFilled(false);
		nextButton.setBorderPainted(false);
		nextButton.addMouseListener(new MouseAdapter(){

			public void mousePressed(MouseEvent e){
				nextButton.setIcon(new ImageIcon("pressednext.png"));
			}


			public void mouseClicked(MouseEvent e){
				try{
					nextButton.setIcon(new ImageIcon("next.png"));
					manager.next();
				} catch(Exception b) {System.out.println("next failed");}
			}
		});

	//toggleShuffle
		toggleShuffle = new JButton (new ImageIcon("shuffle.png"));
		toggleShuffle.setBorder(null);
		toggleShuffle.setOpaque(false);
		toggleShuffle.setContentAreaFilled(false);
		toggleShuffle.setBorderPainted(false);
		toggleShuffle.addMouseListener(new MouseAdapter(){

			public void mouseClicked(MouseEvent e){
				try{
					if(shuffelBool){
						toggleShuffle.setIcon(new ImageIcon("pressedshuffle.png"));
						manager.setShuffle(!manager.shuffleIsOn());
						shuffelBool = false;
					} else {
						toggleShuffle.setIcon(new ImageIcon("shuffle.png"));
						manager.setShuffle(!manager.shuffleIsOn());
						shuffelBool = true;
					}
				} catch(Exception b) {System.out.println("next failed");}
			}
		});


		
	//songList
		try{
			songList = new JList(manager.getSongs().toArray());
		} catch (Exception a){System.out.println("Song list failed");};
	//Add everything
		panelLeft.add(playButton);
		panelLeft.add(nextButton);
		panelLeft.add(toggleShuffle);

		panelRigth.add(songList);
		panelLeft.add(timeline);

		mainPanel.add(panelLeft);
		mainPanel.add(panelRigth);

		frame.add(mainPanel);

		frame.setLocationRelativeTo(null);
		frame.pack();
		frame.setVisible(true);

	}

	public void updateTimeline(){

		try{
			Hashtable labelTable = new Hashtable();
			timeline.setValue(manager.getPosition());
			labelTable.put(new Integer(manager.getPosition()), new JLabel(Song.secondsToString(manager.getPosition())));
			timeline.setMaximum(manager.getCurrentSong().getDuration());
			labelTable.put(new Integer(manager.getCurrentSong().getDuration()), new JLabel(manager.getCurrentSong().getDurationString()));
			timeline.setLabelTable(labelTable);
			timeline.setPaintLabels(true);
		}catch(Exception c) {System.out.println("Thread fucking things!!!");}
	}


    /*	public void actionPerformed(ActionEvent e) {
	if(e.getSource() == playButton){
	try{
	if(playButt){
	playButton.setIcon(new ImageIcon("pause.png"));
	playButt = false;
	manager.play();
	} else {
	playButt = true;
	playButton.setIcon(new ImageIcon("play.png"));
	manager.pause();
	}
	} catch(Exception a){System.out.println("Failed!!!");}
	}
	if(e.getSource() == nextButton){
	try{
	manager.next();
	} catch(Exception b) {System.out.println("next failed");}
	}
	if(e.getSource() == songList){
	try{
	manager.play();
	} catch(Exception a) {System.out.println("List play failed");}
	}
	}
    */


}