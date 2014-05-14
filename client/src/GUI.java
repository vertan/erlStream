import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;

public class GUI extends UI {

	private class Update implements Runnable{
		public void run(){
			while(true){
				updateTimeline();
				GUIchange = false;
				try{
					Thread.sleep(500);
				} catch(InterruptedException e){}
				
			}
		}
	}

    private class SlideListener implements ChangeListener{
	public void stateChanged(ChangeEvent event){
	    JSlider source = (JSlider)event.getSource();
	    if(!source.getValueIsAdjusting() && !GUIchange){
		try{
		    manager.play(manager.getCurrentSong(),source.getValue()*1000);
		}catch(Exception e){System.out.println("Failed!");}
	    }
	}	
    }
    
    private AudioManager manager;

	private String adress;
	private int inPort, outPort;

	private JButton playButton;
	private JButton nextButton;
	private JButton previousButton;
	private JButton toggleShuffle;
	private JList songList;
	private JSlider timeline;
	private JLabel songTime;
	private JLabel curentTime;
    
	private boolean playButt = true;
    private boolean GUIchange = false;
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
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		JPanel panelBot = new JPanel();
		((FlowLayout)panelBot.getLayout()).setVgap(0);
		((FlowLayout)panelBot.getLayout()).setHgap(0);

		JPanel panelTop = new JPanel();
		((FlowLayout)panelTop.getLayout()).setVgap(0);
		((FlowLayout)panelTop.getLayout()).setHgap(0);
		panelTop.setLayout(new BoxLayout(panelTop, BoxLayout.Y_AXIS));

		JPanel timelinePanel = new JPanel();
		((FlowLayout)timelinePanel.getLayout()).setVgap(0);
		((FlowLayout)timelinePanel.getLayout()).setHgap(0);


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
		timeline.addChangeListener(new SlideListener());
		
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

	//Slider labels
		curentTime = new JLabel("0:00");
		songTime = new JLabel("0:00");

	//Previous
		previousButton = new JButton (new ImageIcon("previous.png"));
		previousButton.setBorder(null);
		previousButton.setOpaque(false);
		previousButton.setContentAreaFilled(false);
		previousButton.setBorderPainted(false);
		previousButton.addMouseListener(new MouseAdapter(){

			public void mousePressed(MouseEvent e){
				previousButton.setIcon(new ImageIcon("pressedprevious.png"));
			}


			public void mouseClicked(MouseEvent e){
				try{
					previousButton.setIcon(new ImageIcon("previous.png"));
					manager.previous();
				} catch(Exception b) {System.out.println("next failed");}
			}
		});

	//toggleShuffle
		toggleShuffle = new JButton (new ImageIcon("shufflelila.png"));
		toggleShuffle.setBorder(null);
		toggleShuffle.setOpaque(false);
		toggleShuffle.setContentAreaFilled(false);
		toggleShuffle.setBorderPainted(false);
		toggleShuffle.addMouseListener(new MouseAdapter(){

			public void mousePressed(MouseEvent e){
				if(shuffelBool){
					toggleShuffle.setIcon(new ImageIcon("pressedshufflelila.png"));
				} else {
					toggleShuffle.setIcon(new ImageIcon("pressedshufflesvart.png"));
				}
			}

			public void mouseClicked(MouseEvent e){
				try{
					if(shuffelBool){
						toggleShuffle.setIcon(new ImageIcon("shufflesvart.png"));
						manager.setShuffle(!manager.shuffleIsOn());
						shuffelBool = false;
					} else {
						toggleShuffle.setIcon(new ImageIcon("shufflelila.png"));
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
		panelBot.add(previousButton);
		panelBot.add(playButton);
		panelBot.add(nextButton);
		panelBot.add(toggleShuffle);

		timelinePanel.add(curentTime);
		timelinePanel.add(timeline);
		timelinePanel.add(songTime);

		panelTop.add(songList);
		panelTop.add(timelinePanel);


		
		
		mainPanel.add(panelTop);
		mainPanel.add(panelBot);

		
		mainPanel.setLayout(new BoxLayout(mainPanel,BoxLayout.Y_AXIS));
		frame.add(mainPanel);

		frame.setLocationRelativeTo(null);
		frame.pack();
		frame.setVisible(true);

	}

	public void updateTimeline(){

		try{
		    GUIchange = true; 
			timeline.setValue(manager.getPosition());
			curentTime.setText(Song.secondsToString(manager.getPosition()));
			songTime.setText(manager.getCurrentSong().getDurationString());
			timeline.setMaximum(manager.getCurrentSong().getDuration());
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