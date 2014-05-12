import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class GUI extends UI implements ActionListener{

	private AudioManager manager;

	private String adress;
	private int inPort, outPort;

	private JButton playButton;
	private JButton nextButton;
	private JList songList;

	private boolean playButt = true;


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

		//playButton
		playButton = new JButton (new ImageIcon("play.png"));
		playButton.setBorder(null);
		playButton.addActionListener(this);
		playButton.setOpaque(false);
		playButton.setContentAreaFilled(false);
		playButton.setBorderPainted(false);

		//NextButton
		nextButton = new JButton (new ImageIcon("pressedPlay.png"));
		nextButton.setBorder(null);
		nextButton.addActionListener(this);
		nextButton.setOpaque(false);
		nextButton.setContentAreaFilled(false);
		nextButton.setBorderPainted(false);

		//songList
		try{
		songList = new JList(manager.getSongs().toArray());
		} catch (Exception a){System.out.println("Song list failed");};
		//Add everything
		panelLeft.add(playButton);
		panelLeft.add(nextButton);

		panelRigth.add(songList);

		mainPanel.add(panelLeft);
		mainPanel.add(panelRigth);

		frame.add(mainPanel);

		frame.setLocationRelativeTo(null);
		//frame.pack();
		frame.setVisible(true);

	}

	public void actionPerformed(ActionEvent e) {
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



}