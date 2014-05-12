import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class GUI extends UI implements ActionListener{

	private AudioManager manager;

	private String adress;
	private int inPort, outPort;

	private JButton playButton;

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

		playButton = new JButton (new ImageIcon("play.jpg"));
		playButton.setBorder(null);
		playButton.addActionListener(this);
		//b.addActionListener(new ActionListener(){
		//	public void actionPreformed(ActionEvent e){
		//		manager.play();
		//	}
		//});
		panelLeft.add(playButton);

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
					playButt = false;
					manager.play();
				} else {
					playButt = true;
					manager.pause();
				}
			} catch(Exception a){System.out.println("Failed!!!");};
		}
	}



}