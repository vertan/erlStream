import java.util.List;
import java.util.ArrayList;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import static java.awt.Color.*;
import java.awt.event.*;


public class GUI implements UI, StatusListener {

    private class Update implements Runnable{
        @Override
        public void run(){
            while(true){
                updateAll();
                GUIchange = false;
                try{
                    Thread.sleep(500);
                } catch(InterruptedException e){}

            }
        }
    }


    private class PopUp {
        private JButton okButton;
        private JTextField inAdress;
        private JTextField inPort;

        JFrame popFrame;

        private JLabel inAdressLabel;
        private JLabel inPortLabel;
        private JLabel faildLabel;

        private int port;
        private String adress;

        public void init() throws Exception{

            popFrame = new JFrame("erlStream");
            popFrame.setResizable(false);
            popFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

            JPanel popMain = new JPanel();
            JPanel adressPanel = new JPanel();
            JPanel portPanel = new JPanel();
            JPanel buttonPanel = new JPanel();

            okButton = new JButton("Ok!");
            inAdress = new JTextField("localhost",7);
            inPort = new JTextField("1340",7);

            inPortLabel = new JLabel("Port:");
            inAdressLabel = new JLabel("Address:");
            faildLabel = new JLabel("Choose the server to connect to!");
            faildLabel.setBorder(BorderFactory.createEmptyBorder(5, 10, 0, 10));

            popMain.add(faildLabel);
            adressPanel.add(inAdressLabel);
            adressPanel.add(inAdress);
            popMain.add(adressPanel);
            portPanel.add(inPortLabel);
            portPanel.add(inPort);
            popMain.add(portPanel);
            buttonPanel.add(okButton);
            popMain.add(buttonPanel);
            popMain.setLayout(new BoxLayout(popMain,BoxLayout.Y_AXIS));

            popFrame.add(popMain);
            popFrame.setLocationRelativeTo(null);
            popFrame.pack();
            popFrame.setVisible(true);

            okButton.addMouseListener(new MouseAdapter(){
                @Override
                public void mouseClicked(MouseEvent e){
                   adress = inAdress.getText();
                   port = Integer.parseInt(inPort.getText());
                   try {
                    manager = new AudioManager(adress, port);
		    manager.addStatusListener(GUI.this);
                    launchGUI();
                    popFrame.setVisible(false);
                    popFrame.dispose();
                } catch(Exception ex) {
                    faildLabel.setText("Connection failed, please retry!");
                    faildLabel.setForeground(Color.RED);
                }
            }
        });
        }
    }

    private class SlideListener implements ChangeListener{
        @Override
        public void stateChanged(ChangeEvent event){
           JSlider source = (JSlider)event.getSource();
           if(!source.getValueIsAdjusting() && !GUIchange){
              try{
                  manager.play(manager.getCurrentSong(),source.getValue()*1000);
              }catch(Exception e){
                System.out.println("Failed!");
            }
        }
    }	
}

private AudioManager manager;

private JButton playButton;
private JButton nextButton;
private JButton previousButton;
private JButton toggleShuffle;
private JButton toggleRepeat;
private JTable songTable;
private JSlider timeline;
private JLabel songTime;
private JLabel curentTime;
private JLabel playing;
private JLabel currentSong;
Object[][] data;
private List<Song> songList;
private boolean playButt = true;
private boolean GUIchange = false;
private boolean shuffelBool = true;
private boolean repeatBool = true;



public GUI(){
}

@Override
public void start() {
    try{
        new PopUp().init();
    } catch(Exception e){System.out.println("AudioManager failed!!!");};
}

public void launchGUI() {
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

    JPanel nowPlaying = new JPanel();
    ((FlowLayout)nowPlaying.getLayout()).setVgap(0);
    ((FlowLayout)nowPlaying.getLayout()).setHgap(0);



    Thread update = new Thread(new Update());
    update.start();

    playButton = new JButton (new ImageIcon(this.getClass().getClassLoader().getResource("play.png")));
    playButton.setBorder(null);
    playButton.setOpaque(false);
    playButton.setContentAreaFilled(false);
    playButton.setBorderPainted(false);
    playButton.addMouseListener(new MouseAdapter(){
        @Override
        public void mousePressed(MouseEvent e){
            if(playButt){
		playButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedPlay.png")));
            } else {
		playButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedPause.png")));
            }
        }
        @Override
        public void mouseClicked(MouseEvent e){
            try{
                if(playButt){
		    playButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pause.png")));
                    playButt = false;
                    manager.play();
                } else {
		    playButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("play.png")));	
                    playButt = true;
                    manager.pause();
                }
            } catch(Exception a){System.out.println("Failed!!!");}			    
        }
    });

        //Slider
timeline = new JSlider(JSlider.HORIZONTAL,0,100,0);
timeline.setPreferredSize(new Dimension(450,20));
timeline.addChangeListener(new SlideListener());

nextButton = new JButton (new ImageIcon(this.getClass().getClassLoader().getResource("next.png")));
nextButton.setBorder(null);
nextButton.setOpaque(false);
nextButton.setContentAreaFilled(false);
nextButton.setBorderPainted(false);
nextButton.addMouseListener(new MouseAdapter(){
    @Override
    public void mousePressed(MouseEvent e){
	nextButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedNext.png")));
    }
    @Override
    public void mouseClicked(MouseEvent e){
        try{
	    nextButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("next.png")));
            manager.next();
        } catch(Exception b) {System.out.println("next failed");}
    }
});

        //Slider labels
curentTime = new JLabel("0:00");
songTime = new JLabel("0:00");

previousButton = new JButton (new ImageIcon(this.getClass().getClassLoader().getResource("previous.png")));
previousButton.setBorder(null);
previousButton.setOpaque(false);
previousButton.setContentAreaFilled(false);
previousButton.setBorderPainted(false);
previousButton.addMouseListener(new MouseAdapter(){
    @Override
    public void mousePressed(MouseEvent e){
	previousButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedPrevious.png")));
    }
    @Override
    public void mouseClicked(MouseEvent e){
        try{
	    previousButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("previous.png")));
            manager.previous();
        } catch(Exception b) {System.out.println("next failed");}
    }
});

        //toggleShuffle
toggleShuffle = new JButton (new ImageIcon(this.getClass().getClassLoader().getResource("shuffleSvart.png")));
toggleShuffle.setBorder(null);
toggleShuffle.setOpaque(false);
toggleShuffle.setContentAreaFilled(false);
toggleShuffle.setBorderPainted(false);
toggleShuffle.addMouseListener(new MouseAdapter(){
    @Override
    public void mouseClicked(MouseEvent e){
        try{
            if(shuffelBool){
		toggleShuffle.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("shuffleLila.png")));
                manager.setShuffle(!manager.shuffleIsOn());
                shuffelBool = false;
            } else {
		toggleShuffle.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("shuffleSvart.png")));
                manager.setShuffle(!manager.shuffleIsOn());
                shuffelBool = true;
            }
        } catch(Exception b) {System.out.println("next failed");}
    }
});

        //toggleRepeat
toggleRepeat = new JButton((new ImageIcon(this.getClass().getClassLoader().getResource("pressedRepeat.png"))));
toggleRepeat.setBorder(null);
toggleRepeat.setOpaque(false);
toggleRepeat.setContentAreaFilled(false);
toggleRepeat.setBorderPainted(false);
toggleRepeat.addMouseListener(new MouseAdapter(){
    @Override
    public void mouseClicked(MouseEvent e){
        try{
            if(repeatBool){
		toggleRepeat.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("repeatBlack.png")));
                manager.setRepeat(!manager.repeatIsOn());
                repeatBool = false;
            } else {
		toggleRepeat.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedRepeat.png")));
                manager.setRepeat(!manager.repeatIsOn());
                repeatBool = true;
            }
        } catch(Exception b) {System.out.println("next failed");}
    }
});

        //playing Labels
playing = new JLabel("Playing: ", SwingConstants.CENTER);
        //playing.setBorder(BorderFactory.createLineBorder(BLACK,1));
currentSong = new JLabel("");

       //songList
try {
    songList = manager.getSongs();
    data = new Object[songList.size()][songList.size()];
} catch (Exception ex) {}

try {
    for(int i = 0; i < songList.size() ; i++){
        String title = songList.get(i).getTitle();
        String artist = songList.get(i).getArtist();
        String album = songList.get(i).getAlbum();
        String duration = songList.get(i).getDurationString();
        Object[] listElement = {title,artist,album,duration};
        data[i] = listElement;
    }
} catch (Exception ex) {System.out.println("Loading song failed");}

songTable = new JTable(new TableModel(data));

songTable.setSelectionBackground(MAGENTA);
songTable.setBorder(BorderFactory.createLineBorder(BLACK, 1));
songTable.setIntercellSpacing(new Dimension(0, 0));
songTable.setShowGrid(false);
songTable.setCellSelectionEnabled(false);
songTable.setColumnSelectionAllowed(false);
songTable.setRowSelectionAllowed(true);
songTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
songTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
songTable.getColumnModel().getColumn(0).setPreferredWidth(103);
songTable.getColumnModel().getColumn(1).setPreferredWidth(180);
songTable.getColumnModel().getColumn(2).setPreferredWidth(150);
songTable.getColumnModel().getColumn(3).setPreferredWidth(60);
songTable.setFocusable(false);

songTable.addMouseListener(new MouseAdapter() {
    @Override
    public void mousePressed(MouseEvent e) {
        JTable target = (JTable)e.getSource();
        Point p = e.getPoint();
        if (target.getSelectedRow() == -1 || target.rowAtPoint(p) != target.getSelectedRow()){
           target.clearSelection();                           
       }
       if (e.getClickCount() == 2) {
        Integer row = target.getSelectedRow();
        Object title = target.getValueAt(row,0);
        try {
            manager.playSongByTitle(title.toString(),0);
            playButt = false;
	    playButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pause.png")));
            target.clearSelection();
        } catch (Exception ex) {System.out.println("List Click failed!");}
    }
}
});        

//Add everything

panelBot.add(toggleRepeat);
panelBot.add(previousButton);
panelBot.add(playButton);
panelBot.add(nextButton);
panelBot.add(toggleShuffle);

timelinePanel.add(curentTime);
timelinePanel.add(timeline);
timelinePanel.add(songTime);

nowPlaying.add(playing);
nowPlaying.add(currentSong);

JScrollPane scrollPane = new JScrollPane(songTable);
songTable.setFillsViewportHeight(true);
scrollPane.setPreferredSize(new Dimension(songTable.getPreferredSize().width,400));    
scrollPane.setBorder(BorderFactory.createEmptyBorder(5, 10, 15, 10));
scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);

panelTop.add(scrollPane,BorderLayout.CENTER);
panelTop.add(nowPlaying);
panelTop.add(timelinePanel);    
panelTop.setPreferredSize(new Dimension(530,450));


mainPanel.add(panelTop);
mainPanel.add(panelBot);
mainPanel.setLayout(new BoxLayout(mainPanel,BoxLayout.Y_AXIS));

frame.add(mainPanel);   
frame.setLocationRelativeTo(null);
frame.pack();
frame.setVisible(true);

}

public void updateAll(){
    try{
        GUIchange = true; 
        timeline.setValue(manager.getPosition());
        curentTime.setText(Song.secondsToString(manager.getPosition()));
        songTime.setText(manager.getCurrentSong().getDurationString());
        timeline.setMaximum(manager.getCurrentSong().getDuration());

        currentSong.setText(manager.getCurrentSong().toString());

    }catch(Exception c) {}
}
    
    public void songsUpdated(List<Song> newSongs) {
	System.out.println("New songlist available!");
    }

    public void serverShutdown() {
	System.out.println("Server was shut down! Retrying connection...");
    }

    public void connectionLost() {
	System.out.println("Lost connection to server! Retrying...");
    }

    public void connectionRegained(List<Song> songs) {
	System.out.println("Connection regained!");
    }
}
