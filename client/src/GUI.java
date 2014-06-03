import java.util.List;
import java.util.ArrayList;
import javax.swing.*;
import javax.swing.border.BevelBorder;
import javax.swing.event.*;
import java.awt.*;
import static java.awt.Color.*;
import java.awt.event.*;
import javax.swing.table.TableRowSorter;


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
private TableModel model;
private TableRowSorter<TableModel> sorter;
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
    private JLabel status;
private JLabel currentSong;
Object[][] data;
private List<Song> songList;
private boolean playButt = true;
private boolean GUIchange = false;
private boolean shuffelBool = true;
private boolean repeatBool = true;
private static int sortMode = 0;


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
    panelBot.setBorder(BorderFactory.createEmptyBorder(0, 0, 10, 0));

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

    JPanel statusPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
    ((FlowLayout)statusPanel.getLayout()).setVgap(0);
    ((FlowLayout)statusPanel.getLayout()).setHgap(0);
    statusPanel.setBorder(new BevelBorder(BevelBorder.LOWERED));

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


playing = new JLabel("Playing: ", SwingConstants.CENTER);
currentSong = new JLabel("");
status = new JLabel("Connected.");


status = new JLabel("Connected to " + manager.getAddress() + ":" + manager.getPort() + ".");

Font font = new Font("Verdana", Font.PLAIN, 12);
font.deriveFont(font.getStyle() & ~Font.BOLD);
status.setFont(font);


       //songList

    songList = manager.getSongs();
    data = new Object[songList.size()][songList.size()];
    for(int i = 0; i < songList.size() ; i++){
        String title = songList.get(i).getTitle();
        String artist = songList.get(i).getArtist();
        String album = songList.get(i).getAlbum();
        String duration = songList.get(i).getDurationString();
        Object[] listElement = {title,artist,album,duration};
        data[i] = listElement;
    }
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
songTable.setAutoCreateRowSorter(true);

songTable.getTableHeader().addMouseListener(new MouseAdapter(){
    @Override
    public void mouseClicked(MouseEvent e){
        int col = songTable.columnAtPoint(e.getPoint());
        String colName = songTable.getColumnName(col);
        System.out.println(colName +": "+ Integer.toString(sortMode));
            switch(colName){
                case "Title":   
                    switch(sortMode) {
                        case 0:
                            manager.sort(1);
                            sortMode = 1;
                            break;
                        default:
                            manager.sort(0);
                            sortMode = 0;
                            System.out.println("0");
                            break;
                    }
                    break;
                case "Artist":
                    switch(sortMode) {
                        case 2:
                            sortMode = 3;
                            manager.sort(3);                            
                            break;
                        default:
                            System.out.println("2");
                            sortMode = 2;       
                            manager.sort(2);                                     
                            break;
                    }
                    break;
                case "Album":
                    switch(sortMode) {
                        case 4:
                            sortMode = 5;
                            manager.sort(5);
                            break;
                        default:
                            sortMode = 4;  
                            manager.sort(4);
                            System.out.println("4");
                            break;
                    }
                    break;
                case "Duration":
                    switch(sortMode) {
                        case 6:
                            sortMode = 7;
                            manager.sort(7);
                            System.out.println("7");
                            break;
                        default:
                            sortMode = 6;   
                            manager.sort(6);
                            System.out.println("6");
                            break;
                    }
                    break;
                default:
                    //sortMode = 0;
                    //manager.sort(0);
                    break;
            }
            System.out.println(Integer.toString(sortMode));
        }
    
    });

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

statusPanel.add(status);

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
mainPanel.add(statusPanel);
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

    private void updateSongTable(List<Song> songs) {
	Song song;

	data = new Object[songs.size()][songs.size()];
	for(int i = 0; i < songs.size(); i++) {
	    song = songs.get(i);
	    Object[] element = {song.getTitle(), song.getArtist(), song.getAlbum(), song.getDurationString()};
	    data[i] = element;
	}

	((TableModel) songTable.getModel()).fill(data);
    }

    public void songsUpdated(List<Song> newSongs) {
	updateSongTable(newSongs);
    }

    public void serverShutdown() {
	status.setText("Server was shut down! Retrying...");
	status.setForeground(Color.RED);
    }

    public void connectionLost() {
	status.setText("Lost connection to server! Retrying...");
	status.setForeground(Color.RED);
    }

    public void connectionRegained(List<Song> songs) {
	updateSongTable(songs);
	status.setText("Connected to " + manager.getAddress() + ":" + manager.getPort() + ".");
	status.setForeground(null);
    }
}
