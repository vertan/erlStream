import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import static java.awt.Color.*;
import java.awt.event.*;


public class GUI extends UI {

    private class Update implements Runnable{
            @Override
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
        @Override
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
    private JTable songTable;
    private JSlider timeline;
    private JLabel songTime;
    private JLabel curentTime;
    Object[][] data;

    private boolean playButt = true;
    private boolean GUIchange = false;
    private boolean shuffelBool = true;



    public GUI(){
            try{
                    manager = new AudioManager("bitninja.se",1340);
            } catch(Exception e){System.out.println("AudioManager failed!!!");};
    }

    @Override
    public void start() {
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

        playButton = new JButton (new ImageIcon("play.png"));
        //playButton = new JButton (new ImageIcon(this.getClass().getClassLoader().getResource("play.png")));
        playButton.setBorder(null);
        playButton.setOpaque(false);
        playButton.setContentAreaFilled(false);
        playButton.setBorderPainted(false);
        playButton.addMouseListener(new MouseAdapter(){
            @Override
            public void mousePressed(MouseEvent e){
                if(playButt){
                    playButton.setIcon(new ImageIcon("pressedPlay.png"));
                    //playButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedPlay.png")));
                } else {
                    playButton.setIcon(new ImageIcon("pressedPause.png"));
                    //playButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedPause.png")));
                }
            }
            @Override
            public void mouseClicked(MouseEvent e){
                try{
                    if(playButt){
                        playButton.setIcon(new ImageIcon("pause.png"));
                        //playButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pause.png")));
                        playButt = false;
                        manager.play();
                    } else {
                        playButton.setIcon(new ImageIcon("play.png"));
                            //playButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("play.png")));	
                            playButt = true;
                            manager.pause();
                    }
                } catch(Exception a){System.out.println("Failed!!!");}			    
            }
        });

        //Slider
        timeline = new JSlider(JSlider.HORIZONTAL,0,100,0);
        timeline.addChangeListener(new SlideListener());

        nextButton = new JButton (new ImageIcon("next.png"));
        //nextButton = new JButton (new ImageIcon(this.getClass().getClassLoader().getResource("next.png")));
        nextButton.setBorder(null);
        nextButton.setOpaque(false);
        nextButton.setContentAreaFilled(false);
        nextButton.setBorderPainted(false);
        nextButton.addMouseListener(new MouseAdapter(){
            @Override
            public void mousePressed(MouseEvent e){
                nextButton.setIcon(new ImageIcon("pressedNext.png"));    
                //nextButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedNext.png")));
            }
            @Override
            public void mouseClicked(MouseEvent e){
                try{
                    nextButton.setIcon(new ImageIcon("next.png")); 
                    //nextButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("next.png")));
                    manager.next();
                } catch(Exception b) {System.out.println("next failed");}
            }
        });

        //Slider labels
        curentTime = new JLabel("0:00");
        songTime = new JLabel("0:00");

        previousButton = new JButton(new ImageIcon("previous.png"));
        //previousButton = new JButton (new ImageIcon(this.getClass().getClassLoader().getResource("previous.png")));
        previousButton.setBorder(null);
        previousButton.setOpaque(false);
        previousButton.setContentAreaFilled(false);
        previousButton.setBorderPainted(false);
        previousButton.addMouseListener(new MouseAdapter(){
            @Override
            public void mousePressed(MouseEvent e){
                previousButton.setIcon(new ImageIcon("pressedprevious.png"));
                //previousButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedprevious.png")));
            }
            @Override
            public void mouseClicked(MouseEvent e){
                try{
                    previousButton.setIcon(new ImageIcon("previous.png"));
                    //previousButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("previous.png")));
                    manager.previous();
                } catch(Exception b) {System.out.println("next failed");}
            }
        });

        //toggleShuffle
        toggleShuffle = new JButton(new ImageIcon("shufflelila.png"));
        //toggleShuffle = new JButton (new ImageIcon(this.getClass().getClassLoader().getResource("shufflelila.png")));
        toggleShuffle.setBorder(null);
        toggleShuffle.setOpaque(false);
        toggleShuffle.setContentAreaFilled(false);
        toggleShuffle.setBorderPainted(false);
        toggleShuffle.addMouseListener(new MouseAdapter(){
            @Override
            public void mousePressed(MouseEvent e){
                if(shuffelBool){
                    toggleShuffle.setIcon(new ImageIcon("pressedshufflelila.png"));
                    //toggleShuffle.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedshufflelila.png")));
                } else {
                    toggleShuffle.setIcon(new ImageIcon("pressedshufflesvart.png"));
                    //toggleShuffle.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pressedshufflesvart.png.png")));
                }
            }
            @Override
            public void mouseClicked(MouseEvent e){
                try{
                    if(shuffelBool){
                        toggleShuffle.setIcon(new ImageIcon("shufflesvart.png"));
                        //toggleShuffle.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("shufflesvart.png")));
                        manager.setShuffle(!manager.shuffleIsOn());
                        shuffelBool = false;
                    } else {
                        toggleShuffle.setIcon(new ImageIcon("shufflelila.png"));
                        //toggleShuffle.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("shufflelila.png")));
                        manager.setShuffle(!manager.shuffleIsOn());
                        shuffelBool = true;
                    }
                } catch(Exception b) {System.out.println("next failed");}
            }
        });



       //songList
        try {
            data = new Object[3][manager.getSongs().size()];
        } catch (Exception ex) {}

        try {
            for(int i = 0; i < manager.getSongs().size() ; i++){
                String title = manager.getSongs().get(i).getTitle();
                String artist = manager.getSongs().get(i).getArtist();
                String album = manager.getSongs().get(i).getAlbum();
                String duration = manager.getSongs().get(i).getDurationString();
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
                        playButton.setIcon(new ImageIcon("pause.png"));
                        //playButton.setIcon(new ImageIcon(this.getClass().getClassLoader().getResource("pause.png")));
                        target.clearSelection();
                    } catch (Exception ex) {System.out.println("List Click failed!");}
                }
            }
        });        

//Add everything

        panelBot.add(previousButton);
        panelBot.add(playButton);
        panelBot.add(nextButton);
        panelBot.add(toggleShuffle);

        timelinePanel.add(curentTime);
        timelinePanel.add(timeline);
        timelinePanel.add(songTime);

        JScrollPane scrollPane = new JScrollPane(songTable);
        songTable.setFillsViewportHeight(true);
        scrollPane.setPreferredSize(new Dimension(songTable.getPreferredSize().width,400));    
        scrollPane.setBorder(BorderFactory.createEmptyBorder(5, 10, 15, 10));
        scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);

        panelTop.add(scrollPane,BorderLayout.CENTER);
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

	public void updateTimeline(){
            try{
                GUIchange = true; 
                timeline.setValue(manager.getPosition());
                curentTime.setText(Song.secondsToString(manager.getPosition()));
                songTime.setText(manager.getCurrentSong().getDurationString());
                timeline.setMaximum(manager.getCurrentSong().getDuration());
            }catch(Exception c) {}
	}
}