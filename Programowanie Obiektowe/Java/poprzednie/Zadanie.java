
/*
Kacper Jodlowski
Lista 7
Zadanie 1
Nie dziala wyswietlanie JPanel, i niestety nie udalo sie tego zdebugowac, reszta wyglada dzialac
*/



import javax.swing.*;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.JOptionPane;

public class Zadanie {
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Person Editor");

            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

            PtakPanel ptakEditor = new PtakPanel();
            
            frame.getContentPane().setLayout(new BorderLayout());
            frame.add(ptakEditor, BorderLayout.CENTER);
            System.out.println("PtakPanel added to JFrame");
            
            JButton saveButton = new JButton("Save");
            saveButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    String gatunek = ptakEditor.getGatunek();
                    String zjada = ptakEditor.getZjada();
                    String odglos = ptakEditor.getOdglos();
                    int jaja = ptakEditor.getJaj();
                    int height = ptakEditor.getHeight();
                    int skrzydla = ptakEditor.getSkrzydla();
                    
                    Ptak ptak = new Ptak(gatunek, zjada, odglos, jaja, height, skrzydla);
                    JOptionPane.showMessageDialog(frame, ptak.say_Hello());

                }
            });

            frame.getContentPane().add(saveButton, BorderLayout.SOUTH);

            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    

    }
}

class Zwierze {
    String gatunek;
    String zjada;
    String odglos;

    public Zwierze(String gatunek, String zjada, String odglos){
        this.gatunek = gatunek ;
        this.zjada = zjada ;
        this.odglos = odglos ;
    }

    String say_Hello() {
        String ret = "To jest %s, zjada %s oraz mowi %s\n";
        return String.format(ret, gatunek, zjada, odglos);
    }

}

class Ssak extends Zwierze {
    int dlugosc_ciazy;
    int max_speed;
    int ilu_nozny;

    public Ssak(String gatunek, String zjada, String odglos,int dlugosc_ciazy, int max_speed, int ilu_nozny){
        super(gatunek,zjada,odglos);
        this.dlugosc_ciazy = dlugosc_ciazy;
        this.max_speed = max_speed;
        this.ilu_nozny = ilu_nozny;
    }
    String say_Hello(){
        String ret =  super.say_Hello();
        ret += "Dodatkowo ciaza trwa %d, rozwija predkosc %d chodzac na %d nogach\n";
        return String.format(ret, dlugosc_ciazy, max_speed, ilu_nozny);
    }
}

class Ptak extends Zwierze{
    int ile_jaj;
    int max_wysokosc;
    int rozpietosc_skrzydel;

    public Ptak(String gatunek, String zjada, String odglos,int ile_jaj, int max_wysokosc, int rozpietosc_skrzydel){
        super(gatunek,zjada,odglos);
        this.ile_jaj = ile_jaj;
        this.max_wysokosc = max_wysokosc;
        this.rozpietosc_skrzydel = rozpietosc_skrzydel;
    }
    String say_Hello(){
        String ret =  super.say_Hello();
        ret += "Dodatkowo sklada %d jaj, lata na wysokosci %d metrow posiadajac %d m rozpietosci skrzydel\n";
        return String.format(ret, ile_jaj, max_wysokosc, rozpietosc_skrzydel);
    }
}

class PtakPanel extends JPanel {
    private JTextField gatunekField;
    private JComboBox<String> zjadaCombo;
    private JTextField odglosField;
    private JSpinner ile_jaj_JSpinner;
    private JSpinner max_wysokosc_JSpinner;
    private JSpinner rozpietosc_skrzydel_JSpinner;

    private Ptak ptak;

    public PtakPanel(){
        System.out.println("PtakPanel constructor called");


        setLayout(new GridLayout(6, 2));

        JLabel nameLabel = new JLabel("Gatunek:");
        gatunekField = new JTextField();
        add(nameLabel);
        add(gatunekField);

        JLabel zjadaLabel = new JLabel("Pozera:");
        String[] genders = {"Rosliny", "Zwierzeta", "Wszystko", "Nic"};
        zjadaCombo = new JComboBox<>(genders);
        zjadaCombo.setSelectedItem("Nic");
        add(zjadaLabel);
        add(zjadaCombo);

        JLabel odglosLabel = new JLabel("Odglos (onomatopeja):");
        odglosField = new JTextField();
        add(odglosLabel);
        add(odglosField);

        JLabel jajaLabel = new JLabel("Jaja:");
        ile_jaj_JSpinner = new JSpinner(new SpinnerNumberModel(0, 0, 20, 1));
        add(jajaLabel);
        add(ile_jaj_JSpinner);

        JLabel heightJLabel = new JLabel("Wysokosc lotu:");
        max_wysokosc_JSpinner = new JSpinner(new SpinnerNumberModel(0, 0, 5000, 100));
        add(heightJLabel);
        add(max_wysokosc_JSpinner);

        JLabel skrzydlaJLabel = new JLabel("Skrzydlo:");
        rozpietosc_skrzydel_JSpinner = new JSpinner(new SpinnerNumberModel(0, 0, 5000, 100));
        add(skrzydlaJLabel);
        add(rozpietosc_skrzydel_JSpinner);

        gatunekField.setVisible(true);
        zjadaCombo.setVisible(true);
        odglosField.setVisible(true);
        ile_jaj_JSpinner.setVisible(true);
        max_wysokosc_JSpinner.setVisible(true);
        rozpietosc_skrzydel_JSpinner.setVisible(true);

    }


    public String getGatunek() {
        return gatunekField.getText();
    }
    public String getZjada() {
        return (String) zjadaCombo.getSelectedItem();
    }
    public String getOdglos() {
        return odglosField.getText();
    }
    public int getJaj() {
        return (int) ile_jaj_JSpinner.getValue();
    }
    public int getHeight() {
        return (int) max_wysokosc_JSpinner.getValue();
    }
    public int getSkrzydla() {
        return (int) rozpietosc_skrzydel_JSpinner.getValue();
    }


    
    

}