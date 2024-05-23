import javax.swing.*;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.*;
import java.awt.event.*;

public class Ksiazka implements ActionListener
{
  private String tytuł;
  private String autor;
  private int wydanie;
  
  public Ksiazka(String t, String a, int w)
  {
    this.tytuł = t; this.autor = a; this.wydanie = w;
  }
  
  public String toString()
  {
    return "Książka " + this.tytuł + " " + this.autor + " " + this.wydanie;
  }
  
  private void createGUI()
  {
    JFrame frame = new JFrame("Edycja książki");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    Container kontener = frame.getContentPane();
    GridLayout layout = new GridLayout(4, 2);
    // FlowLayout layout = new FlowLayout();
    kontener.setLayout(layout);
    
    JLabel autor_etykieta = new JLabel("Autor");
    kontener.add(autor_etykieta);
    JTextField autor = new JTextField(this.autor, 40);
    // autor_etykieta.setLabelFor(autor);
    kontener.add(autor);
    
    JLabel tytuł_etykieta = new JLabel("Tytuł");
    kontener.add(tytuł_etykieta);
    JTextField tytuł = new JTextField(this.tytuł, 40);
    kontener.add(tytuł);
       
    JLabel wydanie_etykieta = new JLabel("Wydanie");
    kontener.add(wydanie_etykieta);
    JTextField wydanie = new JTextField(Integer.toString(this.wydanie), 40);
    kontener.add(wydanie);
    
    JButton b = new JButton("Zapisz");
    b.addActionListener(this);
    kontener.add(b);

    frame.pack();
    frame.setVisible(true);
    
  }
  
   
  public void actionPerformed(ActionEvent e)
  {
    System.out.println("Naciśnięto przycisk");
  }
  
  public void Edycja()
  {
    this.createGUI();
  }

}
