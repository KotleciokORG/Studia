/*
Kacper Jodlowski
Lista 6
Zadanie 3
*/

import java.util.concurrent.*;
import java.util.Random;


class Bufor<T> {
    private ConcurrentLinkedQueue<T> q;
    int size;
    public Bufor (int size){
        this.size = size;
        this.q = new ConcurrentLinkedQueue<T>();
    }
    public synchronized void add (T elem) throws InterruptedException{
        if(q.size() < size) {
            q.add(elem);
        }
        else{
            wait();
        }
    }
    public synchronized boolean is_empty(){
        return q.isEmpty();
    }
    public synchronized T get(){
        return q.poll();
        
    }
}

class Producent implements Runnable{
    private String napis = "asfdgdfdhjktydfvasdsdhjkouertbcnchgfh";
    Bufor<String> B;
    private int tick = 0;
    Random rand = new Random();
    public Producent(Bufor<String> Buf){
        this.B = Buf;
    }
    public synchronized void run() {
        for(int i = 0; i<10; i++){

            int pause = rand.nextInt(10000);
            
            while(tick<pause){
                tick++;
            }
            tick = 0;
            int a = rand.nextInt(napis.length()-1);
            int b = rand.nextInt(a,napis.length()-1);
            String nowy = napis.substring(a,b);
            try{
                this.B.add(nowy);
                System.err.println("Wstawiony: " + nowy);
                Thread.sleep(500);
            }
            catch (InterruptedException e){
                System.err.println(e);
            }
        }
        
    }
}

class Consumer implements Runnable{
    Bufor<String> B;
    private int tick = 0;
    Random rand = new Random();
    public Consumer(Bufor<String> Buf){
        this.B = Buf;
    }
    public synchronized void run() {
        for(int i = 0; i<10; i++){
            int pause = rand.nextInt(10000);

            while(tick<pause){
                tick++;
            }
            tick = 0;
            
        
            if( ! B.is_empty())
            {
                System.err.println("Zjadlem: " + B.get());
                
                try{
                    Thread.sleep(750);
                }
                catch (InterruptedException e){
                    System.err.println(e);
                }
            }

            
            synchronized (B){
                B.notify();
            }
        }

        
        
    }
}



public class Zad3{
    public static void main(String[] args){
        
        Bufor<String> Bufik = new Bufor<String>(10);

        Producent prod = new Producent(Bufik);
        Consumer cons = new Consumer(Bufik);

        Thread first = new Thread(prod);
        Thread second = new Thread(cons);

        first.start();
        second.start();
    }

}