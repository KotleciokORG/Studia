/*
Kacper Jodlowski
Lista 2
Zadanie 2
*/

using System;


class Element{
    public int value;
    public int id;
    public Element next,prev;

    public Element(int v=0,int ind=0){
        this.next = null;
        this.prev = null;
        this.value = v;
        this.id = ind;
    }
}

class Lista{
    private Element first;
    private Element last;

    private Element guard; //do przechodzenia po tablicy


    int absolute(int x){
        return (x<0) ? -x : x;
    }

    public int get_len(){
        return this.last.id-this.first.id+1;
    }

    public void add_element_at_the_end(Element e1){
        if(this.first == null || this.last == null){
            this.first = e1;
            this.last = e1;
            this.first.id = 0; 
            return;
        }
        //przepnij wszystko
        this.last.next = e1;
        e1.next = null;
        e1.prev = this.last;

        this.last = e1;//aktualizuj ostatniego
        this.last.id = this.last.prev.id+1;
    }
    public void add_element_at_the_begin(Element e1){
        if(this.first == null || this.last == null){
            this.first = e1;
            this.last = e1;
            this.first.id = 0; 
            return;
        }
        //przepnij wszystko
        this.first.prev = e1;
        e1.next = this.first;
        e1.prev = null;

        this.first = e1;//aktualizuj pierwszego
        this.first.id = this.first.next.id-1; //nie boli mnie posiadanie ujemnych indeksow, jestem swiadom
    }

    public void pop_element_at_the_end(){
        if(this.first == null) return;
        if(this.first == this.last){
            this.first = null;
            this.last = null;
            return;
        }

        Element temp = this.last;
        this.last = this.last.prev;
        this.last.next = null;
        temp = null;
    }
    public void pop_element_at_the_begin(){
        if(this.first == null) return;
        if(this.first == this.last){
            this.first = null;
            this.last = null;
            return;
        }
        
        Element temp = this.first;
        this.first = this.first.next;
        this.first.prev = null;

        temp = null;
    }

    public Lista(int start_ind,int end_ind,int default_values = 1){ //< , )
        this.first = null;
        this.last = null;
        for(int i=start_ind;i<end_ind;i++){
            Element e = new Element(default_values,i);
            add_element_at_the_end(e);
        }
        this.guard = this.first;
    }
    public void push_guard_forward(){
        this.guard = this.guard.next;
        if(this.guard == null){
            this.guard = this.first;
        }
    }
    public void push_guard_backward(){
        this.guard = this.guard.prev;
        if(this.guard == null){
            this.guard = this.last;
        }
    }

    public void move_guard_to(int ind){
        if(this.guard == null){
            this.guard = this.first;
        }
        if(this.absolute(this.last.id-ind)<this.absolute(this.guard.id-ind)){
            this.guard = last;
        }

        if(this.absolute(this.first.id-ind)<this.absolute(this.guard.id-ind)){
            this.guard = first;
        }

        while(this.guard.id<ind)
                this.push_guard_forward();

        while(this.guard.id>ind)
                this.push_guard_backward();
    }

    public void set(int ind,int value){
        this.move_guard_to(ind);
        this.guard.value = value;
    }
    public int get(int ind){
        this.move_guard_to(ind);
        return this.guard.value;
    }

    public void resize(int size){
        int len = this.get_len();
        if(size<len){
            for(int i=0;i<len-size;i++){
                this.pop_element_at_the_end();
            }
        }
        else{
            for(int i=0;i<size-len;i++){
                Element e = new Element();
                this.add_element_at_the_end(e);
            }
        }
    }
    public void reindex(int start_ind,int end_ind){ //idzie dopoki nie skoncza mu sie indeksy albo do konca listy
        this.guard = this.first;
        for(int i=start_ind;i<end_ind;i++){
            this.guard.id = i;
            this.push_guard_forward();
            if(this.guard == null){
                return;
            }
        }
    }
}



class MojProgram {
    public static void Main() {
        Lista a1 = new Lista(0,100);
        Lista a2 = new Lista(0,100);
        Lista a3 = new Lista(0,100);

        for (int i = 0; i < 100; i++)
            a3.set(i, a1.get(i) + a2.get(i));

        for (int i = 0; i < 100; i++)
            Console.WriteLine("Wartosc na {0} indeksie: {1}\n",i,a3.get(i));
    }
}