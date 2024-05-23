/*
Kacper Jodlowski
Lista 3
Zadanie 1
*/

using System;
using System.Reflection;

class Element<T>{

    public T value;
    public Element<T> next;
    public Element<T> prev;

    public Element(T val){
        this.value = val;
        this.next = null;
        this.prev = null;
    }
}

class Lista<T>{
    private Element<T> first;
    private Element<T> last;
    
    private void bind(Element<T> E1, Element<T> E2){
        E1.next = E2;
        E2.prev = E1;
    }

    public Lista(){
        this.first = null;
        this.last = null;
    }
    
    public bool is_empty(){
        return this.first==null && this.last==null;
    }
    
    public void push_front(T elem){
        Element<T> E = new Element<T>(elem);
        if (this.is_empty() == true){
            this.first = E;
            this.last = E;
            return;
        }

        this.bind(E,this.first);
        this.first = E;
    }
    
    public void push_back(T elem){
        Element<T> E = new Element<T>(elem);
        if (this.is_empty() == true){
            this.first = E;
            this.last = E;
            return;
        }

        this.bind(this.last,E);
        this.last = E;
    }

    public T pop_front(){
        if (this.is_empty() == true){
            throw new ArgumentException("Parameter cannot be empty");
        }
        Element<T> temp = this.first;
        if(this.last == this.first){
            this.first = null;
            this.last = null;
        }
        else{
            this.first = this.first.next;
            this.first.prev = null;
        }
        return temp.value;

    }
    public T pop_back(){
        if (this.is_empty() == true){
            throw new ArgumentException("Parameter cannot be empty");
        }
        Element<T> temp = this.last;
        if(this.last == this.first){
            this.first = null;
            this.last = null;
        }
        else{
            this.last = this.last.prev;
            this.last.next = null;
        }
        return temp.value;

    }
    public void print_list(){
        for(Element<T> e = this.first; e!= this.last; e = e.next){
            Console.WriteLine("Wartosc: {0}\n",e.value);
        }
        Console.WriteLine("Wartosc: {0}\n",this.last.value);
    }

    
}
