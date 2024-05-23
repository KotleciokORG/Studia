/*
Kacper Jodlowski
Lista 3
Zadanie 2
*/

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using System.Security.Cryptography;

class Element<K,V>{
    public K key;
    public V value;
    public Element<K,V> next;
    public Element(K k, V v){
        this.key = k;
        this.value = v;
    }
}

class MyDictionary<K,V>{
    private Element<K,V> first;
    public MyDictionary(){
        this.first = null;
    } 
    public void add_elem(K key, V value){
        Element<K,V> E = new Element<K,V>(key, value);
        if(this.first == null){
            this.first = E;
            return;
        }
        Element<K,V> temp = new Element<K,V>(key, value); //it should be changed to the last element
        for(Element<K,V> i = this.first; i != null; i = i.next){
            if(EqualityComparer<K>.Default.Equals(i.key, key)){
                i.value = value;
                return;
            }
            if(i.next == null) temp = i;
        }
        temp.next = E;

    }
    public void del_elem(K key){
        if(this.first == null) return;
        if(EqualityComparer<K>.Default.Equals(this.first.key,key)) this.first = this.first.next;
        for(Element<K,V> i = this.first; i != null; i = i.next){
            if(i.next == null){
                return;
            }
            if(EqualityComparer<K>.Default.Equals(i.next.key, key)){
                i.next = i.next.next;
                return;
            }
        }
    }
    public void print_dic(){
        if(this.first == null) return;
        for(Element<K,V> i = this.first; i != null; i = i.next){
            Console.WriteLine("{0} : {1}\n",i.key,i.value);
        }
    }
    public bool exist_elem(K key){
        if(this.first == null) return false;
        
        for(Element<K,V> i = this.first; i != null; i = i.next){
            if(EqualityComparer<K>.Default.Equals(i.key, key)){
                return true;
            }
        }
        return false;
    }

    public V get_value(K key){
        if(this.first == null) throw new Exception("Nie powinienes tak robic.");;

        for(Element<K,V> i = this.first; i != null; i = i.next){
            if(EqualityComparer<K>.Default.Equals(i.key, key)){
                return i.value;
            }
        }
        throw new Exception("Nie powinienes tak robic.");
    }
}

class MojProgram {
    public static void Main() {
        MyDictionary<int, string> MyDic = new MyDictionary<int, string>();
        MyDic.add_elem(3,"mleeeko");
        MyDic.add_elem(213,"muuuusztarda");//
        MyDic.add_elem(7,"maaaaka");
        MyDic.add_elem(123,"monnkeee");
        MyDic.add_elem(44,"moj kot tez tu jest");//
        if(MyDic.exist_elem(44) == true){
            Console.WriteLine("Znalazlem: {0}\n",MyDic.get_value(44));
            MyDic.del_elem(44);
        }
        MyDic.del_elem(213);
        Console.WriteLine("Znalazlem: {0}\n",MyDic.get_value(3));
        MyDic.print_dic();


    }
}