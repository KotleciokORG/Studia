/*
Kacper Jodlowski
Lista 2
Zadanie 2
*/

using System;
using System.Collections;
using System.Collections.Generic;

class SlowaFibonacciego : IEnumerable<string> {
    private int ile;
    public SlowaFibonacciego(int x){
        this.ile = x;
    }
    
    private class praca : IEnumerator<string>{
        int curr = 0;
        string prev1;
        string prev2;
        string curr_world;
        public praca(int ile){
            Count = ile;
            this.prev1 = "b";
            this.prev2 = "a";
        }
        public int Count { get; }
        public string Current => curr_world;
        object IEnumerator.Current => this.curr_world;
        public bool MoveNext(){
            if(this.curr==1) this.curr_world = "b";
            if(this.curr==2) this.curr_world = "a";
            if(this.curr>2){
                string temp = this.prev2 + this.prev1;
                this.prev1 = this.prev2;
                this.prev2 = temp;
                this.curr_world = this.prev2;
                
            }
            this.curr++;
            return this.curr <= Count+1;
        }
        public void Reset(){
            this.curr=0;
            this.prev1 = "b";
            this.prev2 = "a";
            this.curr_world = "b";
        }
        public void Dispose(){
            //nope
        }
        
    }

    IEnumerator IEnumerable.GetEnumerator(){
        return GetEnumerator();
    }

    public IEnumerator<string> GetEnumerator()
    {
        return new praca(ile);
    }

}


class MojProgram {
    public static void Main() {
        
        SlowaFibonacciego sf = new SlowaFibonacciego(6);
        foreach(string s in sf)
            Console.WriteLine(s);
    }
}