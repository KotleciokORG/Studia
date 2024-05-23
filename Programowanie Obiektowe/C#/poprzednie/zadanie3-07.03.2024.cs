/*
Kacper Jodlowski
Lista 2
Zadanie 3
*/

using System;


class BigNum{
    public string Liczba;
    
    private int gc(int ind){ //getchar
        if(ind >= this.Liczba.Length) return -1;
        else
            return this.Liczba[ind]-'0';
    }
    public BigNum(int val){
        this.Liczba = val.ToString();
    }
    public void print(){
        Console.WriteLine("{0}\n",this.Liczba);
    }
    
    public void change_char(int ind,int val){
        this.Liczba = this.Liczba.Substring(0,ind) + val.ToString() + this.Liczba.Substring(ind+1);
    }
    public void cut_zeros(){
        this.Liczba = this.Liczba.TrimStart('0');
    }

    public void dodaj(BigNum s2){
        int i_m = this.Liczba.Length-1;
        int i_s = s2.Liczba.Length-1;
        int roznica = (i_m-i_s>0) ? i_m-i_s : i_s-i_m;
        string zeros = String.Concat(Enumerable.Repeat("0", roznica+1));
        this.Liczba = zeros+this.Liczba;
        s2.Liczba = zeros + s2.Liczba;

        i_m = this.Liczba.Length-1;
        i_s = s2.Liczba.Length-1;

        int v1,v2,przenies = 0;
        while(i_m >= 0 && i_s >= 0){
            Console.WriteLine("{0} {1}\n",i_m,i_s);
            v1 = this.gc(i_m);
            v2 = s2.gc(i_s);
            Console.WriteLine("{0} {1}\n",v1,v2);
            v1+=v2 + przenies;
            przenies = 0;
            
            if(v1>=10){
                przenies = v1/10;
                v1 = v1 % 10;
            }
            
            this.change_char(i_m,v1);


            i_m--;
            i_s--;
        }
        this.cut_zeros();
        
    }

    //niestety nie wpadlem na odejmowanie
}

class MojProgram {
    public static void Main() {
        BigNum a = new BigNum(99981);
        BigNum b = new BigNum(999999);
        a.dodaj(b);
        a.print();
    }
}