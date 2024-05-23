/*
Kacper Jodlowski
Lista 1 
Zadanie 2
*/

#include <stdio.h>
#include <stdlib.h>

int NWD(int x,int y){
    if(x<y){
        int t = x;
        x = y;
        y = t;
    }
    //printf("%d i %d\n",x,y);
    if(y == 0) return x;

    return NWD(y,x%y);
}

typedef struct{
    int num;
    int denom;
} Ulamek;

Ulamek * nowy_ulamek(int num, int denom){
    // printf("Tworze %d / %d\n",num,denom);
    int nwd = NWD(num,denom);
    num/=nwd;
    denom/=nwd;
    Ulamek* nowy = malloc(sizeof(Ulamek));
    if(nowy == NULL) return NULL;
    nowy->num = num;
    nowy->denom = denom;
    return nowy;
}

void show(Ulamek *u){
    printf("%d/%d\n",u->num,u->denom);
}

//no side effects, new pointer
Ulamek* new_pomnoz (Ulamek* u1,Ulamek* u2){
    Ulamek*  x=   nowy_ulamek(u1->num * u2->num , u1->denom * u2->denom);
    return x;
}

Ulamek* new_podziel (Ulamek* u1,Ulamek* u2){
    Ulamek* x =   nowy_ulamek(u1->num * u2->denom , u1->denom * u2->num);
    return x;
}

Ulamek* new_dodaj (Ulamek* u1,Ulamek* u2){
    Ulamek* x =   nowy_ulamek(u1->num * u2->denom + u2->num * u1->denom , u1->denom * u2->denom);
    return x;
}

Ulamek* new_odejmij (Ulamek* u1,Ulamek* u2){
    Ulamek* x =   nowy_ulamek(u1->num * u2->denom - u2->num * u1->denom , u1->denom * u2->denom);
    return x;
}

//change the first fraction

void pomnoz (Ulamek* u1,Ulamek* u2){
    u1->num *= u2->num;
    u1->denom *= u2->denom;
}

void podziel (Ulamek* u1,Ulamek* u2){
    u1->num *= u2->denom;
    u1->denom *= u2->num;
}

void dodaj (Ulamek* u1,Ulamek* u2){
    u1-> num = u1->num * u2->denom + u2->num * u1->denom;
    u1->denom *= u2->denom;
}

void odejmij (Ulamek* u1,Ulamek* u2){
    u1->num = u1->num * u2->denom - u2->num * u1->denom;
    u1->denom *= u2->denom;
}

int main(){
    Ulamek* u1 = nowy_ulamek(2,5);
    Ulamek* u2 = nowy_ulamek(4,6);
    Ulamek* u3 = nowy_ulamek(9,12);
    Ulamek* u4 = nowy_ulamek(7,11);


    Ulamek* a = new_dodaj(u1,u2);
    show(a);

    podziel(u3,u4);
    show(u3);

    return 0;
}