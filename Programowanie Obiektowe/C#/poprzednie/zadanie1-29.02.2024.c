/*
Kacper Jodlowski
Lista 1 
Zadanie 1
*/

#include <stdio.h>
#include <stdlib.h>
#define Pi 3.14


float absolute(float x){
    return (x<0) ? -x : x;
}

typedef struct{
    float x;
    float y;
} Punkt;

typedef struct{
    enum {
        KOLO,
        CZWOROBOK,
        TROJKAT
    } typfig;


    union {
        Punkt punkty4[4];
        Punkt punkty3[3];
        struct{ 
            Punkt Srodek;
            float radius;
        } Info;
    };

} Figura;

Figura* new_4square(Punkt a,Punkt b, Punkt c,Punkt d){
    Figura* Czworkrat = malloc(sizeof(Figura));
    if(Czworkrat == NULL) return NULL;

    Czworkrat->typfig = CZWOROBOK;

    Czworkrat->punkty4[0] = a;
    Czworkrat->punkty4[1] = b;
    Czworkrat->punkty4[2] = c;
    Czworkrat->punkty4[3] = d;

    return Czworkrat;
}
Figura* new_triangle(Punkt a,Punkt b, Punkt c){
    Figura* Trojkat = malloc(sizeof(Figura));
    if(Trojkat == NULL) return NULL;

    Trojkat->typfig = TROJKAT;

    Trojkat->punkty4[0] = a;
    Trojkat->punkty4[1] = b;
    Trojkat->punkty4[2] = c;
    
    return Trojkat;
}
Figura* new_circle(Punkt s,float radius){
    Figura* Okrag = malloc(sizeof(Figura));
    if(Okrag == NULL) return NULL;

    Okrag->typfig = KOLO;

    Okrag->Info.Srodek = s;
    Okrag->Info.radius = radius;
    
    return Okrag;
}

void usun(Figura* f){
    free(f);
    return;
}

float pole(Figura* f){
    enum {
        A,B,C,D
    };
    switch (f->typfig){
        case KOLO:
            return f->Info.radius*f->Info.radius*Pi;
        case TROJKAT:
            return (0.5)* absolute( (f->punkty3[B].x - f->punkty3[A].x)*(f->punkty3[C].y - f->punkty3[A].y) - (f->punkty3[B].y - f->punkty3[A].y)*(f->punkty3[C].x - f->punkty3[A].x ) );
        case CZWOROBOK:
            return (0.5)* absolute( (f->punkty4[B].x - f->punkty4[A].x)*(f->punkty4[C].y - f->punkty4[A].y) - (f->punkty4[B].y - f->punkty4[A].y)*(f->punkty4[C].x - f->punkty4[A].x ) ) + 
                   (0.5)* absolute( (f->punkty4[D].x - f->punkty4[A].x)*(f->punkty4[C].y - f->punkty4[A].y) - (f->punkty4[D].y - f->punkty4[A].y)*(f->punkty4[C].x - f->punkty4[A].x ) );
    }
}

void przesun(Figura *f, float x, float y){
    switch (f->typfig){
        case KOLO:
            f->Info.Srodek.x+=x;
            f->Info.Srodek.y+=y;
            return;

        case TROJKAT:
            f->punkty3[0].x+=x;
            f->punkty3[0].y+=y;
            f->punkty3[1].x+=x;
            f->punkty3[1].y+=y;
            f->punkty3[2].x+=x;
            f->punkty3[2].y+=y;
            return;
        case CZWOROBOK:
            f->punkty4[0].x+=x;
            f->punkty4[0].y+=y;
            f->punkty4[1].x+=x;
            f->punkty4[1].y+=y;
            f->punkty4[2].x+=x;
            f->punkty4[2].y+=y;
            f->punkty4[3].x+=x;
            f->punkty4[3].y+=y;

    }
}

void show(Figura *f){
    switch (f->typfig){
        case KOLO:
            printf("Jest to okrag o srodku (%f,%f) i promieniu %f\n",f->Info.Srodek.x,f->Info.Srodek.y,f->Info.radius);
        break;
        case TROJKAT:
            printf("Jest to trojkat o wierzcholkach w punkcie (%f,%f), (%f,%f) i (%f,%f)\n",
                    f->punkty3[0].x,f->punkty3[0].y,
                    f->punkty3[1].x,f->punkty3[1].y,
                    f->punkty3[2].x,f->punkty3[2].y);
        break;    
        case CZWOROBOK:
        printf("Jest to czworokat o wierzcholkach w punkcie (%f,%f), (%f,%f), (%f,%f) i (%f,%f)\n",
                    f->punkty4[0].x,f->punkty4[0].y,
                    f->punkty4[1].x,f->punkty4[1].y,
                    f->punkty4[2].x,f->punkty4[2].y,
                    f->punkty4[3].x,f->punkty4[3].y);
        break;
    }
}

float sumapol(Figura* f[], int size){
    float suma = 0;
    for(int i=0;i<size;i++){
        suma += pole(f[i]);
    }
    return suma;
}


int main(){
    //od A przeciwnie do wskazowek zegara punkty
    Punkt A = {.x = 0, .y = 2},
          B = {.x = 7, .y = 2},
          C = {.x = 7, .y = 6},
          D = {.x = 3, .y = 6};
    Figura* kwadrat = new_4square(A,B,C,D);

    printf("%f\n",pole(kwadrat));

    usun(kwadrat);

    return 0;
}