#include <stdio.h>

/* Função que calcula o proximo valor na sequencia de collatz*/
int collatz(int n) {

  if(n!=1) {
    if(n%2==0) {
      n=n/2;
    } else {
      n=n*3+1;
    }
  }

    return n;

}

/* Função que calcula o tamanho da sequencia para um determinado n*/
int seqcollatz(int n) {
  int ciclo=1;

  while(n!=1) {

      n=collatz(n);
      ciclo++;

  }

  return ciclo;


}

int main(void){

  int a,b,i,ciclo,maior,x,y;

  while (scanf("%d %d", &x, &y) != EOF) {

    if(x>y) {
      a=y;
      b=x;
    } else {
      a=x;
      b=y;
    }

    maior=1;

    for(i=a;i<=b;i++) {

      ciclo=seqcollatz(i);

      if(maior<ciclo) {
	maior=ciclo;
      }

    }

    printf("%d %d %d\n",x,y,maior);

  }

  return 0;
}
