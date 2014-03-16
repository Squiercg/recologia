#include<stdio.h>

int main(void)
{
  int a , b , soma , parcial , resultado;
  a = 0;
  b = 1;
  soma = 0;
  resultado = 0;

while(soma <= 4000000){
  a = b;
  b = soma;
  parcial = soma;
  if(soma % 2 ==0) resultado += parcial;
  soma = a + b;
 }
 printf("A soma de todos os elementos da serie de fibonnaci menor que 4 milhoes e %d\n",resultado);
        return 0;
}
