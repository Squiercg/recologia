#include <stdio.h>

int main (void)
{
  int menor , maior , passo , soma;
menor = 0;
maior = 1000;
passo = 1;
soma = 0;

while (menor < maior)
{
if (menor % 3 == 0 || menor % 5 == 0) {
soma += menor;
}
menor += passo;
}
printf("A soma dos multiplo de 3 e 5 menor que 1000 e %d\n", soma);
 return 0;
}
