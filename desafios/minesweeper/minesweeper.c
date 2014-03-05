/*
Problem ID: 10189
http://uva.onlinejudge.org/
*/
#include <stdio.h>

int main(void){

  int a,b,i,j;
  int matrizout[100][100]={{0}};
  char matrizin[100][100];

  scanf("%d %d",&a,&b);

  while(a!=0 && b!=0) {

    for(i=0;i<a;i++) {
      for(j=0;j<b;j++) {
      scanf("\n%c",&matrizin[i][j]);
      }
    }

    printf("\n\n");
    printf("\n %d %d \n",a,b);

    for(i=0;i<a;i++) {
      for(j=0;j<b;j++) {
      printf("%i",matrizout[i][j]);
      }
      printf("\n");
    }

    printf("\n\n");

    scanf("%d %d",&a,&b);
  }



  return 0;
}
