#include <Rcpp.h>
using namespace Rcpp;

void intercala(int p, int q, int r, NumericVector v,  NumericVector w)
{
  int i, j, k;

   i = p;
   j = q;
   k = 0;


   while (i < q && j < r) {
      if (v[i] < v[j]) {
         w[k] = v[i];
         i++;
      }
      else {
         w[k] = v[j];
         j++;
      }
      k++;
   }
   while (i < q) {
      w[k] = v[i];
      i++;
      k++;
   }
   while (j < r) {
      w[k] = v[j];
      j++;
      k++;
   }
   for (i = p; i < r; i++)
      v[i] = w[i-p];
}

void mergesort(int p, int r, NumericVector v, NumericVector aux)
{
   int q;
   if (p < r - 1) {
      q = (p + r) / 2;
      mergesort(p, q, v,aux);
      mergesort(q, r, v,aux);
      intercala(p, q, r, v,aux);
   }
}

// [[Rcpp::export]]
NumericVector mergesortC(NumericVector vetor) {
  Rcpp::NumericVector saida = Rcpp::clone(vetor);
  Rcpp::NumericVector aux = Rcpp::clone(vetor);
  int n = saida.size();
  mergesort(0,n,saida,aux);
  return saida;
}


