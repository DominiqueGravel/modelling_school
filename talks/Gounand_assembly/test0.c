//to compile : gcc test0.c -o test 


#include <stdio.h>
#include <stdlib.h>

// prototypes
//-----------
double *deriv(double *n, double r, double K, double a, double b, double m);
double *euler(double *n, double r, double K, double a, double b, double m, double h);
void dyn(double *init, double r, double K, double a, double b, double m, int tmax, double h);



//===========================================
// MAIN
//=====
int main()
{
  int Tmax = 1000000;
  double pas = 0.1;

  double r = 0.5;
  double K = 5.5;
  double init[2];
  init[0] = 1.0;
  init[1] = 0.5;
  double a = 0.2;
  double b = 0.4;
  double m = 0.2;

  dyn(init,r,K,a,b,m,Tmax,pas);

  return 0;
}




//===========================================
// ode
//----
double *deriv(double *n, double r, double K, double a, double b, double m)
{
  double *dn = (double*)malloc(2 * sizeof(double));
  dn[0] = n[0] * (r * (1 - n[0] / K)) - a * n[0] * n[1] / (1 + b * n[0]);
  dn[1] = a * n[0] * n[1] / (1 + b * n[0]) - m * n[1];
  return(dn);
}


// solver
//-------
double *euler(double *n, double r, double K, double a, double b, double m, double h)
{
  double *der = deriv(n,r,K,a,b,m);
  double *n2 = (double*)malloc(2 * sizeof(double));
  n2[0] = n[0] + h * der[0]; 
  n2[1] = n[1] + h * der[1];
  free(der);
  return(n2);
}


// dynamic
//--------
void dyn(double *init, double r, double K, double a, double b, double m, int tmax, double h)
{
  int i;
  double n0[2];
  n0[0] = init[0];
  n0[1] = init[1];

  for(i = 0 ; i < tmax ; ++i)
  {
    double *n = euler(n0,r,K,a,b,m,h);
    //printf("%.1f\tprey : %.4f\tpredator:%.4f\n",(i+1)*h,n[0],n[1]);
    n0[0] = n[0];
    n0[1] = n[1];
    free(n);
  }
}

