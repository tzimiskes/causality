#include<math.h>
#include<stdlib.h>

void c_correlation(double* x, double* y, size_t* n, double * rho) {
  double mu_x = 0;
  double mu_y = 0;
  for(size_t i = 0; i < *n; ++i) {
    mu_x += x[i];
    mu_y += y[i];
  }
  mu_x = mu_x / *n;
  mu_y = mu_y / *n;
  for (size_t i = 0; i < *n; ++i) {
    x[i] -= mu_x;
    y[i] -= mu_y;
  }
  double sd_x = 0;
  double sd_y = 0;
  double num = 0;
  for (size_t i = 0; i < *n; ++i) {
    sd_x += x[i] * x[i];
    sd_y += y[i] * y[i];
    num  += x[i] * y[i];
  }
  *rho = num /sqrt(sd_x) / sqrt(sd_y);
}



