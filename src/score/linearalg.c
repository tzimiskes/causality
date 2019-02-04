/*
 * fcov_xy calculates the covariance vector between the random variable y
 * and the random variable vector, x.
 */
 void fcov_xy(double *restrict cov_xy, double **x, double *y, int n, int m)
 {
     y = __builtin_assume_aligned(y, 16);
     double inv_nm1 = 1.0f / (n - 1.0f);
     #pragma omp parallel for num_threads(2) if (m > 8)
     for (int i = 0; i < m; ++i) {
         double * x_i = __builtin_assume_aligned(x[i], 16);
         double sum = 0.0f;
         for (int j = 0; j < n; ++j)
             sum += x_i[j] * y[j];
         cov_xy[i] = sum * inv_nm1;
     }
 }
