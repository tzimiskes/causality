/*
 * fcov_xy calculates the covariance vector between the random variable y
 * and the random variable vector, x.
 */
 void fcov_xy(double *restrict cov_xy, double **x, double *y, int n, int m)
 {
     y = __builtin_assume_aligned(y, 32);
     double inv_nm1 = 1.0f / (n - 1.0f);
     //#pragma omp parallel for num_threads(2) if (m > 8)
     for (int i = 0; i < m; ++i) {
         double * x_i = __builtin_assume_aligned(x[i], 32);
         double sum = 0.0f;
         for (int j = 0; j < n; ++j)
             sum += x_i[j] * y[j];
         cov_xy[i] = sum * inv_nm1;
     }
 }


 // void dcov_x_xpy(double *restrict cov_xy, double **x, double **xpy, int n,
 //                                          int m, int p)
 // {
 //     double inv_nm1 = 1.0f / (n - 1.0f);
 //     for (int i = 0; i < m; ++i) {
 //         double *x_i = __builtin_assume_aligned(x[i], 16);
 //         for (int j = 0; j < p; ++j) {
 //             double *xpy_j = __builtin_assume_aligned(xpy[j], 16);
 //             if (x_i == xpy_j) {
 //                 cov_xy[j + i * m] = 1.0f;
 //             }
 //            else {
 //                double sum = 0.0f;
 //                for (int k = 0; k < n; ++k)
 //                sum += x_i[k] * xpy_j[k];
 //                cov_xy[j + i * m] = sum * inv_nm1;
 //            }
 //         }
 //     }
 // }
