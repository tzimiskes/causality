void dc_cov_xy(double *restrict cov_xy, double **x, double *y, int n, int m)
{
    #ifndef  _WIN32
    y = __builtin_assume_aligned(y, 32);
    #endif
    double inv_nm1 = 1.0f / (n - 1.0f);
    //#pragma omp parallel for num_threads(2) if (m > 8)
    for (int i = 0; i < m; ++i) {
        #ifndef _WIN32
        double *x_i = __builtin_assume_aligned(x[i], 32);
        #else
        double *x_i = x[i];
        #endif
        double sum = 0.0f;
        for (int j = 0; j < n; ++j)
            sum += x_i[j] * y[j];
    cov_xy[i] = sum * inv_nm1;
    }
}

 void dc_cov_xx(double * restrict cov_xx, double **x, int n, int m)
 {
     double inv_nm1 = 1.0f / (n - 1.0f);
     for (int i = 0; i < m; ++i) {
         #ifndef _WIN32
         double *x_i =  __builtin_assume_aligned(x[i], 32);
         #else
         double *x_i = x[i];
         #endif
         for (int j = i; j < m; ++j) {
             if (i == j)
                 cov_xx[j + m * i] = 1.0f;
             else {
                 #ifndef _WIN32
                 double *x_j =  __builtin_assume_aligned(x[j], 32);
                 #else
                 double *x_j =  x[j];
                 #endif
                 double sum = 0.0f;
                 for (int k = 0; k < n; ++k)
                    sum += x_i[k] * x_j[k];
                 cov_xx[j + m * i] = sum * inv_nm1;
                 cov_xx[i + m * j] = cov_xx[j + m * i];
             }
         }
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
