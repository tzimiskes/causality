/* TODO */
#include <causality.h>
#include <dataframe.h>
#include <scores.h>
#include <R_ext/Lapack.h>

#ifdef _OPENMP
#include <omp.h>
#endif

#define OMP_N_THRESH     100
#define LOOP_UNROLL_SIZE 4
#define ERROR_THRESH     1e-5

static inline double fddot(double * x, double * y, int n);
void fcov_yy(double * cov_yy, double ** df, int n_par, int n_obs);
void fcov_xy(double * cov_xy, double ** df, int n_par, int n_obs);
/* TODO */
double bic_score(dataframe data, int * xy, int n_par, double * fargs, int * iargs) {
  /* The first thing we need to do is construct the (subset of the) dataframe
   * that is relevent to x and its parents y */
  double * df [n_par + 1];
  for(int i = 0; i < n_par + 1; ++i)
    df[i] = data.df[xy[i]];
  int     n_obs   = data.n_obs;
  double  penalty = fargs[0];
  /* we use this for error checking. If there is a problem with the covariance
   * matrix it is probably not postive definite */
  int not_positive_definite = 0;
  /* allocate enough memory so we have enough space for cov_yy, cov_xy, and
   * cov_xy_cpy. Using only one malloc and one free should be faster than
   * allocating memory for each separately. */
  double * aug_matrix = CALLOC(n_par * (n_par + 2), double);
  double * cov_yy     = aug_matrix;
  double * cov_xy     = aug_matrix + n_par * n_par;
  /* Calculate the covariance matrix of the data matrix of the variables y */
  fcov_yy(cov_yy, df + 1, n_par, n_obs);
  /* calculate the covariance vector between the single variable x, and y */
  fcov_xy(cov_xy, df, n_par, n_obs);
  /* Now, we shall calcluate the BIC score of this configuration by computing
   * log(cov_xx - cov_xy**T cov_yy^-1 * cov_xy) + log(n) * (n_par + 1). The
   * first (and main step) in the rest of this function is to calcluate
   * cov_xy**T cov_yy^-1 * cov_xy.
   * Note that because we normalized all variables, cov_xx = 1,
   * so we don't need to calculate it*/
  double score = 1.0f;
  if(n_par == 1) {
    if(*cov_yy < ERROR_THRESH)
      not_positive_definite = 1;
    score -=(*cov_xy * *cov_xy / *cov_yy);
  }
  else if(n_par == 2) {
    double det = cov_yy[0]*cov_yy[3] - cov_yy[1]*cov_yy[1];
    /* For a 2x2 symmetric matrix, having a non positive determinent, or having
     * a positive determinant and negative trace is sufficient to know that the
     * matrix is not positive definite */
    if(det < ERROR_THRESH || (det > 0 && cov_yy[3] + cov_yy[0] < 0))
      not_positive_definite = 1;
    score -= (cov_yy[3] * cov_xy[0] * cov_xy[0] + cov_yy[0] * cov_xy[1] *
              cov_xy[1] - 2 * cov_yy[1] * cov_xy[0] * cov_xy[1])/det;
  }
  /* Instead, we will now use a few LAPACK routines to solve the equation
   * cov_xy**T * cov_yy^-1 * cov_xy via the cholesky decomposition
   * instead of doing it by hand.*/
  else {
    int err = 0; /* We use err to check for lapack specific problems */
    /* We need to calculate the Cholesky decomposition of cov_yy, so we can use
     * it to solve the linear system cov_yy * X = cov_xy. Hence, we use the
     * LAPACK subroutine dpotrf to achieve this. */
    F77_CALL(dpotrf)("L",
                     &n_par, /* number of rows/cols of cov_yy */
                     cov_yy, /* we want of the decomposotion of this */
                     &n_par, /* stride of cov_yy */
                     &err    /* we use this to check for errors */
                   );
    /* Check to see if cov_yy is not positive definite */
    if(err)
      not_positive_definite = 1;
    else {
      /* We need to create a copy of cov_xy to perform the next subroutine */
      double * cov_xy_cpy = aug_matrix + n_par * (n_par + 1);
      memcpy(cov_xy_cpy, cov_xy, n_par * sizeof(double));
      /* Now, we use the LAPACK routine dpotrs to solve the aforemention system
       * cov_yy * X = cov_xy. cov_xy_cpy is modified in place to be transformed
       * into X, which is why we created a copy of cov_xy. Note that, assuming
       * cov_yy is positive definite, X = cov_yy^-1 * cov_xy */
      int one = 1;
      F77_CALL(dpotrs)("L",
                       &n_par, &one,
                       cov_yy,
                       &n_par,
                       cov_xy_cpy,
                       &n_par,
                       &err);
      if(err)
        not_positive_definite = 1;
      else {
        score -= fddot(cov_xy, cov_xy_cpy, n_par);
      }
    }
  }
  FREE(aug_matrix);
  if(not_positive_definite) {
    warning("covariance matrix not positive definite\n");
    return NA_REAL;
  }
  score = n_obs * log(score) +  penalty * log(n_obs) * (2 * n_par + 1);
  return score;
}

/* fcov_yy in theory should provide a fast calculation of covariance matrix
 * of the ranodom vector y. It attempts to store intermediate scores as much as
 * possible, pull constants for the loop out of loop, and inludes a loop
 * unrolling type technique that will directly caclulate small (dim(y) <= 2)
 * covariance matrices instead of going through the loop, which is much slower.
 * Profiling might add more loop unrolling. Furthermore, this acts directly on R
 * data.frames, so we need to calculate cov_yy anyway to create a compact matrix
 * libRblas can operate on. */
void fcov_yy(double * restrict cov_yy, double ** y_df, int n_par,
             int n_obs)
{
  double inv_nminus1 = 1.0f/(n_obs-1);
  /* I am unsure if these if statements are faster 8*/
  if(n_par == 1) {
    cov_yy[0] = fddot(y_df[0], y_df[0], n_obs) * inv_nminus1;
    return;
  }
  if(n_par == 2) {
    {
      cov_yy[0] = fddot(y_df[0], y_df[0], n_obs) * inv_nminus1;
      cov_yy[2] = /* assigned to coy_yy[1] below */
      cov_yy[1] = fddot(y_df[0], y_df[1], n_obs) * inv_nminus1;
      cov_yy[3] = fddot(y_df[1], y_df[1], n_obs) * inv_nminus1;
    }
    return;
  }
  for(int j = 0; j < n_par; ++j) {
    double * y1 = y_df[j];
    double * cov_yy_jnp  = cov_yy + j * n_par;
    double * cov_yy_joff = cov_yy + j;
    for(int i = 0; i <= j; ++i) {
      /* I don't think there is a speed up if I check i == j here, so just
       *  rewrite if i == j because then there is no branch */
      cov_yy_joff[i*n_par] =
      cov_yy_jnp[i]        = fddot(y1, y_df[i], n_obs) * inv_nminus1;
    }
  }
}
/* fcov_xy caclulates the covariance vector between the ranodom variable x
 * and the random variable vector, y. */
void fcov_xy(double * restrict cov_xy, double ** xy_df, int n_par, int n_obs)
{
  double inv_nminus1 = 1.0f/(n_obs - 1);
  for(int i = 0 ; i < n_par; ++i)
    cov_xy[i] = fddot(xy_df[0], xy_df[i + 1], n_obs) * inv_nminus1;
}

static inline double fddot(double * x, double * y, int n) {
  double sum = 0.0f;
  #pragma omp parallel for reduction(+:sum)
  for(int i = 0; i < n; ++i)
    sum += x[i]*y[i];
  return sum;
}
