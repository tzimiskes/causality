#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <time.h>
#include "headers/causality.h"
#include "headers/causalityRWrapper.h"


SEXP causalityFruchtermanReingold(SEXP graph, SEXP nIterations, SEXP height,
                                               SEXP width)
{

    int nNodes     = length(VECTOR_ELT(graph, NODES));
    int nEdges     = nrows(VECTOR_ELT(graph, EDGES));
    SEXP positions = PROTECT(allocMatrix(REALSXP, nNodes, 2));

    int *edges = calculateEdgesPtr(graph);

    ccf_fr_layout(REAL(positions), nNodes, edges, nEdges,
                                              asReal(width), asReal(height),
                                              asInteger(nIterations));
    free(edges);
    UNPROTECT(1);
    return positions;
}


void ccf_uniform_rng(double *x, int n, double low, double high)
{
    #ifdef R_R_H
    GetRNGstate();
    for (int i = 0; i < n; ++i)
        x[i] = low + (high - low) * unif_rand();
    PutRNGstate();
    #else
    srand();
    for (int i = 0; i < n; ++i)
        x[i] = low  + (high - low) * rand()/ RAND_MAX;
    #endif
}

void ccf_fr_layout(double *positions, int n_nodes, int *edges, int n_edges,
                                      double width, double height,
                                      int iterations)
{
    double  k          = n_edges / n_nodes * 1.0f;
    double  inv_k      = 1.0f / k;
    double  k_sq       = k * k;
    double *x          = positions;
    double *y          = positions + n_nodes;
    double *velocities = malloc(2 * n_nodes * sizeof(double));
    double *v_x        = velocities;
    double *v_y        = velocities + n_nodes;
    /* give each node an initial position in space */
    ccf_uniform_rng(x, n_nodes, 0.25f, width - 0.25f);
    ccf_uniform_rng(y, n_nodes, 0.25f, height - 0.25f);

    double t0 = 2 * (width * height) / (width + height) / 5;
    double t  = t0;
    for (int i = 0; i < iterations; ++i) {
        /* calculate the dispersive forces between x_j and x_k (j != k) */
        for (int j = 0; j < n_nodes; ++j) {
            v_x[j] = 0.0f;
            v_y[j] = 0.0f;
            for (int k = 0; k < n_nodes; ++k) {
                if (j == k)
                    continue;
                double dx [2]  = {x[j] - x[k], y[j] - y[k]};
                double norm_sq = dx[0] * dx[0] + dx[1] * dx[1] + 1e-6;
                v_x[j] += dx[0] / norm_sq * k_sq;
                v_y[j] += dx[1] / norm_sq * k_sq;
            }
        }

        for (int j = 0; j < n_edges; ++j) {
            int n1 = edges[j];
            int n2 = edges[j + n_edges];
            double dx [2] = {x[n1] - x[n2], y[n1] - y[n2]};
            double norm   = sqrt(dx[0] * dx[0] + dx[1] * dx[1]);
            v_x[n1] -= dx[0] * norm * inv_k;
            v_y[n1] -= dx[1] * norm * inv_k;
            v_x[n2] += dx[0] * norm * inv_k;
            v_y[n2] += dx[1] * norm * inv_k;
        }

        for (int j = 0; j < n_nodes; ++j) {
            double s = v_x[j] * v_x[j] + v_y[j] * v_y[j];
            s = sqrt(s + 1e-6);
            x[j] += v_x[j] * fmin(1.0f, t / s);
            y[j] += v_y[j] * fmin(1.0f, t / s);
            x[j] = fmin(width, fmax(0.0f, x[j]));
            y[j] = fmin(height, fmax(0.0f, y[j]));
        }
        t = t0 / sqrt(i + 1);
    }
    free(velocities);
}
