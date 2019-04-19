#ifndef DATAFRAME_H
#define DATAFRAME_H

/* This just defines the structure. R causality, for example implements it. */
struct dataframe {
    void **df;
    int   *states;
    int    nvar;
    int    nobs;
};
#endif /* dataframe.h */
