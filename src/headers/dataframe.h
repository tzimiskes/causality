#ifndef DATAFRAME_H
#define DATAFRAME_H
struct dataframe {
    void **df;
    int   *states;
    int    nvar;
    int    nobs;
};
#endif
