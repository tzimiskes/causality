#ifndef _DATAFRAME_H
#define _DATAFRAME_H
struct dataframe {
    void **df;
    int   *states;
    int    nvar;
    int    nobs;
};
#endif
