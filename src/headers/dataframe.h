#ifndef _DATAFRAME_H
#define _DATAFRAME_H
typedef struct dataframe {
  void ** df;
  int  *  states;
  int     nvar;
  int     nobs;
} dataframe;
#endif
