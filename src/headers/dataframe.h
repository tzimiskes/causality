#ifndef _DATAFRAME_H
#define _DATAFRAME_H
typedef struct dataframe {
  void ** df;
  int  *  states;
  int     n_var;
  int     n_obs;
} dataframe;
#endif
