#ifndef FGES_RECORD_H
#define FGES_RECORD_H

typedef struct fges_record {
  int   parent;
  int   child;
  int   n_s;
  int   n_naXY;
  int * s;
  int * naXY;
} ges_record; /*  32 bytes */

#endif
