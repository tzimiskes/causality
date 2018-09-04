#ifndef GES_RECORD_H
#define GES_RECORD_H

typedef struct ges_record {
  int   parent;
  int   child;
  int   n_s;
  int   n_naXY;
  int * s;
  int * naXY;
} ges_record; /*  32 bytes */

#endif
