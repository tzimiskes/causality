#ifndef GES_RECORD_H
#define GES_RECORD_H

typedef struct ges_record {
  int   x;
  int   y;
  int   set_size;
  int   naxy_size;
  int * set;
  int * naxy;
} ges_record; /*  32 bytes */

#endif
