CC = gcc
CFLAGS = -O3 -g -Wall
CPPFLAGS = -I.
CPICFLAGS = -fpic

OBJECTS = linearalgebra.o bic_score.o bdeu_score.o score_graph.o cgraph.o \
	edge_list.o aggregate_graphs.o tree.o sort.o pdx.o chickering.o meek.o \
	ges_utils.o ges_reorient.o ges_heap.o ges_bic_score.o ges_bdeu_score.o \
	ges.o

all : $(OBJECTS)
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -shared -o libcausality.so \
	$(OBJECTS)

objects: $(OBJECTS)

clean:
	rm -f *.o
linearalgebra.o: scores/linearalgebra.c scores/linearalgebra.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) -std=c99 -O3 -ffast-math -mtune=native \
		-march=native -c scores/linearalgebra.c

bic_score.o: scores/bic_score.c dataframe.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c scores/bic_score.c

bdeu_score.o: scores/bdeu_score.c dataframe.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c scores/bdeu_score.c

score_graph.o: scores/score_graph.c dataframe.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c scores/score_graph.c

cgraph.o: cgraph/cgraph.c edge_list.o cgraph/cgraph.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c cgraph/cgraph.c

edge_list.o: cgraph/edge_list.c cgraph/edge_list.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c cgraph/edge_list.c

tree.o : aggregate/tree.c aggregate/tree.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c aggregate/tree.c

aggregate_graphs.o: aggregate/aggregate_graphs.c tree.o causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c aggregate/aggregate_graphs.c

pdx.o: algorithms/pdx.c cgraph.o edge_list.o causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c algorithms/pdx.c

chickering.o: algorithms/chickering.c cgraph.o edge_list.o causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c algorithms/chickering.c

meek.o: algorithms/meek.c cgraph.o edge_list.o algorithms/meek.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c algorithms/meek.c

sort.o: algorithms/sort.c cgraph.o edge_list.o causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c algorithms/sort.c

ges_utils.o: ges/ges_utils.c ges/ges.h ges/ges_internal.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c ges/ges_utils.c

ges_reorient.o: ges/ges_reorient.c meek.o cgraph.o edge_list.o \
	algorithms/meek.h ges/ges.h ges/ges_internal.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c ges/ges_reorient.c

ges_heap.o: ges/ges_heap.c ges/ges_internal.h causality.h
		$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c ges/ges_heap.c

ges_bic_score.o: ges/ges_bic_score.c linearalgebra.o bic_score.o edge_list.o \
	cgraph.o ges/ges_internal.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c ges/ges_bic_score.c

ges_bdeu_score.o: ges/ges_bdeu_score.c bdeu_score.o edge_list.o cgraph.o \
	ges/ges_internal.h causality.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c ges/ges_bdeu_score.c

ges.o: ges/ges.c ges_reorient.o ges_heap.o ges_utils.o ges_bic_score.o \
	ges_bdeu_score.o ges/ges.h ges/ges_internal.h
	$(CC) $(CPPFLAGS) $(CPICFLAGS) $(CFLAGS) -c ges/ges.c
