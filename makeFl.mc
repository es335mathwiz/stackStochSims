LAPACK  = -L/opt/atlas/lib/ -lcblas -lf77blas -latlas -llapack
CSTOCHSIMSDIR = ../CStochSims/
SPAMADIR = ../sparseAMA

RANLIBLOC = $(CSTOCHSIMSDIR)/ranlib.o
DEBRANLIBLOC = $(RANLIBLOC)


SPAMALIB = $(CSTOCHSIMSDIR)
DEBSPAMALIB = $(CSTOCHSIMSDIR)

STACKLIB = $(CSTOCHSIMSDIR)
DEBSTACKLIB = $(CSTOCHSIMSDIR)

STOCHLIB = $(CSTOCHSIMSDIR)
DEBSTOCHLIB = $(CSTOCHSIMSDIR)

CFLAGS = -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc

LINKFLAGS =   -v  -lc -ldl -lm -L../sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared -lsparseAMA-1.0-SNAPSHOT $(CSTOCHSIMSDIR)stackC.o $(CSTOCHSIMSDIR)stochProto.o

.SUFFIXES:	.o .c .h




deb<*outFileString*>.o:	<*outFileString*>.c
	gcc $(CFLAGS) -g -pg -o deb<*outFileString*>.o <*outFileString*>.c
deb<*outFileString*>Drv.o:	<*outFileString*>Drv.c
	gcc $(CFLAGS) -g -pg -o deb<*outFileString*>Drv.o <*outFileString*>Drv.c
deb<*outFileString*>Support.o:	<*outFileString*>Support.c
	gcc $(CFLAGS) -g -pg -o deb<*outFileString*>Support.o <*outFileString*>Support.c
deb<*outFileString*>Data.o:	<*outFileString*>Data.c
	gcc $(CFLAGS) -g -pg -o deb<*outFileString*>Data.o <*outFileString*>Data.c
deb<*outFileString*>Shocks.o:	<*outFileString*>Shocks.c
	gcc $(CFLAGS) -g -pg -o deb<*outFileString*>Shocks.o <*outFileString*>Shocks.c
debrun<*outFileString*>.o:	run<*outFileString*>.c  run<*outFileString*>LocalDefs.h
	gcc $(CFLAGS) -g -pg -o debrun<*outFileString*>.o run<*outFileString*>.c 




<*outFileString*>.o:	<*outFileString*>.c
	gcc $(CFLAGS) -O4  -o <*outFileString*>.o <*outFileString*>.c
<*outFileString*>Drv.o:	<*outFileString*>Drv.c
	gcc $(CFLAGS) -O4  -o <*outFileString*>Drv.o <*outFileString*>Drv.c
<*outFileString*>Support.o:	<*outFileString*>Support.c
	gcc $(CFLAGS) -O4  -o <*outFileString*>Support.o <*outFileString*>Support.c
<*outFileString*>Data.o:	<*outFileString*>Data.c
	gcc $(CFLAGS) -O4  -o <*outFileString*>Data.o <*outFileString*>Data.c
<*outFileString*>Shocks.o:	<*outFileString*>Shocks.c
	gcc $(CFLAGS) -O4  -o <*outFileString*>Shocks.o <*outFileString*>Shocks.c


run<*outFileString*>.o:	run<*outFileString*>.c run<*outFileString*>LocalDefs.h
	gcc $(CFLAGS) -O4 run<*outFileString*>.c 

mpirun<*outFileString*>.o:	mpirun<*outFileString*>.c run<*outFileString*>LocalDefs.h
	gcc $(CFLAGS) -O4 mpirun<*outFileString*>.c 


run<*outFileString*>:	<*outFileString*>.o run<*outFileString*>.o \
	<*outFileString*>Drv.o <*outFileString*>Support.o \
	<*outFileString*>Data.o <*outFileString*>Shocks.o 
	gfortran -o run<*outFileString*> -O4 <*outFileString*>.o run<*outFileString*>.o \
	<*outFileString*>Drv.o <*outFileString*>Support.o \
	<*outFileString*>Data.o <*outFileString*>Shocks.o \
	  $(RANLIBLOC) $(CSTOCHSIMSDIR)myNewt.o\
		-v  -lc -ldl -lm  $(LINKFLAGS) $(LAPACK) 


debrun<*outFileString*>:	deb<*outFileString*>.o debrun<*outFileString*>.o \
	deb<*outFileString*>Drv.o deb<*outFileString*>Support.o \
	deb<*outFileString*>Data.o deb<*outFileString*>Shocks.o 
	gfortran -g -o debrun<*outFileString*>  deb<*outFileString*>.o debrun<*outFileString*>.o \
	deb<*outFileString*>Drv.o deb<*outFileString*>Support.o \
	deb<*outFileString*>Data.o deb<*outFileString*>Shocks.o \
	  $(RANLIBLOC) $(CSTOCHSIMSDIR)debMyNewt.o\
		-v  -lc -ldl -lm  $(LINKFLAGS) $(LAPACK) 


