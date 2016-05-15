CSTOCHSIMSDIR = /msu/home/m1gsa00/git/CStochSims/
SPARSELIB = $(CSTOCHSIMSDIR)/ranlib.o
DEBSPARSELIB = $(SPARSELIB)

LAPACK  = -L/opt/atlas/lib/ -lcblas -lf77blas -latlas -llapack

RANDOMLIB = $(SPARSELIB)
SPAMADIR = $(CSTOCHSIMSDIR)../sparseAMA
SPAMAINCLUDE = -I$(SPAMADIR)/src/main/include
STOCHSIMSINCLUDE = -I$(CSTOCHSIMSDIR)/consolidateHome/cFiles/nuwebTree/stochSims 


SPAMALIB = $(CSTOCHSIMSDIR)
DEBSPAMALIB = $(CSTOCHSIMSDIR)
STACKLIB = $(CSTOCHSIMSDIR)
DEBSTACKLIB = $(CSTOCHSIMSDIR)
STOCHLIB = $(CSTOCHSIMSDIR)
DEBSTOCHLIB = $(CSTOCHSIMSDIR)

CFLAGS = $(SPAMAINCLUDE) -O4  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc

LINKFLAGS = -O4  -v  -lc -ldl -lm -L../sparseAMA/target/nar/sparseAMA-1.0-SNAPSHOT-amd64-Linux-g++-shared/lib/amd64-Linux-g++/shared -lsparseAMA-1.0-SNAPSHOT $(CSTOCHSIMSDIR)stackC.o $(CSTOCHSIMSDIR)stochProto.o

.SUFFIXES:	.o .c .h




deb<*outFileString*>.o:	<*outFileString*>.c
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -g -pg -o deb<*outFileString*>.o <*outFileString*>.c
deb<*outFileString*>Drv.o:	<*outFileString*>Drv.c
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -g -pg -o deb<*outFileString*>Drv.o <*outFileString*>Drv.c
deb<*outFileString*>Support.o:	<*outFileString*>Support.c
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -g -pg -o deb<*outFileString*>Support.o <*outFileString*>Support.c
deb<*outFileString*>Data.o:	<*outFileString*>Data.c
#	exciseMathEOL.pl <*outFileString*>DataForInclude.h
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -g -pg -o deb<*outFileString*>Data.o <*outFileString*>Data.c
deb<*outFileString*>Shocks.o:	<*outFileString*>Shocks.c
#	exciseMathEOL.pl <*outFileString*>ShocksForInclude.h
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -g -pg -o deb<*outFileString*>Shocks.o <*outFileString*>Shocks.c
debrun<*outFileString*>.o:	run<*outFileString*>.c  run<*outFileString*>LocalDefs.h
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -g -pg -o debrun<*outFileString*>.o run<*outFileString*>.c $(STOCHSIMSINCLUDE)




<*outFileString*>.o:	<*outFileString*>.c
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -O4  -o <*outFileString*>.o <*outFileString*>.c
<*outFileString*>Drv.o:	<*outFileString*>Drv.c
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -O4  -o <*outFileString*>Drv.o <*outFileString*>Drv.c
<*outFileString*>Support.o:	<*outFileString*>Support.c
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -O4  -o <*outFileString*>Support.o <*outFileString*>Support.c
<*outFileString*>Data.o:	<*outFileString*>Data.c
#	exciseMathEOL.pl <*outFileString*>DataForInclude.h
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -O4  -o <*outFileString*>Data.o <*outFileString*>Data.c
<*outFileString*>Shocks.o:	<*outFileString*>Shocks.c
#	exciseMathEOL.pl <*outFileString*>ShocksForInclude.h
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -O4  -o <*outFileString*>Shocks.o <*outFileString*>Shocks.c


run<*outFileString*>.o:	run<*outFileString*>.c run<*outFileString*>LocalDefs.h
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -O4 run<*outFileString*>.c $(STOCHSIMSINCLUDE)

mpirun<*outFileString*>.o:	mpirun<*outFileString*>.c run<*outFileString*>LocalDefs.h
	gcc -c  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc -O4 mpirun<*outFileString*>.c $(STOCHSIMSINCLUDE)


run<*outFileString*>:	<*outFileString*>.o run<*outFileString*>.o \
	<*outFileString*>Drv.o <*outFileString*>Support.o \
	<*outFileString*>Data.o <*outFileString*>Shocks.o 
	gfortran -o run<*outFileString*> -O4 <*outFileString*>.o run<*outFileString*>.o \
	<*outFileString*>Drv.o <*outFileString*>Support.o \
	<*outFileString*>Data.o <*outFileString*>Shocks.o \
	  $(SPARSELIB) $(CSTOCHSIMSDIR)myNewt.o\
		-v  -lc -ldl -lm  $(LINKFLAGS) $(LAPACK) 


debrun<*outFileString*>:	deb<*outFileString*>.o debrun<*outFileString*>.o \
	deb<*outFileString*>Drv.o deb<*outFileString*>Support.o \
	deb<*outFileString*>Data.o deb<*outFileString*>Shocks.o 
	gfortran -o debrun<*outFileString*> -O4 deb<*outFileString*>.o debrun<*outFileString*>.o \
	deb<*outFileString*>Drv.o deb<*outFileString*>Support.o \
	deb<*outFileString*>Data.o deb<*outFileString*>Shocks.o \
	  $(SPARSELIB) $(CSTOCHSIMSDIR)debMyNewt.o\
		-v  -lc -ldl -lm  $(LINKFLAGS) $(LAPACK) 


