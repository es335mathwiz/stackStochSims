#/*Mathematica Creation Date<*Date[]*>*/
GARYHOME = /mq/home/m1gsa00/
SPARSELIB = -L$(GARYHOME)/dataHome/sparse/SPARSKIT2 -lskit
DEBSPARSELIB = -L$(GARYHOME)/dataHome/sparse/SPARSKIT2 -ldebSkit

LAPACK  = $(GARYHOME)/lapack/LAPACK/lapack_os5.a \
	$(GARYHOME)/lapack/LAPACK/blas_os5.a

RANDOMLIB = -L$(GARYHOME)/matlab/ranlib_c-93/src -lran 

SPAIMINCLUDE = -I$(GARYHOME)/consolidateHome/cFiles/nuwebTree/sparseAim
STOCHSIMSINCLUDE = -I$(GARYHOME)/consolidateHome/cFiles/nuwebTree/stochSims \
-I$(GARYHOME)/consolidateHome/mathFiles/src/codeGeneration/ -I$(GARYHOME)/consolidateHome/myPapers/beowulf/justInCase/include
SPAIMLIB = -L$(GARYHOME)/consolidateHome/cFiles/nuwebTree/sparseAim -lsparseAim
DEBSPAIMLIB = -L$(GARYHOME)/consolidateHome/cFiles/nuwebTree/sparseAim -ldebSparseAim \
 -L$(GARYHOME)/f2c/libf2c/  -l f2c

STACKLIB = -L$(GARYHOME)/consolidateHome/cFiles/nuwebTree/stackC -lstackC
DEBSTACKLIB = -L$(GARYHOME)/consolidateHome/cFiles/nuwebTree/stackC -ldebStackC \
 -L$(GARYHOME)/f2c/libf2c/  -l f2c

STOCHLIB = -L$(GARYHOME)/consolidateHome/cFiles/nuwebTree/stochSims -lstochSims
DEBSTOCHLIB = -L$(GARYHOME)/consolidateHome/cFiles/nuwebTree/stochSims -ldebStochSims\
 -L$(GARYHOME)/f2c/libf2c/  -l f2c

CFLAGS = $(SPAIMINCLUDE) -O4  -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc

LINKFLAGS = -O4

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
	f77 -o run<*outFileString*> -O4 <*outFileString*>.o run<*outFileString*>.o \
	<*outFileString*>Drv.o <*outFileString*>Support.o \
	<*outFileString*>Data.o <*outFileString*>Shocks.o\
	$(STOCHLIB) $(STACKLIB) $(SPARSELIB) \
		-v  -lc -ldl -lm $(SPAIMLIB) $(LAPACK) $(RANDOMLIB)\


