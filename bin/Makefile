#*********************************************************
# Makefile for XFOIL V6.93 programs
# H.Youngren 4/24/01
# M.Drela
#*********************************************************


SHELL = sh
BINDIR = /home/codes/bin/
#BINDIR = .

PROGS = xfoil

SRC = ../src
OSRC = ../osrc

XFOILOBJ = xfoil.o xpanel.o xoper.o xgdes.o xqdes.o xmdes.o \
xsolve.o xbl.o xblsys.o xpol.o xgeom.o xutils.o \
aread.o naca.o spline.o iopol.o sort.o

XUTILOBJ  = userio.o

FTNLIB =

###================================================
###  Default compilers and flags
###  FFLOPT used for xsolve.f90
FC = f77
FFLAGS  = -O 
FFLOPT  = -O
INSTALLCMD = install -s

CC = cc
CFLAGS = -O -DUNDERSCORE

##--------------------------

# Uncomment flags for desired machine...

##--------------------------
### DEC Alpha with OSF and DEC f77/f90 compiler
#FC = f77
#FFLAGS = -fast -O4 -tune host
#FFLOPT = -fast -O4 -tune host
#FFLOPT = -fast -O5 -tune host -unroll 3
# Debug flags
#FFLAGS = -O0 -g
#FFLOPT = -fast -O4 -tune host
##--------------------------
### SGI setup
#FC = f77
#FFLAGS = -O2 -static
#FFLOPT = -O2 -static
##--------------------------
##  Uncomment for RS/6000
#FFLAGS = -O -qextname
#FFLOPT = -O -qextname
##--------------------------
##  Uncomment for HP-9000
#FFLAGS = -O +ppu
#FFLOPT = -O +ppu
#FTNLIB = -U77
##--------------------------
### Absoft Linux f77
#FC = f77
#FFLAGS = -O -f -s -W -B108 -N34
#FFLOPT = -O -f -s -W -B108 -N34
##--------------------------
### f2c/gcc compiler driver 
#FC = fort77
#FFLAGS = -O2 -fomit-frame-pointer
#FFLOPT = -O2 -fomit-frame-pointer
##--------------------------
### GNU g77
#FC = g77
#FFLAGS = -O3 -fomit-frame-pointer
#FFLOPT = -O3 -fomit-frame-pointer
# Debug flags (symbols, array bounds)
#FC = g77
#FFLAGS = -g -O0 -C
##--------------------------
### GNU gfortran
#FC = gfortran
#FFLAGS = -O3 -fomit-frame-pointer
#FFLOPT = -O3 -fomit-frame-pointer
# Debug flags (symbols, array bounds)
#FFLAGS = -g -O0 -fbounds-check
#FFLOPT = -g -O0 -fbounds-check
#
### Double precision option
#FFLAGS = -O3 -fdefault-real-8
#FFLOPT = -O3 -fdefault-real-8
#FFLAGS = -O3 -fdefault-real-8 -fbounds-check
#FFLOPT = -O3 -fdefault-real-8 -fbounds-check
# Debug flags (symbols, array bounds)
#FFLAGS = -g -O0 -C
##--------------------------
### Gfortran
FC = gfortran
CHK = 
CHK = -fbounds-check -finit-real=inf -ffpe-trap=invalid,zero
DBL = -fdefault-real-8
FFLAGS = -O $(CHK) $(DBL)
FFLOPT = -O $(CHK) $(DBL)
FTNLIB =



all:	 $(PROGS)

install: 
	$(INSTALLCMD) $(PROGS) $(BINDIR)

clean:
	-/bin/rm $(PROGS)
	-/bin/rm $(XFOILOBJ) $(XUTILOBJ)
#	-/bin/rm *.o



xfoil: $(XFOILOBJ) $(XUTILOBJ) $(OSOBJ)
	$(FC) -o xfoil $(XFOILOBJ) $(XUTILOBJ) $(FTNLIB)
	$(INSTALLCMD) xfoil $(BINDIR)


xfoil.o: $(SRC)/xfoil.f90  $(SRC)/XFOIL.INC $(SRC)/XBL.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xfoil.f90
xpanel.o: $(SRC)/xpanel.f90  $(SRC)/XFOIL.INC
	$(FC) -c $(FFLOPT)  $(SRC)/xpanel.f90
xoper.o: $(SRC)/xoper.f90  $(SRC)/XFOIL.INC  $(SRC)/BLPAR.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xoper.f90
xsolve.o: $(SRC)/xsolve.f90  $(SRC)/XFOIL.INC
	$(FC) -c $(FFLOPT)  $(SRC)/xsolve.f90
xgdes.o: $(SRC)/xgdes.f90  $(SRC)/XFOIL.INC $(SRC)/XDES.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xgdes.f90
xqdes.o: $(SRC)/xqdes.f90  $(SRC)/XFOIL.INC $(SRC)/XDES.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xqdes.f90
xmdes.o: $(SRC)/xmdes.f90  $(SRC)/XFOIL.INC $(SRC)/XDES.INC $(SRC)/CIRCLE.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xmdes.f90
xbl.o:   $(SRC)/xbl.f90    $(SRC)/XFOIL.INC $(SRC)/XBL.INC $(SRC)/BLPAR.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xbl.f90
xblsys.o: $(SRC)/xblsys.f90                  $(SRC)/XBL.INC $(SRC)/BLPAR.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xblsys.f90
xpol.o: $(SRC)/xpol.f90     $(SRC)/XFOIL.INC $(SRC)/PINDEX.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xpol.f90
xgeom.o: $(SRC)/xgeom.f90 
	$(FC) -c $(FFLAGS)  $(SRC)/xgeom.f90
xutils.o: $(SRC)/xutils.f90 
	$(FC) -c $(FFLAGS)  $(SRC)/xutils.f90
aread.o: $(SRC)/aread.f90 
	$(FC) -c $(FFLAGS)  $(SRC)/aread.f90
naca.o: $(SRC)/naca.f90 
	$(FC) -c $(FFLAGS)  $(SRC)/naca.f90
plutil.o: $(SRC)/plutil.f90 
	$(FC) -c $(FFLAGS)  $(SRC)/plutil.f90
userio.o: $(SRC)/userio.f90 
	$(FC) -c $(FFLAGS)  $(SRC)/userio.f90
spline.o: $(SRC)/spline.f90 
	$(FC) -c $(FFLAGS)  $(SRC)/spline.f90
sort.o: $(SRC)/sort.f90
	$(FC) -c $(FFLAGS)  $(SRC)/sort.f90

iopol.o: $(SRC)/iopol.f90 $(SRC)/PINDEX.INC
	$(FC) -c $(FFLAGS)  $(SRC)/iopol.f90

