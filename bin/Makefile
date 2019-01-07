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
###  FFLOPT used for xsolve.f
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


xfoil.o: $(SRC)/xfoil.f  $(SRC)/XFOIL.INC $(SRC)/XBL.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xfoil.f
xpanel.o: $(SRC)/xpanel.f  $(SRC)/XFOIL.INC
	$(FC) -c $(FFLOPT)  $(SRC)/xpanel.f
xoper.o: $(SRC)/xoper.f  $(SRC)/XFOIL.INC  $(SRC)/BLPAR.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xoper.f
xsolve.o: $(SRC)/xsolve.f  $(SRC)/XFOIL.INC
	$(FC) -c $(FFLOPT)  $(SRC)/xsolve.f
xgdes.o: $(SRC)/xgdes.f  $(SRC)/XFOIL.INC $(SRC)/XDES.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xgdes.f
xqdes.o: $(SRC)/xqdes.f  $(SRC)/XFOIL.INC $(SRC)/XDES.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xqdes.f
xmdes.o: $(SRC)/xmdes.f  $(SRC)/XFOIL.INC $(SRC)/XDES.INC $(SRC)/CIRCLE.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xmdes.f
xbl.o:   $(SRC)/xbl.f    $(SRC)/XFOIL.INC $(SRC)/XBL.INC $(SRC)/BLPAR.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xbl.f
xblsys.o: $(SRC)/xblsys.f                  $(SRC)/XBL.INC $(SRC)/BLPAR.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xblsys.f
xpol.o: $(SRC)/xpol.f     $(SRC)/XFOIL.INC $(SRC)/PINDEX.INC
	$(FC) -c $(FFLAGS)  $(SRC)/xpol.f
xgeom.o: $(SRC)/xgeom.f 
	$(FC) -c $(FFLAGS)  $(SRC)/xgeom.f
xutils.o: $(SRC)/xutils.f 
	$(FC) -c $(FFLAGS)  $(SRC)/xutils.f
aread.o: $(SRC)/aread.f 
	$(FC) -c $(FFLAGS)  $(SRC)/aread.f
naca.o: $(SRC)/naca.f 
	$(FC) -c $(FFLAGS)  $(SRC)/naca.f
plutil.o: $(SRC)/plutil.f 
	$(FC) -c $(FFLAGS)  $(SRC)/plutil.f
userio.o: $(SRC)/userio.f 
	$(FC) -c $(FFLAGS)  $(SRC)/userio.f
spline.o: $(SRC)/spline.f 
	$(FC) -c $(FFLAGS)  $(SRC)/spline.f
sort.o: $(SRC)/sort.f
	$(FC) -c $(FFLAGS)  $(SRC)/sort.f

iopol.o: $(SRC)/iopol.f $(SRC)/PINDEX.INC
	$(FC) -c $(FFLAGS)  $(SRC)/iopol.f

