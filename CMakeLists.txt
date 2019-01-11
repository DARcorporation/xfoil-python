cmake_minimum_required(VERSION 3.12)
project(xfoil Fortran)

enable_language(Fortran)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O -fbounds-check -finit-real=inf -ffpe-trap=invalid,zero -fdefault-real-8")
add_executable(xfoil
        src/aread.f90
        src/i_iopol.f90
        src/iopol.f90
        src/naca.f90
        src/sort.f90
        src/spline.f90
        src/userio.f90
        src/xbl.f90
        src/xblsys.f90
        src/xfoil.f90
        src/xgdes.f90
        src/xgeom.f90
        src/xmdes.f90
        src/xoper.f90
        src/xpanel.f90
        src/xpol.f90
        src/xqdes.f90
        src/xsolve.f90
        src/xutils.f90
        )