cmake_minimum_required(VERSION 3.12)
project(xfoil Fortran)

enable_language(Fortran)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O -fbounds-check -finit-real=inf -ffpe-trap=invalid,zero -fdefault-real-8")
add_executable(xfoil
        src/aread.f
        src/iopol.f
        src/naca.f
        src/sort.f
        src/spline.f
        src/userio.f
        src/xbl.f
        src/xblsys.f
        src/xfoil.f
        src/xgdes.f
        src/xgeom.f
        src/xmdes.f
        src/xoper.f
        src/xpanel.f
        src/xpol.f
        src/xqdes.f
        src/xsolve.f
        src/xutils.f
        )