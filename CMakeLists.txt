cmake_minimum_required(VERSION 3.12)
project(xfoil Fortran)

enable_language(Fortran)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O -fbounds-check -finit-real=inf -ffpe-trap=invalid,zero -fdefault-real-8")
set(SRC_DIR "src")
add_executable(xfoil
        # Include modules
        ${SRC_DIR}/i_pindex.f90
        ${SRC_DIR}/i_xfoil.f90
        ${SRC_DIR}/i_blpar.f90
        ${SRC_DIR}/i_circle.f90
        ${SRC_DIR}/i_xbl.f90
        # Shared modules to avoid circular dependencies
        ${SRC_DIR}/s_xbl.f90
        ${SRC_DIR}/s_xoper.f90
        # Source
        ${SRC_DIR}/aread.f90
        ${SRC_DIR}/iopol.f90
        ${SRC_DIR}/naca.f90
        ${SRC_DIR}/sort.f90
        ${SRC_DIR}/spline.f90
        ${SRC_DIR}/userio.f90
        ${SRC_DIR}/xbl.f90
        ${SRC_DIR}/xblsys.f90
        ${SRC_DIR}/xfoil.f90
        ${SRC_DIR}/xgdes.f90
        ${SRC_DIR}/xgeom.f90
        ${SRC_DIR}/xmdes.f90
        ${SRC_DIR}/xoper.f90
        ${SRC_DIR}/xpanel.f90
        ${SRC_DIR}/xpol.f90
        ${SRC_DIR}/xqdes.f90
        ${SRC_DIR}/xsolve.f90
        ${SRC_DIR}/xutils.f90
        )