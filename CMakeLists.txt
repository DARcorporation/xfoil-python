cmake_minimum_required(VERSION 3.12)
project(xfoil Fortran)

enable_language(Fortran)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} \
-O \
-fbounds-check \
-finit-real=inf \
-ffpe-trap=invalid,zero \
-fdefault-real-8")
set(SRC_DIR "src/fortran")

add_library(xfoil SHARED
        # Include modules
        ${SRC_DIR}/i_pindex.f90
        ${SRC_DIR}/i_xfoil.f90
        ${SRC_DIR}/i_blpar.f90
        ${SRC_DIR}/i_circle.f90
        ${SRC_DIR}/i_xbl.f90
        # Shared modules to avoid circular dependencies
        ${SRC_DIR}/s_xbl.f90
        ${SRC_DIR}/s_xoper.f90
        ${SRC_DIR}/s_xfoil.f90
        # Main modules
        ${SRC_DIR}/m_aread.f90
        ${SRC_DIR}/m_iopol.f90
        ${SRC_DIR}/m_naca.f90
        ${SRC_DIR}/m_sort.f90
        ${SRC_DIR}/m_spline.f90
        ${SRC_DIR}/m_userio.f90
        ${SRC_DIR}/m_xbl.f90
        ${SRC_DIR}/m_xblsys.f90
        ${SRC_DIR}/m_xfoil.f90
        ${SRC_DIR}/m_xgdes.f90
        ${SRC_DIR}/m_xgeom.f90
        ${SRC_DIR}/m_xmdes.f90
        ${SRC_DIR}/m_xoper.f90
        ${SRC_DIR}/m_xpanel.f90
        ${SRC_DIR}/m_xpol.f90
        ${SRC_DIR}/m_xqdes.f90
        ${SRC_DIR}/m_xsolve.f90
        ${SRC_DIR}/m_xutils.f90
        # API
        ${SRC_DIR}/api.f90)