cmake_minimum_required(VERSION 3.10)
project(xfoil Fortran)

enable_language(Fortran)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} \
-O \
-fbounds-check \
-finit-real=inf \
-ffpe-trap=invalid,zero \
-fdefault-real-8")

add_library(xfoil SHARED
        # Include modules
        src/i_pindex.f90
        src/i_xfoil.f90
        src/i_blpar.f90
        src/i_circle.f90
        src/i_xbl.f90
        # Shared modules to avoid circular dependencies
        src/s_xbl.f90
        src/s_xoper.f90
        src/s_xfoil.f90
        # Main modules
        src/m_aread.f90
        src/m_iopol.f90
        src/m_naca.f90
        src/m_sort.f90
        src/m_spline.f90
        src/m_userio.f90
        src/m_xbl.f90
        src/m_xblsys.f90
        src/m_xfoil.f90
        src/m_xgdes.f90
        src/m_xgeom.f90
        src/m_xmdes.f90
        src/m_xoper.f90
        src/m_xpanel.f90
        src/m_xpol.f90
        src/m_xqdes.f90
        src/m_xsolve.f90
        src/m_xutils.f90
        # API
        src/api.f90)
