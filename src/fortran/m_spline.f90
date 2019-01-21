!***********************************************************************
!   Copyright (c) 2019 D. de Vries
!   Original Copyright (c) 2000 Mark Drela
!
!   This file is part of XFoil.
!
!   XFoil is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   XFoil is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with XFoil.  If not, see <https://www.gnu.org/licenses/>.
!***********************************************************************

!*==M_SPLINE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module m_spline
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    !*** End of declarations rewritten by SPAG
    !
    !
    !*** Start of declarations rewritten by SPAG
    !
    !*** End of declarations rewritten by SPAG
    !
contains
    subroutine spline(X, Xs, S, N)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        integer, parameter :: NMAX = 1000
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(N) :: S, X, Xs
        intent (in) S, X
        !
        ! Local variables
        !
        real, dimension(NMAX) :: a, b, c
        real :: dsm, dsp
        integer :: i
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-------------------------------------------------------
        !     Calculates spline coefficients for X(S).          |
        !     Zero 2nd derivative end conditions are used.      |
        !     To evaluate the spline at some value of S,        |
        !     use SEVAL and/or DEVAL.                           |
        !                                                       |
        !     S        independent variable array (input)       |
        !     X        dependent variable array   (input)       |
        !     XS       dX/dS array                (calculated)  |
        !     N        number of points           (input)       |
        !                                                       |
        !-------------------------------------------------------
        if (N>NMAX) stop 'SPLINE: array overflow, increase NMAX'
        !
        do i = 2, N - 1
            dsm = S(i) - S(i - 1)
            dsp = S(i + 1) - S(i)
            b(i) = dsp
            a(i) = 2.0 * (dsm + dsp)
            c(i) = dsm
            Xs(i) = 3.0 * ((X(i + 1) - X(i)) * dsm / dsp + (X(i) - X(i - 1)) * dsp / dsm)
        enddo
        !
        !---- set zero second derivative end conditions
        a(1) = 2.0
        c(1) = 1.0
        Xs(1) = 3.0 * (X(2) - X(1)) / (S(2) - S(1))
        b(N) = 1.0
        a(N) = 2.0
        Xs(N) = 3.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
        !
        !---- solve for derivative array XS
        call trisol(a, b, c, Xs, N)
        !
    end subroutine spline
    ! SPLINE


    subroutine splind(X, Xs, S, N, Xs1, Xs2)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        integer, parameter :: NMAX = 1000
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Xs1, Xs2
        real, dimension(N) :: S, X, Xs
        intent (in) S, X, Xs1, Xs2
        !
        ! Local variables
        !
        real, dimension(NMAX) :: a, b, c
        real :: dsm, dsp
        integer :: i
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-------------------------------------------------------
        !     Calculates spline coefficients for X(S).          |
        !     Specified 1st derivative and/or usual zero 2nd    |
        !     derivative end conditions are used.               |
        !     To evaluate the spline at some value of S,        |
        !     use SEVAL and/or DEVAL.                           |
        !                                                       |
        !     S        independent variable array (input)       |
        !     X        dependent variable array   (input)       |
        !     XS       dX/dS array                (calculated)  |
        !     N        number of points           (input)       |
        !     XS1,XS2  endpoint derivatives       (input)       |
        !              If = 999.0, then usual zero second       |
        !              derivative end condition(s) are used     |
        !              If = -999.0, then zero third             |
        !              derivative end condition(s) are used     |
        !                                                       |
        !-------------------------------------------------------
        if (N>NMAX) stop 'SPLIND: array overflow, increase NMAX'
        !
        do i = 2, N - 1
            dsm = S(i) - S(i - 1)
            dsp = S(i + 1) - S(i)
            b(i) = dsp
            a(i) = 2.0 * (dsm + dsp)
            c(i) = dsm
            Xs(i) = 3.0 * ((X(i + 1) - X(i)) * dsm / dsp + (X(i) - X(i - 1)) * dsp / dsm)
        enddo
        !
        if (Xs1==999.0) then
            !----- set zero second derivative end condition
            a(1) = 2.0
            c(1) = 1.0
            Xs(1) = 3.0 * (X(2) - X(1)) / (S(2) - S(1))
        elseif (Xs1==-999.0) then
            !----- set zero third derivative end condition
            a(1) = 1.0
            c(1) = 1.0
            Xs(1) = 2.0 * (X(2) - X(1)) / (S(2) - S(1))
        else
            !----- set specified first derivative end condition
            a(1) = 1.0
            c(1) = 0.
            Xs(1) = Xs1
        endif
        !
        if (Xs2==999.0) then
            b(N) = 1.0
            a(N) = 2.0
            Xs(N) = 3.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
        elseif (Xs2==-999.0) then
            b(N) = 1.0
            a(N) = 1.0
            Xs(N) = 2.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
        else
            a(N) = 1.0
            b(N) = 0.
            Xs(N) = Xs2
        endif
        !
        if (N==2 .and. Xs1==-999.0 .and. Xs2==-999.0) then
            b(N) = 1.0
            a(N) = 2.0
            Xs(N) = 3.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
        endif
        !
        !---- solve for derivative array XS
        call trisol(a, b, c, Xs, N)
        !
    end subroutine splind
    ! SPLIND



    subroutine splina(X, Xs, S, N)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(N) :: S, X, Xs
        intent (in) N, S, X
        intent (out) Xs
        !
        ! Local variables
        !
        real :: ds, dx, xs1, xs2
        integer :: i
        logical :: lend
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-------------------------------------------------------
        !     Calculates spline coefficients for X(S).          |
        !     A simple averaging of adjacent segment slopes     |
        !     is used to achieve non-oscillatory curve          |
        !     End conditions are set by end segment slope       |
        !     To evaluate the spline at some value of S,        |
        !     use SEVAL and/or DEVAL.                           |
        !                                                       |
        !     S        independent variable array (input)       |
        !     X        dependent variable array   (input)       |
        !     XS       dX/dS array                (calculated)  |
        !     N        number of points           (input)       |
        !                                                       |
        !-------------------------------------------------------
        !
        lend = .true.
        do i = 1, N - 1
            ds = S(i + 1) - S(i)
            if (ds==0.) then
                Xs(i) = xs1
                lend = .true.
            else
                dx = X(i + 1) - X(i)
                xs2 = dx / ds
                if (lend) then
                    Xs(i) = xs2
                    lend = .false.
                else
                    Xs(i) = 0.5 * (xs1 + xs2)
                endif
            endif
            xs1 = xs2
        enddo
        Xs(N) = xs1
        !
    end subroutine splina
    ! SPLINA



    subroutine trisol(A, B, C, D, Kk)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Kk
        real, dimension(Kk) :: A, B, C, D
        intent (in) B, Kk
        intent (inout) A, C, D
        !
        ! Local variables
        !
        integer :: k, km
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-----------------------------------------
        !     Solves KK long, tri-diagonal system |
        !                                         |
        !             A C          D              |
        !             B A C        D              |
        !               B A .      .              |
        !                 . . C    .              |
        !                   B A    D              |
        !                                         |
        !     The righthand side D is replaced by |
        !     the solution.  A, C are destroyed.  |
        !-----------------------------------------
        !
        do k = 2, Kk
            km = k - 1
            C(km) = C(km) / A(km)
            D(km) = D(km) / A(km)
            A(k) = A(k) - B(k) * C(km)
            D(k) = D(k) - B(k) * D(km)
        enddo
        !
        D(Kk) = D(Kk) / A(Kk)
        !
        do k = Kk - 1, 1, -1
            D(k) = D(k) - C(k) * D(k + 1)
        enddo
        !
    end subroutine trisol
    ! TRISOL


    function seval(Ss, X, Xs, S, N)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Ss
        real, dimension(N) :: S, X, Xs
        real :: seval
        intent (in) N, S, Ss, X, Xs
        !
        ! Local variables
        !
        real :: cx1, cx2, ds, t
        integer :: i, ilow, imid
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !--------------------------------------------------
        !     Calculates X(SS)                             |
        !     XS array must have been calculated by SPLINE |
        !--------------------------------------------------
        ilow = 1
        i = N
        !
        do while (i - ilow>1)
            !
            imid = (i + ilow) / 2
            if (Ss<S(imid)) then
                i = imid
            else
                ilow = imid
            endif
        enddo
        !
        ds = S(i) - S(i - 1)
        t = (Ss - S(i - 1)) / ds
        cx1 = ds * Xs(i - 1) - X(i) + X(i - 1)
        cx2 = ds * Xs(i) - X(i) + X(i - 1)
        seval = t * X(i) + (1.0 - t) * X(i - 1) + (t - t * t) * ((1.0 - t) * cx1 - t * cx2)
    end function seval
    ! SEVAL

    function deval(Ss, X, Xs, S, N)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Ss
        real :: deval
        real, dimension(N) :: S, X, Xs
        intent (in) N, S, Ss, X, Xs
        !
        ! Local variables
        !
        real :: cx1, cx2, ds, t
        integer :: i, ilow, imid
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !--------------------------------------------------
        !     Calculates dX/dS(SS)                         |
        !     XS array must have been calculated by SPLINE |
        !--------------------------------------------------
        ilow = 1
        i = N
        !
        do while (i - ilow>1)
            !
            imid = (i + ilow) / 2
            if (Ss<S(imid)) then
                i = imid
            else
                ilow = imid
            endif
        enddo
        !
        ds = S(i) - S(i - 1)
        t = (Ss - S(i - 1)) / ds
        cx1 = ds * Xs(i - 1) - X(i) + X(i - 1)
        cx2 = ds * Xs(i) - X(i) + X(i - 1)
        deval = X(i) - X(i - 1) + (1. - 4.0 * t + 3.0 * t * t) * cx1 + t * (3.0 * t - 2.) * cx2
        deval = deval / ds
    end function deval
    ! DEVAL

    function d2val(Ss, X, Xs, S, N)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Ss
        real :: d2val
        real, dimension(N) :: S, X, Xs
        intent (in) N, S, Ss, X, Xs
        !
        ! Local variables
        !
        real :: cx1, cx2, ds, t
        integer :: i, ilow, imid
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !--------------------------------------------------
        !     Calculates d2X/dS2(SS)                       |
        !     XS array must have been calculated by SPLINE |
        !--------------------------------------------------
        ilow = 1
        i = N
        !
        do while (i - ilow>1)
            !
            imid = (i + ilow) / 2
            if (Ss<S(imid)) then
                i = imid
            else
                ilow = imid
            endif
        enddo
        !
        ds = S(i) - S(i - 1)
        t = (Ss - S(i - 1)) / ds
        cx1 = ds * Xs(i - 1) - X(i) + X(i - 1)
        cx2 = ds * Xs(i) - X(i) + X(i - 1)
        d2val = (6. * t - 4.) * cx1 + (6. * t - 2.0) * cx2
        d2val = d2val / ds**2
    end function d2val
    ! D2VAL


    function curv(Ss, X, Xs, Y, Ys, S, N)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Ss
        real :: curv
        real, dimension(N) :: S, X, Xs, Y, Ys
        intent (in) N, S, Ss, X, Xs, Y, Ys
        !
        ! Local variables
        !
        real :: cx1, cx2, cy1, cy2, ds, sd, t, xd, xdd, yd, ydd
        integer :: i, ilow, imid
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-----------------------------------------------
        !     Calculates curvature of splined 2-D curve |
        !     at S = SS                                 |
        !                                               |
        !     S        arc length array of curve        |
        !     X, Y     coordinate arrays of curve       |
        !     XS,YS    derivative arrays                |
        !              (calculated earlier by SPLINE)   |
        !-----------------------------------------------
        !
        ilow = 1
        i = N
        !
        do while (i - ilow>1)
            !
            imid = (i + ilow) / 2
            if (Ss<S(imid)) then
                i = imid
            else
                ilow = imid
            endif
        enddo
        !
        ds = S(i) - S(i - 1)
        t = (Ss - S(i - 1)) / ds
        !
        cx1 = ds * Xs(i - 1) - X(i) + X(i - 1)
        cx2 = ds * Xs(i) - X(i) + X(i - 1)
        xd = X(i) - X(i - 1) + (1.0 - 4.0 * t + 3.0 * t * t) * cx1 + t * (3.0 * t - 2.0) * cx2
        xdd = (6.0 * t - 4.0) * cx1 + (6.0 * t - 2.0) * cx2
        !
        cy1 = ds * Ys(i - 1) - Y(i) + Y(i - 1)
        cy2 = ds * Ys(i) - Y(i) + Y(i - 1)
        yd = Y(i) - Y(i - 1) + (1.0 - 4.0 * t + 3.0 * t * t) * cy1 + t * (3.0 * t - 2.0) * cy2
        ydd = (6.0 * t - 4.0) * cy1 + (6.0 * t - 2.0) * cy2
        !
        sd = sqrt(xd * xd + yd * yd)
        sd = max(sd, 0.001 * ds)
        !
        curv = (xd * ydd - yd * xdd) / sd**3
        !
    end function curv
    ! CURV


    function curvs(Ss, X, Xs, Y, Ys, S, N)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Ss
        real :: curvs
        real, dimension(N) :: S, X, Xs, Y, Ys
        intent (in) N, S, Ss, X, Xs, Y, Ys
        !
        ! Local variables
        !
        real :: bot, cx1, cx2, cy1, cy2, dbotdt, ds, dtopdt, sd, t, top, xd, xdd, xddd, yd, ydd, yddd
        integer :: i, ilow, imid
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-----------------------------------------------
        !     Calculates curvature derivative of        |
        !     splined 2-D curve at S = SS               |
        !                                               |
        !     S        arc length array of curve        |
        !     X, Y     coordinate arrays of curve       |
        !     XS,YS    derivative arrays                |
        !              (calculated earlier by SPLINE)   |
        !-----------------------------------------------
        !
        ilow = 1
        i = N
        !
        do while (i - ilow>1)
            !
            imid = (i + ilow) / 2
            if (Ss<S(imid)) then
                i = imid
            else
                ilow = imid
            endif
        enddo
        !
        ds = S(i) - S(i - 1)
        t = (Ss - S(i - 1)) / ds
        !
        cx1 = ds * Xs(i - 1) - X(i) + X(i - 1)
        cx2 = ds * Xs(i) - X(i) + X(i - 1)
        xd = X(i) - X(i - 1) + (1.0 - 4.0 * t + 3.0 * t * t) * cx1 + t * (3.0 * t - 2.0) * cx2
        xdd = (6.0 * t - 4.0) * cx1 + (6.0 * t - 2.0) * cx2
        xddd = 6.0 * cx1 + 6.0 * cx2
        !
        cy1 = ds * Ys(i - 1) - Y(i) + Y(i - 1)
        cy2 = ds * Ys(i) - Y(i) + Y(i - 1)
        yd = Y(i) - Y(i - 1) + (1.0 - 4.0 * t + 3.0 * t * t) * cy1 + t * (3.0 * t - 2.0) * cy2
        ydd = (6.0 * t - 4.0) * cy1 + (6.0 * t - 2.0) * cy2
        yddd = 6.0 * cy1 + 6.0 * cy2
        !
        sd = sqrt(xd * xd + yd * yd)
        sd = max(sd, 0.001 * ds)
        !
        bot = sd**3
        dbotdt = 3.0 * sd * (xd * xdd + yd * ydd)
        !
        top = xd * ydd - yd * xdd
        dtopdt = xd * yddd - yd * xddd
        !
        curvs = (dtopdt * bot - dbotdt * top) / bot**2
        !
    end function curvs
    ! CURVS


    subroutine sinvrt(Si, Xi, X, Xs, S, N)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Si, Xi
        real, dimension(N) :: S, X, Xs
        intent (in) N, S, X, Xi, Xs
        intent (inout) Si
        !
        ! Local variables
        !
        real :: ds, res, resp, sisav
        integer :: iter
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-------------------------------------------------------
        !     Calculates the "inverse" spline function S(X).    |
        !     Since S(X) can be multi-valued or not defined,    |
        !     this is not a "black-box" routine.  The calling   |
        !     program must pass via SI a sufficiently good      |
        !     initial guess for S(XI).                          |
        !                                                       |
        !     XI      specified X value       (input)           |
        !     SI      calculated S(XI) value  (input,output)    |
        !     X,XS,S  usual spline arrays     (input)           |
        !                                                       |
        !-------------------------------------------------------
        !
        sisav = Si
        !
        do iter = 1, 10
            res = seval(Si, X, Xs, S, N) - Xi
            resp = deval(Si, X, Xs, S, N)
            ds = -res / resp
            Si = Si + ds
            if (abs(ds / (S(N) - S(1)))<1.0E-5) return
        enddo
        if (show_output) write (*, *) 'SINVRT: spline inversion failed. Input value returned.'
        Si = sisav
        !
    end subroutine sinvrt
    ! SINVRT


    subroutine scalc(X, Y, S, N)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(N) :: S, X, Y
        intent (in) N, X, Y
        intent (inout) S
        !
        ! Local variables
        !
        integer :: i
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !----------------------------------------
        !     Calculates the arc length array S  |
        !     for a 2-D array of points (X,Y).   |
        !----------------------------------------
        !
        S(1) = 0.
        do i = 2, N
            S(i) = S(i - 1) + sqrt((X(i) - X(i - 1))**2 + (Y(i) - Y(i - 1))**2)
        enddo
        !
    end subroutine scalc
    ! SCALC


    subroutine splnxy(X, Xs, Y, Ys, S, N)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        integer, parameter :: KMAX = 32
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(N) :: S, X, Xs, Y, Ys
        intent (inout) S
        !
        ! Local variables
        !
        real :: cx1, cx2, cy1, cy2, ds, dx, dy, serr, sint, sint1, sint2, t
        integer :: i, ipass, k, kk, npass
        real, dimension(0:KMAX) :: xt, yt
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-----------------------------------------
        !     Splines 2-D shape X(S), Y(S), along |
        !     with true arc length parameter S.   |
        !-----------------------------------------
        !
        kk = KMAX
        npass = 10
        !
        !---- set first estimate of arc length parameter
        call scalc(X, Y, S, N)
        !
        !---- spline X(S) and Y(S)
        call segspl(X, Xs, S, N)
        call segspl(Y, Ys, S, N)
        !
        !---- re-integrate true arc length
        do ipass = 1, npass
            !
            serr = 0.
            !
            ds = S(2) - S(1)
            do i = 2, N
                dx = X(i) - X(i - 1)
                dy = Y(i) - Y(i - 1)
                !
                cx1 = ds * Xs(i - 1) - dx
                cx2 = ds * Xs(i) - dx
                cy1 = ds * Ys(i - 1) - dy
                cy2 = ds * Ys(i) - dy
                !
                xt(0) = 0.
                yt(0) = 0.
                do k = 1, kk - 1
                    t = float(k) / float(kk)
                    xt(k) = t * dx + (t - t * t) * ((1.0 - t) * cx1 - t * cx2)
                    yt(k) = t * dy + (t - t * t) * ((1.0 - t) * cy1 - t * cy2)
                enddo
                xt(kk) = dx
                yt(kk) = dy
                !
                sint1 = 0.
                do k = 1, kk
                    sint1 = sint1 + sqrt((xt(k) - xt(k - 1))**2 + (yt(k) - yt(k - 1))**2)
                enddo
                !
                sint2 = 0.
                do k = 2, kk, 2
                    sint2 = sint2 + sqrt((xt(k) - xt(k - 2))**2 + (yt(k) - yt(k - 2))**2)
                enddo
                !
                sint = (4.0 * sint1 - sint2) / 3.0
                !
                if (abs(sint - ds)>abs(serr)) serr = sint - ds
                !
                if (i<N) ds = S(i + 1) - S(i)
                !
                S(i) = S(i - 1) + sqrt(sint)
            enddo
            !
            serr = serr / (S(N) - S(1))
            if (show_output) write (*, *) ipass, serr
            !
            !------ re-spline X(S) and Y(S)
            call segspl(X, Xs, S, N)
            call segspl(Y, Ys, S, N)
            !
            if (abs(serr)<1.0E-7) return
            !
        enddo
        !
    end subroutine splnxy
    ! SPLNXY



    subroutine segspl(X, Xs, S, N)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(N) :: S, X, Xs
        intent (in) N
        !
        ! Local variables
        !
        integer :: iseg, iseg0, nseg
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-----------------------------------------------
        !     Splines X(S) array just like SPLINE,      |
        !     but allows derivative discontinuities     |
        !     at segment joints.  Segment joints are    |
        !     defined by identical successive S values. |
        !-----------------------------------------------
        !
        if (S(1)==S(2)) stop 'SEGSPL:  First input point duplicated'
        if (S(N)==S(N - 1)) stop 'SEGSPL:  Last  input point duplicated'
        !
        iseg0 = 1
        do iseg = 2, N - 2
            if (S(iseg)==S(iseg + 1)) then
                nseg = iseg - iseg0 + 1
                call splind(X(iseg0), Xs(iseg0), S(iseg0), nseg, -999.0, -999.0)
                iseg0 = iseg + 1
            endif
        enddo
        !
        nseg = N - iseg0 + 1
        call splind(X(iseg0), Xs(iseg0), S(iseg0), nseg, -999.0, -999.0)
        !
    end subroutine segspl
    ! SEGSPL



    subroutine segspld(X, Xs, S, N, Xs1, Xs2)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Xs1, Xs2
        real, dimension(N) :: S, X, Xs
        intent (in) N
        !
        ! Local variables
        !
        integer :: iseg, iseg0, nseg
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-----------------------------------------------
        !     Splines X(S) array just like SPLIND,      |
        !     but allows derivative discontinuities     |
        !     at segment joints.  Segment joints are    |
        !     defined by identical successive S values. |
        !-----------------------------------------------
        !
        if (S(1)==S(2)) stop 'SEGSPL:  First input point duplicated'
        if (S(N)==S(N - 1)) stop 'SEGSPL:  Last  input point duplicated'
        !
        iseg0 = 1
        do iseg = 2, N - 2
            if (S(iseg)==S(iseg + 1)) then
                nseg = iseg - iseg0 + 1
                call splind(X(iseg0), Xs(iseg0), S(iseg0), nseg, Xs1, Xs2)
                iseg0 = iseg + 1
            endif
        enddo
        !
        nseg = N - iseg0 + 1
        call splind(X(iseg0), Xs(iseg0), S(iseg0), nseg, Xs1, Xs2)
        !
    end subroutine segspld
    ! SEGSPL
end module m_spline
