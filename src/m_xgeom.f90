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

!*==LEFIND.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! GETXYF

module m_xgeom
contains
    subroutine lefind(Sle, X, Xp, Y, Yp, S, N)
        use i_xfoil, only: show_output
        use m_spline, only: d2val, seval, deval
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Sle
        real, dimension(*) :: S, X, Xp, Y, Yp
        intent (in) N, S, X, Xp, Y, Yp
        intent (inout) Sle
        !
        ! Local variables
        !
        real :: dotp, dseps, dsle, dx, dxdd, dxds, dxte, dy, dydd, dyds, dyte, res, ress, xchord, xle, xte, &
                & ychord, yle, yte
        integer :: i, iter
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
        !------------------------------------------------------
        !     Locates leading edge spline-parameter value SLE
        !
        !     The defining condition is
        !
        !      (X-XTE,Y-YTE) . (X',Y') = 0     at  S = SLE
        !
        !     i.e. the surface tangent is normal to the chord
        !     line connecting X(SLE),Y(SLE) and the TE point.
        !------------------------------------------------------
        !
        !---- convergence tolerance
        dseps = (S(N) - S(1)) * 1.0E-5
        !
        !---- set trailing edge point coordinates
        xte = 0.5 * (X(1) + X(N))
        yte = 0.5 * (Y(1) + Y(N))
        !
        !---- get first guess for SLE
        do i = 3, N - 2
            dxte = X(i) - xte
            dyte = Y(i) - yte
            dx = X(i + 1) - X(i)
            dy = Y(i + 1) - Y(i)
            dotp = dxte * dx + dyte * dy
            if (dotp<0.0) exit
        enddo
        !
        Sle = S(i)
        !
        !---- check for sharp LE case
        !cc        WRITE(*,*) 'Sharp LE found at ',I,SLE
        if (S(i)==S(i - 1)) return
        !
        !---- Newton iteration to get exact SLE value
        do iter = 1, 50
            xle = seval(Sle, X, Xp, S, N)
            yle = seval(Sle, Y, Yp, S, N)
            dxds = deval(Sle, X, Xp, S, N)
            dyds = deval(Sle, Y, Yp, S, N)
            dxdd = d2val(Sle, X, Xp, S, N)
            dydd = d2val(Sle, Y, Yp, S, N)
            !
            xchord = xle - xte
            ychord = yle - yte
            !
            !------ drive dot product between chord line and LE tangent to zero
            res = xchord * dxds + ychord * dyds
            ress = dxds * dxds + dyds * dyds + xchord * dxdd + ychord * dydd
            !
            !------ Newton delta for SLE
            dsle = -res / ress
            !
            dsle = max(dsle, -0.02 * abs(xchord + ychord))
            dsle = min(dsle, 0.02 * abs(xchord + ychord))
            Sle = Sle + dsle
            if (abs(dsle)<dseps) return
        enddo
        if (show_output) write (*, *) 'LEFIND:  LE point not found.  Continuing...'
        Sle = S(i)
    end subroutine lefind
    !*==SOPPS.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine sopps(Sopp, Si, X, Xp, Y, Yp, S, N, Sle)
        use i_xfoil, only: show_output
        use m_spline, only: seval, deval
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Si, Sle, Sopp
        real, dimension(*) :: S, X, Xp, Y, Yp
        intent (in) N, S, Si, Sle, X, Xp, Y, Yp
        intent (inout) Sopp
        !
        ! Local variables
        !
        real :: chord, dsopp, dxc, dyc, res, resd, sfrac, slen, xbar, xi, xle, xopp, xoppd, xte, yi, yle, &
                & yopp, yoppd, yte
        integer :: in, inopp, iter
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
        !     Calculates arc length SOPP of point
        !     which is opposite of point SI, on the
        !     other side of the airfoil baseline
        !--------------------------------------------------
        !
        !---- reference length for testing convergence
        slen = S(N) - S(1)
        !
        !---- set chordline vector
        xle = seval(Sle, X, Xp, S, N)
        yle = seval(Sle, Y, Yp, S, N)
        xte = 0.5 * (X(1) + X(N))
        yte = 0.5 * (Y(1) + Y(N))
        chord = sqrt((xte - xle)**2 + (yte - yle)**2)
        dxc = (xte - xle) / chord
        dyc = (yte - yle) / chord
        !
        if (Si<Sle) then
            in = 1
            inopp = N
        else
            in = N
            inopp = 1
        endif
        sfrac = (Si - Sle) / (S(in) - Sle)
        Sopp = Sle + sfrac * (S(inopp) - Sle)
        !
        if (abs(sfrac)<=1.0E-5) then
            Sopp = Sle
            return
        endif
        !
        !---- XBAR = x coordinate in chord-line axes
        xi = seval(Si, X, Xp, S, N)
        yi = seval(Si, Y, Yp, S, N)
        xle = seval(Sle, X, Xp, S, N)
        yle = seval(Sle, Y, Yp, S, N)
        xbar = (xi - xle) * dxc + (yi - yle) * dyc
        !
        !---- converge on exact opposite point with same XBAR value
        do iter = 1, 12
            xopp = seval(Sopp, X, Xp, S, N)
            yopp = seval(Sopp, Y, Yp, S, N)
            xoppd = deval(Sopp, X, Xp, S, N)
            yoppd = deval(Sopp, Y, Yp, S, N)
            !
            res = (xopp - xle) * dxc + (yopp - yle) * dyc - xbar
            resd = xoppd * dxc + yoppd * dyc
            !
            if (abs(res) / slen<1.0E-5) goto 99999
            if (resd==0.0) exit
            !
            dsopp = -res / resd
            Sopp = Sopp + dsopp
            !
            if (abs(dsopp) / slen<1.0E-5) goto 99999
        enddo
        if (show_output) write (*, *) 'SOPPS: Opposite-point location failed. Continuing...'
        Sopp = Sle + sfrac * (S(inopp) - Sle)
        !
    99999 end subroutine sopps
    !*==NORM.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! SOPPS



    subroutine norm(X, Xp, Y, Yp, S, N)
        use m_spline, only: seval, segspl, scalc
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(*) :: S, X, Xp, Y, Yp
        intent (inout) S, X, Y
        !
        ! Local variables
        !
        real :: fudge, sle, xmax, xmin, ymin
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
        !-----------------------------------------------
        !     Scales coordinates to get unit chord
        !-----------------------------------------------
        !
        call scalc(X, Y, S, N)
        call segspl(X, Xp, S, N)
        call segspl(Y, Yp, S, N)
        !
        call lefind(sle, X, Xp, Y, Yp, S, N)
        !
        xmax = 0.5 * (X(1) + X(N))
        xmin = seval(sle, X, Xp, S, N)
        ymin = seval(sle, Y, Yp, S, N)
        !
        fudge = 1.0 / (xmax - xmin)
        do i = 1, N
            X(i) = (X(i) - xmin) * fudge
            Y(i) = (Y(i) - ymin) * fudge
            S(i) = S(i) * fudge
        enddo
        !
    end subroutine norm
    !*==GEOPAR.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine geopar(X, Xp, Y, Yp, S, N, T, &
            Sle, Chord, Area, Radle, Angte, Ei11a, Ei22a, Apx1a, Apx2a, Ei11t, Ei22t, Apx1t, Apx2t, Thick, &
            & Cambr)
        use i_xfoil, only: show_output
        use m_spline, only: curv, seval
        use m_xutils, only: atanc
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        integer, parameter :: IBX = 600
        !
        ! Dummy arguments
        !
        real :: Angte, Apx1a, Apx1t, Apx2a, Apx2t, Area, Cambr, Chord, Ei11a, Ei11t, Ei22a, Ei22t, Radle, Sle, &
                & Thick
        integer :: N
        real, dimension(*) :: S, T, X, Xp, Y, Yp
        intent (out) Angte, Chord, Radle
        !
        ! Local variables
        !
        real :: ang1, ang2, chsq, curvle, slen, xcambr, xcena, xcent, xle, xte, xthick, ycena, ycent, yle, yte
        integer :: i
        real, dimension(2 * IBX) :: xcam, xthk, ycam, ycamp, ythk, ythkp
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
        !
        !------------------------------------------------------
        !     Sets geometric parameters for airfoil shape
        !------------------------------------------------------
        call lefind(Sle, X, Xp, Y, Yp, S, N)
        !
        xle = seval(Sle, X, Xp, S, N)
        yle = seval(Sle, Y, Yp, S, N)
        xte = 0.5 * (X(1) + X(N))
        yte = 0.5 * (Y(1) + Y(N))
        !
        chsq = (xte - xle)**2 + (yte - yle)**2
        Chord = sqrt(chsq)
        !
        curvle = curv(Sle, X, Xp, Y, Yp, S, N)
        !
        Radle = 0.0
        if (abs(curvle)>0.001 * (S(N) - S(1))) Radle = 1.0 / curvle
        !
        ang1 = atan2(-Yp(1), -Xp(1))
        ang2 = atanc(Yp(N), Xp(N), ang1)
        Angte = ang2 - ang1
        !

        do i = 1, N
            T(i) = 1.0
        enddo
        !
        call aecalc(N, X, Y, T, 1, Area, xcena, ycena, Ei11a, Ei22a, Apx1a, Apx2a)
        !
        call aecalc(N, X, Y, T, 2, slen, xcent, ycent, Ei11t, Ei22t, Apx1t, Apx2t)
        !
        !--- Old, approximate thickness,camber routine (on discrete points only)
        call tccalc(X, Xp, Y, Yp, S, N, Thick, xthick, Cambr, xcambr)
        !
        !--- More accurate thickness and camber estimates
        !c      CALL GETCAM(XCAM,YCAM,NCAM,XTHK,YTHK,NTHK,
        !c     &            X,XP,Y,YP,S,N )
        !c      CALL GETMAX(XCAM,YCAM,YCAMP,NCAM,XCAMBR,CAMBR)
        !c      CALL GETMAX(XTHK,YTHK,YTHKP,NTHK,XTHICK,THICK)
        !c      THICK = 2.0*THICK
        !
        if (show_output) write (*, 99001) Thick, xthick, Cambr, xcambr
        99001 format (' Max thickness = ', f12.6, '  at x = ', f7.3, /' Max camber    = ', f12.6, '  at x = ', f7.3)


        !
    end subroutine geopar
    !*==AECALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! GEOPAR


    subroutine aecalc(N, X, Y, T, Itype, Area, Xcen, Ycen, Ei11, Ei22, Apx1, Apx2)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Apx1, Apx2, Area, Ei11, Ei22, Xcen, Ycen
        integer :: Itype, N
        real, dimension(*) :: T, X, Y
        intent (in) Itype, N, T, X, Y
        intent (out) Area
        intent (inout) Apx1, Apx2, Ei11, Ei22, Xcen, Ycen
        !
        ! Local variables
        !
        real :: aint, c1, c2, da, ds, dx, dy, eisq, eixx, eixy, eiyy, s1, s2, sgn, sint, ta, xa, xint, &
                & xxint, xyint, ya, yint, yyint
        integer :: io, ip
        real, save :: pi
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
        !---------------------------------------------------------------
        !     Calculates geometric properties of shape X,Y
        !
        !     Input:
        !       N      number of points
        !       X(.)   shape coordinate point arrays
        !       Y(.)
        !       T(.)   skin-thickness array, used only if ITYPE = 2
        !       ITYPE  = 1 ...   integration is over whole area  dx dy
        !              = 2 ...   integration is over skin  area   t ds
        !
        !     Output:
        !       XCEN,YCEN  centroid location
        !       EI11,EI22  principal moments of inertia
        !       APX1,APX2  principal-axis angles
        !---------------------------------------------------------------
        data pi/3.141592653589793238/
        !
        sint = 0.0
        aint = 0.0
        xint = 0.0
        yint = 0.0
        xxint = 0.0
        xyint = 0.0
        yyint = 0.0
        !
        do io = 1, N
            if (io==N) then
                ip = 1
            else
                ip = io + 1
            endif
            !
            dx = X(io) - X(ip)
            dy = Y(io) - Y(ip)
            xa = (X(io) + X(ip)) * 0.50
            ya = (Y(io) + Y(ip)) * 0.50
            ta = (T(io) + T(ip)) * 0.50
            !
            ds = sqrt(dx * dx + dy * dy)
            sint = sint + ds

            if (Itype==1) then
                !-------- integrate over airfoil cross-section
                da = ya * dx
                aint = aint + da
                xint = xint + xa * da
                yint = yint + ya * da / 2.0
                xxint = xxint + xa * xa * da
                xyint = xyint + xa * ya * da / 2.0
                yyint = yyint + ya * ya * da / 3.0
            else
                !-------- integrate over skin thickness
                da = ta * ds
                aint = aint + da
                xint = xint + xa * da
                yint = yint + ya * da
                xxint = xxint + xa * xa * da
                xyint = xyint + xa * ya * da
                yyint = yyint + ya * ya * da
            endif
            !
        enddo
        !
        Area = aint
        !
        if (aint==0.0) then
            Xcen = 0.0
            Ycen = 0.0
            Ei11 = 0.0
            Ei22 = 0.0
            Apx1 = 0.0
            Apx2 = atan2(1.0, 0.0)
            return
        endif
        !
        !
        !---- calculate centroid location
        Xcen = xint / aint
        Ycen = yint / aint
        !
        !---- calculate inertias
        eixx = yyint - Ycen * Ycen * aint
        eixy = xyint - Xcen * Ycen * aint
        eiyy = xxint - Xcen * Xcen * aint
        !
        !---- set principal-axis inertias, EI11 is closest to "up-down" bending inertia
        eisq = 0.25 * (eixx - eiyy)**2 + eixy**2
        sgn = sign(1.0, eiyy - eixx)
        Ei11 = 0.5 * (eixx + eiyy) - sgn * sqrt(eisq)
        Ei22 = 0.5 * (eixx + eiyy) + sgn * sqrt(eisq)
        !
        if (Ei11==0.0 .or. Ei22==0.0) then
            !----- vanishing section stiffness
            Apx1 = 0.0
            Apx2 = atan2(1.0, 0.0)
            !
        elseif (eisq / (Ei11 * Ei22)<(0.001 * sint)**4) then
            !----- rotationally-invariant section (circle, square, etc.)
            Apx1 = 0.0
            Apx2 = atan2(1.0, 0.0)
            !
        else
            !----- normal airfoil section
            c1 = eixy
            s1 = eixx - Ei11
            !
            c2 = eixy
            s2 = eixx - Ei22
            !
            if (abs(s1)>abs(s2)) then
                Apx1 = atan2(s1, c1)
                Apx2 = Apx1 + 0.5 * pi
            else
                Apx2 = atan2(s2, c2)
                Apx1 = Apx2 - 0.5 * pi
            endif

            if (Apx1<-0.5 * pi) Apx1 = Apx1 + pi
            if (Apx1>+0.5 * pi) Apx1 = Apx1 - pi
            if (Apx2<-0.5 * pi) Apx2 = Apx2 + pi
            if (Apx2>+0.5 * pi) Apx2 = Apx2 - pi
            !
        endif
        !
    end subroutine aecalc
    !*==TCCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! AECALC



    subroutine tccalc(X, Xp, Y, Yp, S, N, Thick, Xthick, Cambr, Xcambr)
        use m_spline, only: seval
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Cambr, Thick, Xcambr, Xthick
        integer :: N
        real, dimension(*) :: S, X, Xp, Y, Yp
        intent (out) Xcambr, Xthick
        intent (inout) Cambr, Thick
        !
        ! Local variables
        !
        real :: chord, dxc, dyc, sle, sopp, xbar, xle, xopp, xte, ybar, ybarop, yc, yle, yopp, yt, yte
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
        !---------------------------------------------------------------
        !     Calculates max thickness and camber at airfoil points
        !
        !     Note: this routine does not find the maximum camber or
        !           thickness exactly as it only looks at discrete points
        !
        !     Input:
        !       N      number of points
        !       X(.)   shape coordinate point arrays
        !       Y(.)
        !
        !     Output:
        !       THICK  max thickness
        !       CAMBR  max camber
        !---------------------------------------------------------------
        call lefind(sle, X, Xp, Y, Yp, S, N)
        xle = seval(sle, X, Xp, S, N)
        yle = seval(sle, Y, Yp, S, N)
        xte = 0.5 * (X(1) + X(N))
        yte = 0.5 * (Y(1) + Y(N))
        chord = sqrt((xte - xle)**2 + (yte - yle)**2)
        !
        !---- set unit chord-line vector
        dxc = (xte - xle) / chord
        dyc = (yte - yle) / chord
        !
        Thick = 0.
        Xthick = 0.
        Cambr = 0.
        Xcambr = 0.
        !
        !---- go over each point, finding the y-thickness and camber
        do i = 1, N
            xbar = (X(i) - xle) * dxc + (Y(i) - yle) * dyc
            ybar = (Y(i) - yle) * dxc - (X(i) - xle) * dyc
            !
            !------ set point on the opposite side with the same chord x value
            call sopps(sopp, S(i), X, Xp, Y, Yp, S, N, sle)
            xopp = seval(sopp, X, Xp, S, N)
            yopp = seval(sopp, Y, Yp, S, N)
            !
            ybarop = (yopp - yle) * dxc - (xopp - xle) * dyc
            !
            yc = 0.5 * (ybar + ybarop)
            yt = abs(ybar - ybarop)
            !
            if (abs(yc)>abs(Cambr)) then
                Cambr = yc
                Xcambr = xopp
            endif
            if (abs(yt)>abs(Thick)) then
                Thick = yt
                Xthick = xopp
            endif
        enddo
        !
    end subroutine tccalc
    !*==CANG.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! TCCALC




    subroutine cang(X, Y, N, Iprint, Imax, Amax)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Amax
        integer :: Imax, Iprint, N
        real, dimension(*) :: X, Y
        intent (in) Iprint, N, X, Y
        intent (inout) Amax, Imax
        !
        ! Local variables
        !
        real :: angl, crossp, dx1, dx2, dy1, dy2
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
        !-------------------------------------------------------------------
        !     IPRINT=2:   Displays all panel node corner angles
        !     IPRINT=1:   Displays max panel node corner angle
        !     IPRINT=0:   No display... just returns values
        !-------------------------------------------------------------------
        !
        Amax = 0.0
        Imax = 1
        !
        !---- go over each point, calculating corner angle
        if (Iprint==2 .and. show_output) write (*, 99001)
        !
        99001 format (/'  i       x        y      angle')
        do i = 2, N - 1
            dx1 = X(i) - X(i - 1)
            dy1 = Y(i) - Y(i - 1)
            dx2 = X(i) - X(i + 1)
            dy2 = Y(i) - Y(i + 1)
            !
            !------ allow for doubled points
            if (dx1==0.0 .and. dy1==0.0) then
                dx1 = X(i) - X(i - 2)
                dy1 = Y(i) - Y(i - 2)
            endif
            if (dx2==0.0 .and. dy2==0.0) then
                dx2 = X(i) - X(i + 2)
                dy2 = Y(i) - Y(i + 2)
            endif
            !
            crossp = (dx2 * dy1 - dy2 * dx1) / sqrt((dx1**2 + dy1**2) * (dx2**2 + dy2**2))
            angl = asin(crossp) * (180.0 / 3.1415926)
            if (Iprint==2 .and. show_output) write (*, 99002) i, X(i), Y(i), angl
            !CC             120   0.2134  -0.0234   25.322
            99002 format (1x, i3, 2F9.4, f9.3)
            if (abs(angl)>abs(Amax)) then
                Amax = angl
                Imax = i
            endif
        enddo
        !
        if (Iprint>=1 .and. show_output) write (*, 99003) Amax, Imax, X(Imax), Y(Imax)
        99003 format (/' Maximum panel corner angle =', f7.3, '   at  i,x,y  = ', i3, 2F9.4)
        !
    end subroutine cang
    !*==INTER.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! CANG



    subroutine inter(X0, Xp0, Y0, Yp0, S0, N0, Sle0, X1, Xp1, Y1, Yp1, S1, N1, Sle1, X, Y, N, Frac)
        use m_spline, only: seval
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Frac, Sle0, Sle1
        integer :: N, N0, N1
        real, dimension(N0) :: S0, X0, Xp0, Y0, Yp0
        real, dimension(N1) :: S1, X1, Xp1, Y1, Yp1
        real, dimension(*) :: X, Y
        intent (in) Frac, N0, N1, S0, S1, Sle0, Sle1, X0, X1, Xp0, Xp1, Y0, Y1, Yp0, Yp1
        intent (out) X, Y
        intent (inout) N
        !
        ! Local variables
        !
        real :: bots0, bots1, f0, f1, sn, st0, st1, tops0, tops1, xt0, xt1, yt0, yt1
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
        !     .....................................................................
        !
        !     Interpolates two source airfoil shapes into an "intermediate" shape.
        !
        !     Procedure:
        !        The interpolated x coordinate at a given normalized spline
        !        parameter value is a weighted average of the two source
        !        x coordinates at the same normalized spline parameter value.
        !        Ditto for the y coordinates. The normalized spline parameter
        !        runs from 0 at the leading edge to 1 at the trailing edge on
        !        each surface.
        !     .....................................................................
        !
        !
        !---- number of points in interpolated airfoil is the same as in airfoil 0
        N = N0
        !
        !---- interpolation weighting fractions
        f0 = 1.0 - Frac
        f1 = Frac
        !
        !---- top side spline parameter increments
        tops0 = S0(1) - Sle0
        tops1 = S1(1) - Sle1
        !
        !---- bottom side spline parameter increments
        bots0 = S0(N0) - Sle0
        bots1 = S1(N1) - Sle1
        !
        do i = 1, N
            !
            !------ normalized spline parameter is taken from airfoil 0 value
            if (S0(i)<Sle0) sn = (S0(i) - Sle0) / tops0             ! top side
            if (S0(i)>=Sle0) sn = (S0(i) - Sle0) / bots0            ! bottom side
            !
            !------ set actual spline parameters
            st0 = S0(i)
            if (st0<Sle0) st1 = Sle1 + tops1 * sn
            if (st0>=Sle0) st1 = Sle1 + bots1 * sn
            !
            !------ set input coordinates at common spline parameter location
            xt0 = seval(st0, X0, Xp0, S0, N0)
            yt0 = seval(st0, Y0, Yp0, S0, N0)
            xt1 = seval(st1, X1, Xp1, S1, N1)
            yt1 = seval(st1, Y1, Yp1, S1, N1)
            !
            !------ set interpolated x,y coordinates
            X(i) = f0 * xt0 + f1 * xt1
            Y(i) = f0 * yt0 + f1 * yt1
            !
        enddo
        !
    end subroutine inter
    !*==INTERX.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! INTER



    subroutine interx(X0, Xp0, Y0, Yp0, S0, N0, Sle0, X1, Xp1, Y1, Yp1, S1, N1, Sle1, X, Y, N, Frac)
        use m_spline, only: seval, sinvrt
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Frac, Sle0, Sle1
        integer :: N, N0, N1
        real, dimension(N0) :: S0, X0, Xp0, Y0, Yp0
        real, dimension(N1) :: S1, X1, Xp1, Y1, Yp1
        real, dimension(N) :: X, Y
        intent (in) Frac, Sle0, Sle1, Y0, Y1, Yp0, Yp1
        intent (out) X, Y
        intent (inout) N
        !
        ! Local variables
        !
        real :: f0, f1, st0, st1, xle0, xle1, xn, xt0, xt1, yt0, yt1
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
        !     .....................................................................
        !
        !     Interpolates two source airfoil shapes into an "intermediate" shape.
        !
        !     Procedure:
        !        The interpolated x coordinate at a given normalized spline
        !        parameter value is a weighted average of the two source
        !        x coordinates at the same normalized spline parameter value.
        !        Ditto for the y coordinates. The normalized spline parameter
        !        runs from 0 at the leading edge to 1 at the trailing edge on
        !        each surface.
        !     .....................................................................
        !
        !
        !---- number of points in interpolated airfoil is the same as in airfoil 0
        N = N0
        !
        !---- interpolation weighting fractions
        f0 = 1.0 - Frac
        f1 = Frac
        !
        xle0 = seval(Sle0, X0, Xp0, S0, N0)
        xle1 = seval(Sle1, X1, Xp1, S1, N1)
        !
        do i = 1, N
            !
            !------ normalized x parameter is taken from airfoil 0 value
            if (S0(i)<Sle0) xn = (X0(i) - xle0) / (X0(1) - xle0)
            if (S0(i)>=Sle0) xn = (X0(i) - xle0) / (X0(N0) - xle0)
            !
            !------ set target x and initial spline parameters
            xt0 = X0(i)
            st0 = S0(i)
            if (st0<Sle0) then
                xt1 = xle1 + (X1(1) - xle1) * xn
                st1 = Sle1 + (S1(1) - Sle1) * xn
            else
                xt1 = xle1 + (X1(N1) - xle1) * xn
                st1 = Sle1 + (S1(N1) - Sle1) * xn
            endif
            !
            call sinvrt(st0, xt0, X0, Xp0, S0, N0)
            call sinvrt(st1, xt1, X1, Xp1, S1, N1)
            !
            !------ set input coordinates at common spline parameter location
            xt0 = seval(st0, X0, Xp0, S0, N0)
            yt0 = seval(st0, Y0, Yp0, S0, N0)
            xt1 = seval(st1, X1, Xp1, S1, N1)
            yt1 = seval(st1, Y1, Yp1, S1, N1)
            !
            !------ set interpolated x,y coordinates
            X(i) = f0 * xt0 + f1 * xt1
            Y(i) = f0 * yt0 + f1 * yt1
            !
        enddo
        !
    end subroutine interx
    !*==BENDUMP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! INTERX





    subroutine bendump(N, X, Y)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(*) :: X, Y
        !
        ! Local variables
        !
        real :: aixx, aixxt, aiyy, aiyyt, aj, ajt, area, pex, slen, xbar, xbart, xc, xct, xexint, xmax, xmin, &
                & ybar, ybart, yc, yct, yexint, ymax, ymin
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
        !
        pex = 16.0
        call ijsect(N, X, Y, pex, area, slen, &
                xmin, xmax, xexint, ymin, ymax, yexint, xc, yc, xct, yct, aixx, aiyy, aixxt, aiyyt, aj, ajt)
        !      CALL IJSECT(N,X,Y, PEX,
        !     &    AREA, SLEN,
        !     &    XC, XMIN, XMAX, XEXINT,
        !     &    YC, YMIN, YMAX, YEXINT,
        !     &    AIXX, AIXXT,
        !     &    AIYY, AIYYT,
        !     &    AJ  , AJT   )
        !
        if (show_output) then
            write (*, *)
            write (*, 99001) 'Area =', area
            write (*, 99001) 'Slen =', slen
            write (*, *)
            write (*, 99001) 'X-bending parameters(solid):'
            write (*, 99001) '        Xc =', xc
            write (*, 99001) '  max X-Xc =', xmax - xc
            write (*, 99001) '  min X-Xc =', xmin - xc
            write (*, 99001) '       Iyy =', aiyy
        endif
        xbar = max(abs(xmax - xc), abs(xmin - xc))
        if (show_output) then
            write (*, 99001) ' Iyy/(X-Xc)=', aiyy / xbar
            write (*, *)
            write (*, 99001) 'Y-bending parameters(solid):'
            write (*, 99001) '        Yc =', yc
            write (*, 99001) '  max Y-Yc =', ymax - yc
            write (*, 99001) '  min Y-Yc =', ymin - yc
            write (*, 99001) '       Ixx =', aixx
        endif
        ybar = max(abs(ymax - yc), abs(ymin - yc))
        if (show_output) then
            write (*, 99001) ' Ixx/(Y-Yc)=', aixx / ybar
            write (*, *)
            write (*, 99001) '       J   =', aj
            !
            write (*, *)
            write (*, *)
            write (*, 99001) 'X-bending parameters(skin):'
            write (*, 99001) '         Xc =', xct
            write (*, 99001) '   max X-Xc =', xmax - xct
            write (*, 99001) '   min X-Xc =', xmin - xct
            write (*, 99001) '      Iyy/t =', aiyyt
        endif
        xbart = max(abs(xmax - xct), abs(xmin - xct))
        if (show_output) then
            write (*, 99001) ' Iyy/t(X-Xc)=', aiyyt / xbart
            write (*, *)
            write (*, 99001) 'Y-bending parameters(skin):'
            write (*, 99001) '         Yc =', yct
            write (*, 99001) '   max Y-Yc =', ymax - yct
            write (*, 99001) '   min Y-Yc =', ymin - yct
            write (*, 99001) '      Ixx/t =', aixxt
        endif
        ybart = max(abs(ymax - yct), abs(ymin - yct))
        if (show_output) then
            write (*, 99001) ' Ixx/t(Y-Yc)=', aixxt / ybart
            write (*, *)
            write (*, 99001) '      J/t   =', ajt
        endif
        !
        !      WRITE(*,*)
        !      WRITE(*,1200) '  power-avg X-Xc =', XEXINT
        !      WRITE(*,1200) '  power-avg Y-Yc =', YEXINT
        !
        return
        !
        99001 format (1x, a, g14.6)
    end subroutine bendump
    !*==BENDUMP2.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! BENDUMP



    subroutine bendump2(N, X, Y, T)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(*) :: T, X, Y
        !
        ! Local variables
        !
        real :: aixx, aixxt, aiyy, aiyyt, aj, ajt, apx1a, apx1t, apx2a, apx2t, area, dtr, ei11a, ei11t, ei22a, &
                & ei22t, pex, slen, xbara, xbart, xc, xcena, xcent, xct, xexint, xmax, xmin, ybara, ybart, yc, &
                & ycena, ycent, yct, yexint, ymax, ymin
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
        !
        dtr = atan(1.0) / 45.0
        !
        pex = 16.0
        call ijsect(N, X, Y, pex, area, slen, &
                xmin, xmax, xexint, ymin, ymax, yexint, xc, yc, xct, yct, aixx, aiyy, aixxt, aiyyt, aj, ajt)
        !      CALL IJSECT(N,X,Y, PEX,
        !     &    AREA, SLEN,
        !     &    XC, XMIN, XMAX, XEXINT,
        !     &    YC, YMIN, YMAX, YEXINT,
        !     &    AIXX, AIXXT,
        !     &    AIYY, AIYYT,
        !     &    AJ  , AJT   )
        !
        !
        call aecalc(N, X, Y, T, 1, area, xcena, ycena, ei11a, ei22a, apx1a, apx2a)
        !
        call aecalc(N, X, Y, T, 2, slen, xcent, ycent, ei11t, ei22t, apx1t, apx2t)
        !

        if (show_output) then
            write (*, *)
            write (*, 99001) 'Area =', area
            write (*, 99001) 'Slen =', slen
            write (*, *)
            write (*, 99001) 'X-bending parameters:'
            write (*, 99001) 'solid centroid Xc=', xcena
            write (*, 99001) 'skin  centroid Xc=', xcent
            write (*, 99001) ' solid max X-Xc  =', xmax - xcena
            write (*, 99001) ' solid min X-Xc  =', xmin - xcena
            write (*, 99001) ' skin  max X-Xc  =', xmax - xcent
            write (*, 99001) ' skin  min X-Xc  =', xmin - xcent
            write (*, 99001) '     solid Iyy   =', ei22a
            write (*, 99001) '     skin  Iyy/t =', ei22t
        endif
        xbara = max(abs(xmax - xcena), abs(xmin - xcena))
        xbart = max(abs(xmax - xcent), abs(xmin - xcent))
        if (show_output) then
            write (*, 99001) ' solid Iyy/(X-Xc)=', ei22a / xbara
            write (*, 99001) ' skin Iyy/t(X-Xc)=', ei22t / xbart
            !
            write (*, *)
            write (*, 99001) 'Y-bending parameters:'
            write (*, 99001) 'solid centroid Yc=', ycena
            write (*, 99001) 'skin  centroid Yc=', ycent
            write (*, 99001) ' solid max Y-Yc  =', ymax - ycena
            write (*, 99001) ' solid min Y-Yc  =', ymin - ycena
            write (*, 99001) ' skin  max Y-Yc  =', ymax - ycent
            write (*, 99001) ' skin  min Y-Yc  =', ymin - ycent
            write (*, 99001) '     solid Ixx   =', ei11a
            write (*, 99001) '     skin  Ixx/t =', ei11t
        endif
        ybara = max(abs(ymax - ycena), abs(ymin - ycena))
        ybart = max(abs(ymax - ycent), abs(ymin - ycent))
        if (show_output) then
            write (*, 99001) ' solid Ixx/(Y-Yc)=', ei11a / ybara
            write (*, 99001) ' skin Ixx/t(Y-Yc)=', ei11t / ybart
            !
            write (*, *)
            write (*, 99001) ' solid principal axis angle (deg ccw) =', apx1a / dtr
            write (*, 99001) ' skin  principal axis angle (deg ccw) =', apx1t / dtr
        endif

        !      WRITE(*,*)
        !      WRITE(*,1200) '  power-avg X-Xc =', XEXINT
        !      WRITE(*,1200) '  power-avg Y-Yc =', YEXINT
        !
        if (show_output) then
            write (*, *)
            write (*, 99001) '    solid J     =', aj
            write (*, 99001) '    skin  J/t   =', ajt
        endif
        return
        !
        99001 format (1x, a, g14.6)
    end subroutine bendump2
    !*==IJSECT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! BENDUMP2



    subroutine ijsect(N, X, Y, Pex, Area, Slen, &
            Xmin, Xmax, Xexint, Ymin, Ymax, Yexint, Xc, Yc, Xct, Yct, Aixx, Aiyy, Aixxt, Aiyyt, Aj, Ajt)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Aixx, Aixxt, Aiyy, Aiyyt, Aj, Ajt, Area, Pex, Slen, Xc, Xct, Xexint, Xmax, Xmin, Yc, Yct, &
                & Yexint, Ymax, Ymin
        integer :: N
        real, dimension(*) :: X, Y
        intent (in) N, X, Y
        intent (out) Aixx, Aixxt, Aiyy, Aiyyt, Ajt
        intent (inout) Aj, Area, Slen, Xc, Xct, Xmax, Xmin, Yc, Yct, Ymax, Ymin
        !
        ! Local variables
        !
        real :: c_ds, ds, dx, dy, frac, sint, xavg, xint, xxx_dy, xx_ds, xx_dy, x_ds, x_dy, yavg, yint, yopp, &
                & yyy_dx, yy_ds, yy_dx, y_ds, y_dx
        integer :: i, imid, j
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
        !
        Xmin = X(1)
        Xmax = X(1)
        Ymin = Y(1)
        Ymax = Y(1)
        !
        dx = X(1) - X(N)
        dy = Y(1) - Y(N)
        ds = sqrt(dx * dx + dy * dy)
        xavg = 0.5 * (X(1) + X(N))
        yavg = 0.5 * (Y(1) + Y(N))
        !
        x_dy = dy * xavg
        xx_dy = dy * xavg**2
        xxx_dy = dy * xavg**3
        x_ds = ds * xavg
        xx_ds = ds * xavg**2
        !
        y_dx = dx * yavg
        yy_dx = dx * yavg**2
        yyy_dx = dx * yavg**3
        y_ds = ds * yavg
        yy_ds = ds * yavg**2
        !
        c_ds = ds
        !
        do i = 2, N
            dx = X(i) - X(i - 1)
            dy = Y(i) - Y(i - 1)
            ds = sqrt(dx * dx + dy * dy)
            xavg = 0.5 * (X(i) + X(i - 1))
            yavg = 0.5 * (Y(i) + Y(i - 1))
            !
            x_dy = x_dy + dy * xavg
            xx_dy = xx_dy + dy * xavg**2
            xxx_dy = xxx_dy + dy * xavg**3
            x_ds = x_ds + ds * xavg
            xx_ds = xx_ds + ds * xavg**2
            !
            y_dx = y_dx + dx * yavg
            yy_dx = yy_dx + dx * yavg**2
            yyy_dx = yyy_dx + dx * yavg**3
            y_ds = y_ds + ds * yavg
            yy_ds = yy_ds + ds * yavg**2
            !
            c_ds = c_ds + ds
            !
            Xmin = min(Xmin, X(i))
            Xmax = max(Xmax, X(i))
            Ymin = min(Ymin, Y(i))
            Ymax = max(Ymax, Y(i))
        enddo
        !
        Area = -y_dx
        Slen = c_ds
        !
        if (Area==0.0) return
        !
        Xc = xx_dy / (2.0 * x_dy)
        Xct = x_ds / c_ds
        Aiyy = xxx_dy / 3.0 - xx_dy * Xc + x_dy * Xc**2
        Aiyyt = xx_ds - x_ds * Xct * 2.0 + c_ds * Xct**2
        !
        Yc = yy_dx / (2.0 * y_dx)
        Yct = y_ds / c_ds
        Aixx = -yyy_dx / 3.0 + yy_dx * Yc - y_dx * Yc**2
        Aixxt = yy_ds - y_ds * Yct * 2.0 + c_ds * Yct**2
        !
        !
        sint = 0.
        xint = 0.
        yint = 0.
        !
        do i = 2, N
            dx = X(i) - X(i - 1)
            dy = Y(i) - Y(i - 1)
            ds = sqrt(dx * dx + dy * dy)
            xavg = 0.5 * (X(i) + X(i - 1)) - Xc
            yavg = 0.5 * (Y(i) + Y(i - 1)) - Yc
            !
            sint = sint + ds
            !c        XINT = XINT + DS * ABS(XAVG)**PEX
            !c        YINT = YINT + DS * ABS(YAVG)**PEX
        enddo
        !
        do i = 1, N - 1
            if (X(i + 1)>=X(i)) goto 100
        enddo
        imid = N / 2
        100  imid = i
        !
        Aj = 0.0
        do i = 2, imid
            xavg = 0.5 * (X(i) + X(i - 1))
            yavg = 0.5 * (Y(i) + Y(i - 1))
            dx = X(i - 1) - X(i)
            !
            if (xavg>X(N)) then
                yopp = Y(N)
                goto 150
            endif
            if (xavg<=X(imid)) then
                yopp = Y(imid)
                goto 150
            endif
            !
            do j = N, imid, -1
                if (xavg>X(j - 1) .and. xavg<=X(j)) then
                    frac = (xavg - X(j - 1)) / (X(j) - X(j - 1))
                    yopp = Y(j - 1) + (Y(j) - Y(j - 1)) * frac
                    exit
                endif
            enddo
            !
            150  Aj = Aj + abs(yavg - yopp)**3 * dx / 3.0
        enddo
        !
        Ajt = 4.0 * Area**2 / Slen
        !
        !c      XEXINT = (XINT/SINT)**(1.0/PEX)
        !c      YEXINT = (YINT/SINT)**(1.0/PEX)
        !
    end subroutine ijsect
    !*==HALF.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! IJSECT


    subroutine half(X, Y, S, N)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(*) :: S, X, Y
        intent (in) S
        intent (inout) N, X, Y
        !
        ! Local variables
        !
        integer :: i, inext, k
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
        !-------------------------------------------------
        !     Halves the number of points in airfoil
        !-------------------------------------------------
        !
        k = 1
        inext = 3
        do i = 2, N - 1
            !------ if corner is found, preserve it.
            if (S(i)==S(i + 1)) then
                k = k + 1
                X(k) = X(i)
                Y(k) = Y(i)
                k = k + 1
                X(k) = X(i + 1)
                Y(k) = Y(i + 1)
                inext = i + 3
            endif
            !
            if (i==inext) then
                k = k + 1
                X(k) = X(i)
                Y(k) = Y(i)
                inext = i + 2
            endif
            !
        enddo
        k = k + 1
        X(k) = X(N)
        Y(k) = Y(N)
        !
        !---- set new number of points
        N = k
        !
    end subroutine half

end module m_xgeom