!***********************************************************************
!    Module:  xgeom.f
! 
!    Copyright (C) 2000 Mark Drela 
! 
!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation; either version 2 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program; if not, write to the Free Software
!    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!***********************************************************************

SUBROUTINE LEFIND(SLE, X, XP, Y, YP, S, N)
    DIMENSION X(*), XP(*), Y(*), YP(*), S(*)
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
    DSEPS = (S(N) - S(1)) * 1.0E-5
    !
    !---- set trailing edge point coordinates
    XTE = 0.5 * (X(1) + X(N))
    YTE = 0.5 * (Y(1) + Y(N))
    !
    !---- get first guess for SLE
    DO 10 I = 3, N - 2
        DXTE = X(I) - XTE
        DYTE = Y(I) - YTE
        DX = X(I + 1) - X(I)
        DY = Y(I + 1) - Y(I)
        DOTP = DXTE * DX + DYTE * DY
        IF(DOTP < 0.0) GO TO 11
    10 CONTINUE
    !
    11 SLE = S(I)
    !
    !---- check for sharp LE case
    IF(S(I) == S(I - 1)) THEN
        !cc        WRITE(*,*) 'Sharp LE found at ',I,SLE
        RETURN
    ENDIF
    !
    !---- Newton iteration to get exact SLE value
    DO 20 ITER = 1, 50
        XLE = SEVAL(SLE, X, XP, S, N)
        YLE = SEVAL(SLE, Y, YP, S, N)
        DXDS = DEVAL(SLE, X, XP, S, N)
        DYDS = DEVAL(SLE, Y, YP, S, N)
        DXDD = D2VAL(SLE, X, XP, S, N)
        DYDD = D2VAL(SLE, Y, YP, S, N)
        !
        XCHORD = XLE - XTE
        YCHORD = YLE - YTE
        !
        !------ drive dot product between chord line and LE tangent to zero
        RES = XCHORD * DXDS + YCHORD * DYDS
        RESS = DXDS * DXDS + DYDS * DYDS&
                + XCHORD * DXDD + YCHORD * DYDD
        !
        !------ Newton delta for SLE
        DSLE = -RES / RESS
        !
        DSLE = MAX(DSLE, -0.02 * ABS(XCHORD + YCHORD))
        DSLE = MIN(DSLE, 0.02 * ABS(XCHORD + YCHORD))
        SLE = SLE + DSLE
        IF(ABS(DSLE) < DSEPS) RETURN
    20 CONTINUE
    WRITE(*, *) 'LEFIND:  LE point not found.  Continuing...'
    SLE = S(I)
    RETURN
END


SUBROUTINE SOPPS(SOPP, SI, X, XP, Y, YP, S, N, SLE)
    DIMENSION X(*), XP(*), Y(*), YP(*), S(*)
    !--------------------------------------------------
    !     Calculates arc length SOPP of point
    !     which is opposite of point SI, on the
    !     other side of the airfoil baseline
    !--------------------------------------------------
    !
    !---- reference length for testing convergence
    SLEN = S(N) - S(1)
    !
    !---- set chordline vector
    XLE = SEVAL(SLE, X, XP, S, N)
    YLE = SEVAL(SLE, Y, YP, S, N)
    XTE = 0.5 * (X(1) + X(N))
    YTE = 0.5 * (Y(1) + Y(N))
    CHORD = SQRT((XTE - XLE)**2 + (YTE - YLE)**2)
    DXC = (XTE - XLE) / CHORD
    DYC = (YTE - YLE) / CHORD
    !
    IF(SI < SLE) THEN
        IN = 1
        INOPP = N
    ELSE
        IN = N
        INOPP = 1
    ENDIF
    SFRAC = (SI - SLE) / (S(IN) - SLE)
    SOPP = SLE + SFRAC * (S(INOPP) - SLE)
    !
    IF(ABS(SFRAC) <= 1.0E-5) THEN
        SOPP = SLE
        RETURN
    ENDIF
    !
    !---- XBAR = x coordinate in chord-line axes
    XI = SEVAL(SI, X, XP, S, N)
    YI = SEVAL(SI, Y, YP, S, N)
    XLE = SEVAL(SLE, X, XP, S, N)
    YLE = SEVAL(SLE, Y, YP, S, N)
    XBAR = (XI - XLE) * DXC + (YI - YLE) * DYC
    !
    !---- converge on exact opposite point with same XBAR value
    DO 300 ITER = 1, 12
        XOPP = SEVAL(SOPP, X, XP, S, N)
        YOPP = SEVAL(SOPP, Y, YP, S, N)
        XOPPD = DEVAL(SOPP, X, XP, S, N)
        YOPPD = DEVAL(SOPP, Y, YP, S, N)
        !
        RES = (XOPP - XLE) * DXC + (YOPP - YLE) * DYC - XBAR
        RESD = XOPPD * DXC + YOPPD * DYC
        !
        IF(ABS(RES) / SLEN < 1.0E-5) GO TO 305
        IF(RESD == 0.0) GO TO 303
        !
        DSOPP = -RES / RESD
        SOPP = SOPP + DSOPP
        !
        IF(ABS(DSOPP) / SLEN < 1.0E-5) GO TO 305
    300  CONTINUE
    303  WRITE(*, *)&
            'SOPPS: Opposite-point location failed. Continuing...'
    SOPP = SLE + SFRAC * (S(INOPP) - SLE)
    !
    305  CONTINUE
    RETURN
END
! SOPPS



SUBROUTINE NORM(X, XP, Y, YP, S, N)
    DIMENSION X(*), XP(*), Y(*), YP(*), S(*)
    !-----------------------------------------------
    !     Scales coordinates to get unit chord
    !-----------------------------------------------
    !
    CALL SCALC(X, Y, S, N)
    CALL SEGSPL(X, XP, S, N)
    CALL SEGSPL(Y, YP, S, N)
    !
    CALL LEFIND(SLE, X, XP, Y, YP, S, N)
    !
    XMAX = 0.5 * (X(1) + X(N))
    XMIN = SEVAL(SLE, X, XP, S, N)
    YMIN = SEVAL(SLE, Y, YP, S, N)
    !
    FUDGE = 1.0 / (XMAX - XMIN)
    DO 40 I = 1, N
        X(I) = (X(I) - XMIN) * FUDGE
        Y(I) = (Y(I) - YMIN) * FUDGE
        S(I) = S(I) * FUDGE
    40 CONTINUE
    !
    RETURN
END


SUBROUTINE GEOPAR(X, XP, Y, YP, S, N, T, &
        SLE, CHORD, AREA, RADLE, ANGTE, &
        EI11A, EI22A, APX1A, APX2A, &
        EI11T, EI22T, APX1T, APX2T, &
        THICK, CAMBR)
    DIMENSION X(*), XP(*), Y(*), YP(*), S(*), T(*)
    !
    PARAMETER (IBX = 600)
    DIMENSION&
            XCAM(2 * IBX), YCAM(2 * IBX), YCAMP(2 * IBX), &
            XTHK(2 * IBX), YTHK(2 * IBX), YTHKP(2 * IBX)
    !------------------------------------------------------
    !     Sets geometric parameters for airfoil shape
    !------------------------------------------------------
    CALL LEFIND(SLE, X, XP, Y, YP, S, N)
    !
    XLE = SEVAL(SLE, X, XP, S, N)
    YLE = SEVAL(SLE, Y, YP, S, N)
    XTE = 0.5 * (X(1) + X(N))
    YTE = 0.5 * (Y(1) + Y(N))
    !
    CHSQ = (XTE - XLE)**2 + (YTE - YLE)**2
    CHORD = SQRT(CHSQ)
    !
    CURVLE = CURV(SLE, X, XP, Y, YP, S, N)
    !
    RADLE = 0.0
    IF(ABS(CURVLE) > 0.001 * (S(N) - S(1))) RADLE = 1.0 / CURVLE
    !
    ANG1 = ATAN2(-YP(1), -XP(1))
    ANG2 = ATANC(YP(N), XP(N), ANG1)
    ANGTE = ANG2 - ANG1
    !

    DO I = 1, N
        T(I) = 1.0
    ENDDO
    !
    CALL AECALC(N, X, Y, T, 1, &
            AREA, XCENA, YCENA, EI11A, EI22A, APX1A, APX2A)
    !
    CALL AECALC(N, X, Y, T, 2, &
            SLEN, XCENT, YCENT, EI11T, EI22T, APX1T, APX2T)
    !
    !--- Old, approximate thickness,camber routine (on discrete points only)
    CALL TCCALC(X, XP, Y, YP, S, N, THICK, XTHICK, CAMBR, XCAMBR)
    !
    !--- More accurate thickness and camber estimates
    !c      CALL GETCAM(XCAM,YCAM,NCAM,XTHK,YTHK,NTHK,
    !c     &            X,XP,Y,YP,S,N )
    !c      CALL GETMAX(XCAM,YCAM,YCAMP,NCAM,XCAMBR,CAMBR)
    !c      CALL GETMAX(XTHK,YTHK,YTHKP,NTHK,XTHICK,THICK)
    !c      THICK = 2.0*THICK
    !
    WRITE(*, 1000) THICK, XTHICK, CAMBR, XCAMBR
    1000 FORMAT(' Max thickness = ', F12.6, '  at x = ', F7.3, &
            /' Max camber    = ', F12.6, '  at x = ', F7.3)


    !
    RETURN
END
! GEOPAR


SUBROUTINE AECALC(N, X, Y, T, ITYPE, &
        AREA, XCEN, YCEN, EI11, EI22, APX1, APX2)
    DIMENSION X(*), Y(*), T(*)
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
    DATA PI / 3.141592653589793238 /
    !
    SINT = 0.0
    AINT = 0.0
    XINT = 0.0
    YINT = 0.0
    XXINT = 0.0
    XYINT = 0.0
    YYINT = 0.0
    !
    DO 10 IO = 1, N
        IF(IO == N) THEN
            IP = 1
        ELSE
            IP = IO + 1
        ENDIF
        !
        DX = X(IO) - X(IP)
        DY = Y(IO) - Y(IP)
        XA = (X(IO) + X(IP)) * 0.50
        YA = (Y(IO) + Y(IP)) * 0.50
        TA = (T(IO) + T(IP)) * 0.50
        !
        DS = SQRT(DX * DX + DY * DY)
        SINT = SINT + DS

        IF(ITYPE == 1) THEN
            !-------- integrate over airfoil cross-section
            DA = YA * DX
            AINT = AINT + DA
            XINT = XINT + XA * DA
            YINT = YINT + YA * DA / 2.0
            XXINT = XXINT + XA * XA * DA
            XYINT = XYINT + XA * YA * DA / 2.0
            YYINT = YYINT + YA * YA * DA / 3.0
        ELSE
            !-------- integrate over skin thickness
            DA = TA * DS
            AINT = AINT + DA
            XINT = XINT + XA * DA
            YINT = YINT + YA * DA
            XXINT = XXINT + XA * XA * DA
            XYINT = XYINT + XA * YA * DA
            YYINT = YYINT + YA * YA * DA
        ENDIF
        !
    10   CONTINUE
    !
    AREA = AINT
    !
    IF(AINT == 0.0) THEN
        XCEN = 0.0
        YCEN = 0.0
        EI11 = 0.0
        EI22 = 0.0
        APX1 = 0.0
        APX2 = ATAN2(1.0, 0.0)
        RETURN
    ENDIF
    !
    !
    !---- calculate centroid location
    XCEN = XINT / AINT
    YCEN = YINT / AINT
    !
    !---- calculate inertias
    EIXX = YYINT - YCEN * YCEN * AINT
    EIXY = XYINT - XCEN * YCEN * AINT
    EIYY = XXINT - XCEN * XCEN * AINT
    !
    !---- set principal-axis inertias, EI11 is closest to "up-down" bending inertia
    EISQ = 0.25 * (EIXX - EIYY)**2 + EIXY**2
    SGN = SIGN(1.0, EIYY - EIXX)
    EI11 = 0.5 * (EIXX + EIYY) - SGN * SQRT(EISQ)
    EI22 = 0.5 * (EIXX + EIYY) + SGN * SQRT(EISQ)
    !
    IF(EI11 == 0.0 .OR. EI22 == 0.0) THEN
        !----- vanishing section stiffness
        APX1 = 0.0
        APX2 = ATAN2(1.0, 0.0)
        !
    ELSEIF(EISQ / (EI11 * EI22) < (0.001 * SINT)**4) THEN
        !----- rotationally-invariant section (circle, square, etc.)
        APX1 = 0.0
        APX2 = ATAN2(1.0, 0.0)
        !
    ELSE
        !----- normal airfoil section
        C1 = EIXY
        S1 = EIXX - EI11
        !
        C2 = EIXY
        S2 = EIXX - EI22
        !
        IF(ABS(S1) > ABS(S2)) THEN
            APX1 = ATAN2(S1, C1)
            APX2 = APX1 + 0.5 * PI
        ELSE
            APX2 = ATAN2(S2, C2)
            APX1 = APX2 - 0.5 * PI
        ENDIF

        IF(APX1 < -0.5 * PI) APX1 = APX1 + PI
        IF(APX1 > +0.5 * PI) APX1 = APX1 - PI
        IF(APX2 < -0.5 * PI) APX2 = APX2 + PI
        IF(APX2 > +0.5 * PI) APX2 = APX2 - PI
        !
    ENDIF
    !
    RETURN
END
! AECALC



SUBROUTINE TCCALC(X, XP, Y, YP, S, N, &
        THICK, XTHICK, CAMBR, XCAMBR)
    DIMENSION X(*), XP(*), Y(*), YP(*), S(*)
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
    CALL LEFIND(SLE, X, XP, Y, YP, S, N)
    XLE = SEVAL(SLE, X, XP, S, N)
    YLE = SEVAL(SLE, Y, YP, S, N)
    XTE = 0.5 * (X(1) + X(N))
    YTE = 0.5 * (Y(1) + Y(N))
    CHORD = SQRT((XTE - XLE)**2 + (YTE - YLE)**2)
    !
    !---- set unit chord-line vector
    DXC = (XTE - XLE) / CHORD
    DYC = (YTE - YLE) / CHORD
    !
    THICK = 0.
    XTHICK = 0.
    CAMBR = 0.
    XCAMBR = 0.
    !
    !---- go over each point, finding the y-thickness and camber
    DO 30 I = 1, N
        XBAR = (X(I) - XLE) * DXC + (Y(I) - YLE) * DYC
        YBAR = (Y(I) - YLE) * DXC - (X(I) - XLE) * DYC
        !
        !------ set point on the opposite side with the same chord x value
        CALL SOPPS(SOPP, S(I), X, XP, Y, YP, S, N, SLE)
        XOPP = SEVAL(SOPP, X, XP, S, N)
        YOPP = SEVAL(SOPP, Y, YP, S, N)
        !
        YBAROP = (YOPP - YLE) * DXC - (XOPP - XLE) * DYC
        !
        YC = 0.5 * (YBAR + YBAROP)
        YT = ABS(YBAR - YBAROP)
        !
        IF(ABS(YC) > ABS(CAMBR)) THEN
            CAMBR = YC
            XCAMBR = XOPP
        ENDIF
        IF(ABS(YT) > ABS(THICK)) THEN
            THICK = YT
            XTHICK = XOPP
        ENDIF
    30 CONTINUE
    !
    RETURN
END
! TCCALC




SUBROUTINE CANG(X, Y, N, IPRINT, IMAX, AMAX)
    DIMENSION X(*), Y(*)
    !-------------------------------------------------------------------
    !     IPRINT=2:   Displays all panel node corner angles
    !     IPRINT=1:   Displays max panel node corner angle
    !     IPRINT=0:   No display... just returns values
    !-------------------------------------------------------------------
    !
    AMAX = 0.0
    IMAX = 1
    !
    !---- go over each point, calculating corner angle
    IF(IPRINT == 2) WRITE(*, 1050)
    DO 30 I = 2, N - 1
        DX1 = X(I) - X(I - 1)
        DY1 = Y(I) - Y(I - 1)
        DX2 = X(I) - X(I + 1)
        DY2 = Y(I) - Y(I + 1)
        !
        !------ allow for doubled points
        IF(DX1 == 0.0 .AND. DY1 == 0.0) THEN
            DX1 = X(I) - X(I - 2)
            DY1 = Y(I) - Y(I - 2)
        ENDIF
        IF(DX2 == 0.0 .AND. DY2 == 0.0) THEN
            DX2 = X(I) - X(I + 2)
            DY2 = Y(I) - Y(I + 2)
        ENDIF
        !
        CROSSP = (DX2 * DY1 - DY2 * DX1)&
                / SQRT((DX1**2 + DY1**2) * (DX2**2 + DY2**2))
        ANGL = ASIN(CROSSP) * (180.0 / 3.1415926)
        IF(IPRINT == 2) WRITE(*, 1100) I, X(I), Y(I), ANGL
        IF(ABS(ANGL) > ABS(AMAX)) THEN
            AMAX = ANGL
            IMAX = I
        ENDIF
    30 CONTINUE
    !
    IF(IPRINT >= 1) WRITE(*, 1200) AMAX, IMAX, X(IMAX), Y(IMAX)
    !
    RETURN
    !
    1050 FORMAT(/'  i       x        y      angle')
    !CC             120   0.2134  -0.0234   25.322
    1100 FORMAT(1X, I3, 2F9.4, F9.3)
    1200 FORMAT(/' Maximum panel corner angle =', F7.3, &
            '   at  i,x,y  = ', I3, 2F9.4)
END
! CANG



SUBROUTINE INTER(X0, XP0, Y0, YP0, S0, N0, SLE0, &
        X1, XP1, Y1, YP1, S1, N1, SLE1, &
        X, Y, N, FRAC)
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
    REAL X0(N0), Y0(N0), XP0(N0), YP0(N0), S0(N0)
    REAL X1(N1), Y1(N1), XP1(N1), YP1(N1), S1(N1)
    REAL X(*), Y(*)
    !
    !---- number of points in interpolated airfoil is the same as in airfoil 0
    N = N0
    !
    !---- interpolation weighting fractions
    F0 = 1.0 - FRAC
    F1 = FRAC
    !
    !---- top side spline parameter increments
    TOPS0 = S0(1) - SLE0
    TOPS1 = S1(1) - SLE1
    !
    !---- bottom side spline parameter increments
    BOTS0 = S0(N0) - SLE0
    BOTS1 = S1(N1) - SLE1
    !
    DO 50 I = 1, N
        !
        !------ normalized spline parameter is taken from airfoil 0 value
        IF(S0(I) < SLE0) SN = (S0(I) - SLE0) / TOPS0    ! top side
        IF(S0(I) >= SLE0) SN = (S0(I) - SLE0) / BOTS0    ! bottom side
        !
        !------ set actual spline parameters
        ST0 = S0(I)
        IF(ST0 < SLE0) ST1 = SLE1 + TOPS1 * SN
        IF(ST0 >= SLE0) ST1 = SLE1 + BOTS1 * SN
        !
        !------ set input coordinates at common spline parameter location
        XT0 = SEVAL(ST0, X0, XP0, S0, N0)
        YT0 = SEVAL(ST0, Y0, YP0, S0, N0)
        XT1 = SEVAL(ST1, X1, XP1, S1, N1)
        YT1 = SEVAL(ST1, Y1, YP1, S1, N1)
        !
        !------ set interpolated x,y coordinates
        X(I) = F0 * XT0 + F1 * XT1
        Y(I) = F0 * YT0 + F1 * YT1
        !
    50 CONTINUE
    !
    RETURN
END
! INTER



SUBROUTINE INTERX(X0, XP0, Y0, YP0, S0, N0, SLE0, &
        X1, XP1, Y1, YP1, S1, N1, SLE1, &
        X, Y, N, FRAC)
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
    REAL X0(N0), Y0(N0), XP0(N0), YP0(N0), S0(N0)
    REAL X1(N1), Y1(N1), XP1(N1), YP1(N1), S1(N1)
    REAL X(N), Y(N)
    !
    !---- number of points in interpolated airfoil is the same as in airfoil 0
    N = N0
    !
    !---- interpolation weighting fractions
    F0 = 1.0 - FRAC
    F1 = FRAC
    !
    XLE0 = SEVAL(SLE0, X0, XP0, S0, N0)
    XLE1 = SEVAL(SLE1, X1, XP1, S1, N1)
    !
    DO 50 I = 1, N
        !
        !------ normalized x parameter is taken from airfoil 0 value
        IF(S0(I) < SLE0) XN = (X0(I) - XLE0) / (X0(1) - XLE0)
        IF(S0(I) >= SLE0) XN = (X0(I) - XLE0) / (X0(N0) - XLE0)
        !
        !------ set target x and initial spline parameters
        XT0 = X0(I)
        ST0 = S0(I)
        IF(ST0 < SLE0) THEN
            XT1 = XLE1 + (X1(1) - XLE1) * XN
            ST1 = SLE1 + (S1(1) - SLE1) * XN
        ELSE
            XT1 = XLE1 + (X1(N1) - XLE1) * XN
            ST1 = SLE1 + (S1(N1) - SLE1) * XN
        ENDIF
        !
        CALL SINVRT(ST0, XT0, X0, XP0, S0, N0)
        CALL SINVRT(ST1, XT1, X1, XP1, S1, N1)
        !
        !------ set input coordinates at common spline parameter location
        XT0 = SEVAL(ST0, X0, XP0, S0, N0)
        YT0 = SEVAL(ST0, Y0, YP0, S0, N0)
        XT1 = SEVAL(ST1, X1, XP1, S1, N1)
        YT1 = SEVAL(ST1, Y1, YP1, S1, N1)
        !
        !------ set interpolated x,y coordinates
        X(I) = F0 * XT0 + F1 * XT1
        Y(I) = F0 * YT0 + F1 * YT1
        !
    50 CONTINUE
    !
    RETURN
END
! INTERX





SUBROUTINE BENDUMP(N, X, Y)
    REAL X(*), Y(*)
    !
    PEX = 16.0
    CALL IJSECT(N, X, Y, PEX, &
            AREA, SLEN, &
            XMIN, XMAX, XEXINT, &
            YMIN, YMAX, YEXINT, &
            XC, YC, &
            XCT, YCT, &
            AIXX, AIYY, &
            AIXXT, AIYYT, &
            AJ, AJT)
    !      CALL IJSECT(N,X,Y, PEX,
    !     &    AREA, SLEN,
    !     &    XC, XMIN, XMAX, XEXINT,
    !     &    YC, YMIN, YMAX, YEXINT,
    !     &    AIXX, AIXXT,
    !     &    AIYY, AIYYT,
    !     &    AJ  , AJT   )
    !
    WRITE(*, *)
    WRITE(*, 1200) 'Area =', AREA
    WRITE(*, 1200) 'Slen =', SLEN
    WRITE(*, *)
    WRITE(*, 1200) 'X-bending parameters(solid):'
    WRITE(*, 1200) '        Xc =', XC
    WRITE(*, 1200) '  max X-Xc =', XMAX - XC
    WRITE(*, 1200) '  min X-Xc =', XMIN - XC
    WRITE(*, 1200) '       Iyy =', AIYY
    XBAR = MAX(ABS(XMAX - XC), ABS(XMIN - XC))
    WRITE(*, 1200) ' Iyy/(X-Xc)=', AIYY / XBAR
    WRITE(*, *)
    WRITE(*, 1200) 'Y-bending parameters(solid):'
    WRITE(*, 1200) '        Yc =', YC
    WRITE(*, 1200) '  max Y-Yc =', YMAX - YC
    WRITE(*, 1200) '  min Y-Yc =', YMIN - YC
    WRITE(*, 1200) '       Ixx =', AIXX
    YBAR = MAX(ABS(YMAX - YC), ABS(YMIN - YC))
    WRITE(*, 1200) ' Ixx/(Y-Yc)=', AIXX / YBAR
    WRITE(*, *)
    WRITE(*, 1200) '       J   =', AJ
    !
    WRITE(*, *)
    WRITE(*, *)
    WRITE(*, 1200) 'X-bending parameters(skin):'
    WRITE(*, 1200) '         Xc =', XCT
    WRITE(*, 1200) '   max X-Xc =', XMAX - XCT
    WRITE(*, 1200) '   min X-Xc =', XMIN - XCT
    WRITE(*, 1200) '      Iyy/t =', AIYYT
    XBART = MAX(ABS(XMAX - XCT), ABS(XMIN - XCT))
    WRITE(*, 1200) ' Iyy/t(X-Xc)=', AIYYT / XBART
    WRITE(*, *)
    WRITE(*, 1200) 'Y-bending parameters(skin):'
    WRITE(*, 1200) '         Yc =', YCT
    WRITE(*, 1200) '   max Y-Yc =', YMAX - YCT
    WRITE(*, 1200) '   min Y-Yc =', YMIN - YCT
    WRITE(*, 1200) '      Ixx/t =', AIXXT
    YBART = MAX(ABS(YMAX - YCT), ABS(YMIN - YCT))
    WRITE(*, 1200) ' Ixx/t(Y-Yc)=', AIXXT / YBART
    WRITE(*, *)
    WRITE(*, 1200) '      J/t   =', AJT
    !
    !      WRITE(*,*)
    !      WRITE(*,1200) '  power-avg X-Xc =', XEXINT
    !      WRITE(*,1200) '  power-avg Y-Yc =', YEXINT
    !
    RETURN
    !
    1200 FORMAT(1X, A, G14.6)
END
! BENDUMP



SUBROUTINE BENDUMP2(N, X, Y, T)
    REAL X(*), Y(*), T(*)
    !
    DTR = ATAN(1.0) / 45.0
    !
    PEX = 16.0
    CALL IJSECT(N, X, Y, PEX, &
            AREA, SLEN, &
            XMIN, XMAX, XEXINT, &
            YMIN, YMAX, YEXINT, &
            XC, YC, &
            XCT, YCT, &
            AIXX, AIYY, &
            AIXXT, AIYYT, &
            AJ, AJT)
    !      CALL IJSECT(N,X,Y, PEX,
    !     &    AREA, SLEN,
    !     &    XC, XMIN, XMAX, XEXINT,
    !     &    YC, YMIN, YMAX, YEXINT,
    !     &    AIXX, AIXXT,
    !     &    AIYY, AIYYT,
    !     &    AJ  , AJT   )
    !
    !
    CALL AECALC(N, X, Y, T, 1, &
            AREA, XCENA, YCENA, EI11A, EI22A, APX1A, APX2A)
    !
    CALL AECALC(N, X, Y, T, 2, &
            SLEN, XCENT, YCENT, EI11T, EI22T, APX1T, APX2T)
    !

    WRITE(*, *)
    WRITE(*, 1200) 'Area =', AREA
    WRITE(*, 1200) 'Slen =', SLEN
    WRITE(*, *)
    WRITE(*, 1200) 'X-bending parameters:'
    WRITE(*, 1200) 'solid centroid Xc=', XCENA
    WRITE(*, 1200) 'skin  centroid Xc=', XCENT
    WRITE(*, 1200) ' solid max X-Xc  =', XMAX - XCENA
    WRITE(*, 1200) ' solid min X-Xc  =', XMIN - XCENA
    WRITE(*, 1200) ' skin  max X-Xc  =', XMAX - XCENT
    WRITE(*, 1200) ' skin  min X-Xc  =', XMIN - XCENT
    WRITE(*, 1200) '     solid Iyy   =', EI22A
    WRITE(*, 1200) '     skin  Iyy/t =', EI22T
    XBARA = MAX(ABS(XMAX - XCENA), ABS(XMIN - XCENA))
    XBART = MAX(ABS(XMAX - XCENT), ABS(XMIN - XCENT))
    WRITE(*, 1200) ' solid Iyy/(X-Xc)=', EI22A / XBARA
    WRITE(*, 1200) ' skin Iyy/t(X-Xc)=', EI22T / XBART
    !
    WRITE(*, *)
    WRITE(*, 1200) 'Y-bending parameters:'
    WRITE(*, 1200) 'solid centroid Yc=', YCENA
    WRITE(*, 1200) 'skin  centroid Yc=', YCENT
    WRITE(*, 1200) ' solid max Y-Yc  =', YMAX - YCENA
    WRITE(*, 1200) ' solid min Y-Yc  =', YMIN - YCENA
    WRITE(*, 1200) ' skin  max Y-Yc  =', YMAX - YCENT
    WRITE(*, 1200) ' skin  min Y-Yc  =', YMIN - YCENT
    WRITE(*, 1200) '     solid Ixx   =', EI11A
    WRITE(*, 1200) '     skin  Ixx/t =', EI11T
    YBARA = MAX(ABS(YMAX - YCENA), ABS(YMIN - YCENA))
    YBART = MAX(ABS(YMAX - YCENT), ABS(YMIN - YCENT))
    WRITE(*, 1200) ' solid Ixx/(Y-Yc)=', EI11A / YBARA
    WRITE(*, 1200) ' skin Ixx/t(Y-Yc)=', EI11T / YBART
    !
    WRITE(*, *)
    WRITE(*, 1200) ' solid principal axis angle (deg ccw) =', APX1A / DTR
    WRITE(*, 1200) ' skin  principal axis angle (deg ccw) =', APX1T / DTR

    !      WRITE(*,*)
    !      WRITE(*,1200) '  power-avg X-Xc =', XEXINT
    !      WRITE(*,1200) '  power-avg Y-Yc =', YEXINT
    !
    WRITE(*, *)
    WRITE(*, 1200) '    solid J     =', AJ
    WRITE(*, 1200) '    skin  J/t   =', AJT
    RETURN
    !
    1200 FORMAT(1X, A, G14.6)
END
! BENDUMP2



SUBROUTINE IJSECT(N, X, Y, PEX, &
        AREA, SLEN, &
        XMIN, XMAX, XEXINT, &
        YMIN, YMAX, YEXINT, &
        XC, YC, &
        XCT, YCT, &
        AIXX, AIYY, &
        AIXXT, AIYYT, &
        AJ, AJT)
    DIMENSION X(*), Y(*)
    !
    XMIN = X(1)
    XMAX = X(1)
    YMIN = Y(1)
    YMAX = Y(1)
    !
    DX = X(1) - X(N)
    DY = Y(1) - Y(N)
    DS = SQRT(DX * DX + DY * DY)
    XAVG = 0.5 * (X(1) + X(N))
    YAVG = 0.5 * (Y(1) + Y(N))
    !
    X_DY = DY * XAVG
    XX_DY = DY * XAVG**2
    XXX_DY = DY * XAVG**3
    X_DS = DS * XAVG
    XX_DS = DS * XAVG**2
    !
    Y_DX = DX * YAVG
    YY_DX = DX * YAVG**2
    YYY_DX = DX * YAVG**3
    Y_DS = DS * YAVG
    YY_DS = DS * YAVG**2
    !
    C_DS = DS
    !
    DO 10 I = 2, N
        DX = X(I) - X(I - 1)
        DY = Y(I) - Y(I - 1)
        DS = SQRT(DX * DX + DY * DY)
        XAVG = 0.5 * (X(I) + X(I - 1))
        YAVG = 0.5 * (Y(I) + Y(I - 1))
        !
        X_DY = X_DY + DY * XAVG
        XX_DY = XX_DY + DY * XAVG**2
        XXX_DY = XXX_DY + DY * XAVG**3
        X_DS = X_DS + DS * XAVG
        XX_DS = XX_DS + DS * XAVG**2
        !
        Y_DX = Y_DX + DX * YAVG
        YY_DX = YY_DX + DX * YAVG**2
        YYY_DX = YYY_DX + DX * YAVG**3
        Y_DS = Y_DS + DS * YAVG
        YY_DS = YY_DS + DS * YAVG**2
        !
        C_DS = C_DS + DS
        !
        XMIN = MIN(XMIN, X(I))
        XMAX = MAX(XMAX, X(I))
        YMIN = MIN(YMIN, Y(I))
        YMAX = MAX(YMAX, Y(I))
    10   CONTINUE
    !
    AREA = -Y_DX
    SLEN = C_DS
    !
    IF(AREA == 0.0) RETURN
    !
    XC = XX_DY / (2.0 * X_DY)
    XCT = X_DS / C_DS
    AIYY = XXX_DY / 3.0 - XX_DY * XC + X_DY * XC**2
    AIYYT = XX_DS - X_DS * XCT * 2.0 + C_DS * XCT**2
    !
    YC = YY_DX / (2.0 * Y_DX)
    YCT = Y_DS / C_DS
    AIXX = -YYY_DX / 3.0 + YY_DX * YC - Y_DX * YC**2
    AIXXT = YY_DS - Y_DS * YCT * 2.0 + C_DS * YCT**2
    !
    !
    SINT = 0.
    XINT = 0.
    YINT = 0.
    !
    DO 20 I = 2, N
        DX = X(I) - X(I - 1)
        DY = Y(I) - Y(I - 1)
        DS = SQRT(DX * DX + DY * DY)
        XAVG = 0.5 * (X(I) + X(I - 1)) - XC
        YAVG = 0.5 * (Y(I) + Y(I - 1)) - YC
        !
        SINT = SINT + DS
        !c        XINT = XINT + DS * ABS(XAVG)**PEX
        !c        YINT = YINT + DS * ABS(YAVG)**PEX
    20   CONTINUE
    !
    DO I = 1, N - 1
        IF(X(I + 1) >= X(I)) GO TO 30
    ENDDO
    IMID = N / 2
    30   IMID = I
    !
    AJ = 0.0
    DO I = 2, IMID
        XAVG = 0.5 * (X(I) + X(I - 1))
        YAVG = 0.5 * (Y(I) + Y(I - 1))
        DX = X(I - 1) - X(I)
        !
        IF(XAVG > X(N)) THEN
            YOPP = Y(N)
            GO TO 41
        ENDIF
        IF(XAVG <= X(IMID)) THEN
            YOPP = Y(IMID)
            GO TO 41
        ENDIF
        !
        DO J = N, IMID, -1
            IF(XAVG > X(J - 1) .AND. XAVG <= X(J)) THEN
                FRAC = (XAVG - X(J - 1))&
                        / (X(J) - X(J - 1))
                YOPP = Y(J - 1) + (Y(J) - Y(J - 1)) * FRAC
                GO TO 41
            ENDIF
        ENDDO
        41     CONTINUE
        !
        AJ = AJ + ABS(YAVG - YOPP)**3 * DX / 3.0
    ENDDO
    !
    AJT = 4.0 * AREA**2 / SLEN
    !
    !c      XEXINT = (XINT/SINT)**(1.0/PEX)
    !c      YEXINT = (YINT/SINT)**(1.0/PEX)
    !
    RETURN
END
! IJSECT


SUBROUTINE HALF(X, Y, S, N)
    !-------------------------------------------------
    !     Halves the number of points in airfoil
    !-------------------------------------------------
    REAL X(*), Y(*), S(*)
    !
    K = 1
    INEXT = 3
    DO 20 I = 2, N - 1
        !------ if corner is found, preserve it.
        IF(S(I) == S(I + 1)) THEN
            K = K + 1
            X(K) = X(I)
            Y(K) = Y(I)
            K = K + 1
            X(K) = X(I + 1)
            Y(K) = Y(I + 1)
            INEXT = I + 3
        ENDIF
        !
        IF(I == INEXT) THEN
            K = K + 1
            X(K) = X(I)
            Y(K) = Y(I)
            INEXT = I + 2
        ENDIF
        !
    20 CONTINUE
    K = K + 1
    X(K) = X(N)
    Y(K) = Y(N)
    !
    !---- set new number of points
    N = K
    !
    RETURN
END
! HALF

