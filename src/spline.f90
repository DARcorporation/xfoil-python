!***********************************************************************
!    Module:  spline.f
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

SUBROUTINE SPLINE(X, XS, S, N)
    DIMENSION X(N), XS(N), S(N)
    PARAMETER (NMAX = 1000)
    DIMENSION A(NMAX), B(NMAX), C(NMAX)
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
    IF(N > NMAX) STOP 'SPLINE: array overflow, increase NMAX'
    !
    DO I = 2, N - 1
        DSM = S(I) - S(I - 1)
        DSP = S(I + 1) - S(I)
        B(I) = DSP
        A(I) = 2.0 * (DSM + DSP)
        C(I) = DSM
        XS(I) = 3.0 * ((X(I + 1) - X(I)) * DSM / DSP + (X(I) - X(I - 1)) * DSP / DSM)
    end do
    !
    !---- set zero second derivative end conditions
    A(1) = 2.0
    C(1) = 1.0
    XS(1) = 3.0 * (X(2) - X(1)) / (S(2) - S(1))
    B(N) = 1.0
    A(N) = 2.0
    XS(N) = 3.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
    !
    !---- solve for derivative array XS
    CALL TRISOL(A, B, C, XS, N)
    !
    RETURN
END
! SPLINE


SUBROUTINE SPLIND(X, XS, S, N, XS1, XS2)
    DIMENSION X(N), XS(N), S(N)
    PARAMETER (NMAX = 1000)
    DIMENSION  A(NMAX), B(NMAX), C(NMAX)
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
    IF(N > NMAX) STOP 'SPLIND: array overflow, increase NMAX'
    !
    DO I = 2, N - 1
        DSM = S(I) - S(I - 1)
        DSP = S(I + 1) - S(I)
        B(I) = DSP
        A(I) = 2.0 * (DSM + DSP)
        C(I) = DSM
        XS(I) = 3.0 * ((X(I + 1) - X(I)) * DSM / DSP + (X(I) - X(I - 1)) * DSP / DSM)
    end do
    !
    IF(XS1 == 999.0) THEN
        !----- set zero second derivative end condition
        A(1) = 2.0
        C(1) = 1.0
        XS(1) = 3.0 * (X(2) - X(1)) / (S(2) - S(1))
    ELSE IF(XS1 == -999.0) THEN
        !----- set zero third derivative end condition
        A(1) = 1.0
        C(1) = 1.0
        XS(1) = 2.0 * (X(2) - X(1)) / (S(2) - S(1))
    ELSE
        !----- set specified first derivative end condition
        A(1) = 1.0
        C(1) = 0.
        XS(1) = XS1
    ENDIF
    !
    IF(XS2 == 999.0) THEN
        B(N) = 1.0
        A(N) = 2.0
        XS(N) = 3.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
    ELSE IF(XS2 == -999.0) THEN
        B(N) = 1.0
        A(N) = 1.0
        XS(N) = 2.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
    ELSE
        A(N) = 1.0
        B(N) = 0.
        XS(N) = XS2
    ENDIF
    !
    IF(N == 2 .AND. XS1 == -999.0 .AND. XS2 == -999.0) THEN
        B(N) = 1.0
        A(N) = 2.0
        XS(N) = 3.0 * (X(N) - X(N - 1)) / (S(N) - S(N - 1))
    ENDIF
    !
    !---- solve for derivative array XS
    CALL TRISOL(A, B, C, XS, N)
    !
    RETURN
END
! SPLIND



SUBROUTINE SPLINA(X, XS, S, N)
    IMPLICIT REAL (A-H, O-Z)
    DIMENSION X(N), XS(N), S(N)
    LOGICAL LEND
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
    LEND = .TRUE.
    DO I = 1, N - 1
        DS = S(I + 1) - S(I)
        IF (DS == 0.) THEN
            XS(I) = XS1
            LEND = .TRUE.
        ELSE
            DX = X(I + 1) - X(I)
            XS2 = DX / DS
            IF (LEND) THEN
                XS(I) = XS2
                LEND = .FALSE.
            ELSE
                XS(I) = 0.5 * (XS1 + XS2)
            ENDIF
        ENDIF
        XS1 = XS2
    end do
    XS(N) = XS1
    !
    RETURN
END
! SPLINA



SUBROUTINE TRISOL(A, B, C, D, KK)
    DIMENSION A(KK), B(KK), C(KK), D(KK)
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
    DO K = 2, KK
        KM = K - 1
        C(KM) = C(KM) / A(KM)
        D(KM) = D(KM) / A(KM)
        A(K) = A(K) - B(K) * C(KM)
        D(K) = D(K) - B(K) * D(KM)
    end do
    !
    D(KK) = D(KK) / A(KK)
    !
    DO K = KK - 1, 1, -1
        D(K) = D(K) - C(K) * D(K + 1)
    end do
    !
    RETURN
END
! TRISOL


FUNCTION SEVAL(SS, X, XS, S, N)
    DIMENSION X(N), XS(N), S(N)
    !--------------------------------------------------
    !     Calculates X(SS)                             |
    !     XS array must have been calculated by SPLINE |
    !--------------------------------------------------
    ILOW = 1
    I = N
    !
    10 IF(I - ILOW <= 1) GO TO 11
    !
    IMID = (I + ILOW) / 2
    IF(SS < S(IMID)) THEN
        I = IMID
    ELSE
        ILOW = IMID
    ENDIF
    GO TO 10
    !
    11 DS = S(I) - S(I - 1)
    T = (SS - S(I - 1)) / DS
    CX1 = DS * XS(I - 1) - X(I) + X(I - 1)
    CX2 = DS * XS(I) - X(I) + X(I - 1)
    SEVAL = T * X(I) + (1.0 - T) * X(I - 1) + (T - T * T) * ((1.0 - T) * CX1 - T * CX2)
    RETURN
END
! SEVAL

FUNCTION DEVAL(SS, X, XS, S, N)
    DIMENSION X(N), XS(N), S(N)
    !--------------------------------------------------
    !     Calculates dX/dS(SS)                         |
    !     XS array must have been calculated by SPLINE |
    !--------------------------------------------------
    ILOW = 1
    I = N
    !
    10 IF(I - ILOW <= 1) GO TO 11
    !
    IMID = (I + ILOW) / 2
    IF(SS < S(IMID)) THEN
        I = IMID
    ELSE
        ILOW = IMID
    ENDIF
    GO TO 10
    !
    11 DS = S(I) - S(I - 1)
    T = (SS - S(I - 1)) / DS
    CX1 = DS * XS(I - 1) - X(I) + X(I - 1)
    CX2 = DS * XS(I) - X(I) + X(I - 1)
    DEVAL = X(I) - X(I - 1) + (1. - 4.0 * T + 3.0 * T * T) * CX1 + T * (3.0 * T - 2.) * CX2
    DEVAL = DEVAL / DS
    RETURN
END
! DEVAL

FUNCTION D2VAL(SS, X, XS, S, N)
    DIMENSION X(N), XS(N), S(N)
    !--------------------------------------------------
    !     Calculates d2X/dS2(SS)                       |
    !     XS array must have been calculated by SPLINE |
    !--------------------------------------------------
    ILOW = 1
    I = N
    !
    10 IF(I - ILOW <= 1) GO TO 11
    !
    IMID = (I + ILOW) / 2
    IF(SS < S(IMID)) THEN
        I = IMID
    ELSE
        ILOW = IMID
    ENDIF
    GO TO 10
    !
    11 DS = S(I) - S(I - 1)
    T = (SS - S(I - 1)) / DS
    CX1 = DS * XS(I - 1) - X(I) + X(I - 1)
    CX2 = DS * XS(I) - X(I) + X(I - 1)
    D2VAL = (6. * T - 4.) * CX1 + (6. * T - 2.0) * CX2
    D2VAL = D2VAL / DS**2
    RETURN
END
! D2VAL


FUNCTION CURV(SS, X, XS, Y, YS, S, N)
    DIMENSION X(N), XS(N), Y(N), YS(N), S(N)
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
    ILOW = 1
    I = N
    !
    10 IF(I - ILOW <= 1) GO TO 11
    !
    IMID = (I + ILOW) / 2
    IF(SS < S(IMID)) THEN
        I = IMID
    ELSE
        ILOW = IMID
    ENDIF
    GO TO 10
    !
    11 DS = S(I) - S(I - 1)
    T = (SS - S(I - 1)) / DS
    !
    CX1 = DS * XS(I - 1) - X(I) + X(I - 1)
    CX2 = DS * XS(I) - X(I) + X(I - 1)
    XD = X(I) - X(I - 1) + (1.0 - 4.0 * T + 3.0 * T * T) * CX1 + T * (3.0 * T - 2.0) * CX2
    XDD = (6.0 * T - 4.0) * CX1 + (6.0 * T - 2.0) * CX2
    !
    CY1 = DS * YS(I - 1) - Y(I) + Y(I - 1)
    CY2 = DS * YS(I) - Y(I) + Y(I - 1)
    YD = Y(I) - Y(I - 1) + (1.0 - 4.0 * T + 3.0 * T * T) * CY1 + T * (3.0 * T - 2.0) * CY2
    YDD = (6.0 * T - 4.0) * CY1 + (6.0 * T - 2.0) * CY2
    !
    SD = SQRT(XD * XD + YD * YD)
    SD = MAX(SD, 0.001 * DS)
    !
    CURV = (XD * YDD - YD * XDD) / SD**3
    !
    RETURN
END
! CURV


FUNCTION CURVS(SS, X, XS, Y, YS, S, N)
    DIMENSION X(N), XS(N), Y(N), YS(N), S(N)
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
    ILOW = 1
    I = N
    !
    10 IF(I - ILOW <= 1) GO TO 11
    !
    IMID = (I + ILOW) / 2
    IF(SS < S(IMID)) THEN
        I = IMID
    ELSE
        ILOW = IMID
    ENDIF
    GO TO 10
    !
    11 DS = S(I) - S(I - 1)
    T = (SS - S(I - 1)) / DS
    !
    CX1 = DS * XS(I - 1) - X(I) + X(I - 1)
    CX2 = DS * XS(I) - X(I) + X(I - 1)
    XD = X(I) - X(I - 1) + (1.0 - 4.0 * T + 3.0 * T * T) * CX1 + T * (3.0 * T - 2.0) * CX2
    XDD = (6.0 * T - 4.0) * CX1 + (6.0 * T - 2.0) * CX2
    XDDD = 6.0 * CX1 + 6.0 * CX2
    !
    CY1 = DS * YS(I - 1) - Y(I) + Y(I - 1)
    CY2 = DS * YS(I) - Y(I) + Y(I - 1)
    YD = Y(I) - Y(I - 1) + (1.0 - 4.0 * T + 3.0 * T * T) * CY1 + T * (3.0 * T - 2.0) * CY2
    YDD = (6.0 * T - 4.0) * CY1 + (6.0 * T - 2.0) * CY2
    YDDD = 6.0 * CY1 + 6.0 * CY2
    !
    SD = SQRT(XD * XD + YD * YD)
    SD = MAX(SD, 0.001 * DS)
    !
    BOT = SD**3
    DBOTDT = 3.0 * SD * (XD * XDD + YD * YDD)
    !
    TOP = XD * YDD - YD * XDD
    DTOPDT = XD * YDDD - YD * XDDD
    !
    CURVS = (DTOPDT * BOT - DBOTDT * TOP) / BOT**2
    !
    RETURN
END
! CURVS


SUBROUTINE SINVRT(SI, XI, X, XS, S, N)
    DIMENSION X(N), XS(N), S(N)
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
    SISAV = SI
    !
    DO ITER = 1, 10
        RES = SEVAL(SI, X, XS, S, N) - XI
        RESP = DEVAL(SI, X, XS, S, N)
        DS = -RES / RESP
        SI = SI + DS
        IF(ABS(DS / (S(N) - S(1))) < 1.0E-5) RETURN
    end do
    WRITE(*, *)&
            'SINVRT: spline inversion failed. Input value returned.'
    SI = SISAV
    !
    RETURN
END
! SINVRT


SUBROUTINE SCALC(X, Y, S, N)
    DIMENSION X(N), Y(N), S(N)
    !----------------------------------------
    !     Calculates the arc length array S  |
    !     for a 2-D array of points (X,Y).   |
    !----------------------------------------
    !
    S(1) = 0.
    DO I = 2, N
        S(I) = S(I - 1) + SQRT((X(I) - X(I - 1))**2 + (Y(I) - Y(I - 1))**2)
    end do
    !
    RETURN
END
! SCALC


SUBROUTINE SPLNXY(X, XS, Y, YS, S, N)
    DIMENSION X(N), XS(N), Y(N), YS(N), S(N)
    !-----------------------------------------
    !     Splines 2-D shape X(S), Y(S), along |
    !     with true arc length parameter S.   |
    !-----------------------------------------
    PARAMETER (KMAX = 32)
    DIMENSION XT(0:KMAX), YT(0:KMAX)
    !
    KK = KMAX
    NPASS = 10
    !
    !---- set first estimate of arc length parameter
    CALL SCALC(X, Y, S, N)
    !
    !---- spline X(S) and Y(S)
    CALL SEGSPL(X, XS, S, N)
    CALL SEGSPL(Y, YS, S, N)
    !
    !---- re-integrate true arc length
    DO IPASS = 1, NPASS
        !
        SERR = 0.
        !
        DS = S(2) - S(1)
        DO I = 2, N
            DX = X(I) - X(I - 1)
            DY = Y(I) - Y(I - 1)
            !
            CX1 = DS * XS(I - 1) - DX
            CX2 = DS * XS(I) - DX
            CY1 = DS * YS(I - 1) - DY
            CY2 = DS * YS(I) - DY
            !
            XT(0) = 0.
            YT(0) = 0.
            DO K = 1, KK - 1
                T = FLOAT(K) / FLOAT(KK)
                XT(K) = T * DX + (T - T * T) * ((1.0 - T) * CX1 - T * CX2)
                YT(K) = T * DY + (T - T * T) * ((1.0 - T) * CY1 - T * CY2)
            ENDDO
            XT(KK) = DX
            YT(KK) = DY
            !
            SINT1 = 0.
            DO K = 1, KK
                SINT1 = SINT1&
                        + SQRT((XT(K) - XT(K - 1))**2 + (YT(K) - YT(K - 1))**2)
            ENDDO
            !
            SINT2 = 0.
            DO K = 2, KK, 2
                SINT2 = SINT2&
                        + SQRT((XT(K) - XT(K - 2))**2 + (YT(K) - YT(K - 2))**2)
            ENDDO
            !
            SINT = (4.0 * SINT1 - SINT2) / 3.0
            !
            IF(ABS(SINT - DS) > ABS(SERR))  SERR = SINT - DS
            !
            IF(I < N) DS = S(I + 1) - S(I)
            !
            S(I) = S(I - 1) + SQRT(SINT)
        ENDDO
        !
        SERR = SERR / (S(N) - S(1))
        WRITE(*, *) IPASS, SERR
        !
        !------ re-spline X(S) and Y(S)
        CALL SEGSPL(X, XS, S, N)
        CALL SEGSPL(Y, YS, S, N)
        !
        IF(ABS(SERR) < 1.0E-7) RETURN
        !
    end do
    !
    RETURN
END
! SPLNXY



SUBROUTINE SEGSPL(X, XS, S, N)
    !-----------------------------------------------
    !     Splines X(S) array just like SPLINE,      |
    !     but allows derivative discontinuities     |
    !     at segment joints.  Segment joints are    |
    !     defined by identical successive S values. |
    !-----------------------------------------------
    DIMENSION X(N), XS(N), S(N)
    !
    IF(S(1) == S(2)) STOP 'SEGSPL:  First input point duplicated'
    IF(S(N) == S(N - 1)) STOP 'SEGSPL:  Last  input point duplicated'
    !
    ISEG0 = 1
    DO ISEG = 2, N - 2
        IF(S(ISEG) == S(ISEG + 1)) THEN
            NSEG = ISEG - ISEG0 + 1
            CALL SPLIND(X(ISEG0), XS(ISEG0), S(ISEG0), NSEG, -999.0, -999.0)
            ISEG0 = ISEG + 1
        ENDIF
    end do
    !
    NSEG = N - ISEG0 + 1
    CALL SPLIND(X(ISEG0), XS(ISEG0), S(ISEG0), NSEG, -999.0, -999.0)
    !
    RETURN
END
! SEGSPL



SUBROUTINE SEGSPLD(X, XS, S, N, XS1, XS2)
    !-----------------------------------------------
    !     Splines X(S) array just like SPLIND,      |
    !     but allows derivative discontinuities     |
    !     at segment joints.  Segment joints are    |
    !     defined by identical successive S values. |
    !-----------------------------------------------
    DIMENSION X(N), XS(N), S(N)
    !
    IF(S(1) == S(2)) STOP 'SEGSPL:  First input point duplicated'
    IF(S(N) == S(N - 1)) STOP 'SEGSPL:  Last  input point duplicated'
    !
    ISEG0 = 1
    DO ISEG = 2, N - 2
        IF(S(ISEG) == S(ISEG + 1)) THEN
            NSEG = ISEG - ISEG0 + 1
            CALL SPLIND(X(ISEG0), XS(ISEG0), S(ISEG0), NSEG, XS1, XS2)
            ISEG0 = ISEG + 1
        ENDIF
    end do
    !
    NSEG = N - ISEG0 + 1
    CALL SPLIND(X(ISEG0), XS(ISEG0), S(ISEG0), NSEG, XS1, XS2)
    !
    RETURN
END
! SEGSPL
