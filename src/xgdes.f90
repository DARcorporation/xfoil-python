!***********************************************************************
!    Module:  xgdes.f
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
!

SUBROUTINE ABCOPY(LCONF)
    use m_spline
    use i_xfoil
    LOGICAL LCONF
    !
    IF(NB.LE.1) THEN
        WRITE(*, *) 'ABCOPY: Buffer airfoil not available.'
        RETURN
    ELSEIF(NB.GT.IQX - 5) THEN
        WRITE(*, *) 'Maximum number of panel nodes  : ', IQX - 5
        WRITE(*, *) 'Number of buffer airfoil points: ', NB
        WRITE(*, *) 'Current airfoil cannot be set.'
        WRITE(*, *) 'Try executing PANE at Top Level instead.'
        RETURN
    ENDIF
    IF(N.NE.NB) LBLINI = .FALSE.
    !
    N = NB
    DO 101 I = 1, N
        X(I) = XB(I)
        Y(I) = YB(I)
    101 CONTINUE
    LGSAME = .TRUE.
    !
    IF(LBFLAP) THEN
        XOF = XBF
        YOF = YBF
        LFLAP = .TRUE.
    ENDIF
    !
    !---- strip out doubled points
    I = 1
    102  CONTINUE
    I = I + 1
    IF(X(I - 1).EQ.X(I) .AND. Y(I - 1).EQ.Y(I)) THEN
        DO 104 J = I, N - 1
            X(J) = X(J + 1)
            Y(J) = Y(J + 1)
        104    CONTINUE
        N = N - 1
    ENDIF
    IF(I.LT.N) GO TO 102
    !
    CALL SCALC(X, Y, S, N)
    CALL SEGSPL(X, XP, S, N)
    CALL SEGSPL(Y, YP, S, N)

    CALL NCALC(X, Y, S, N, NX, NY)

    CALL LEFIND(SLE, X, XP, Y, YP, S, N)
    XLE = SEVAL(SLE, X, XP, S, N)
    YLE = SEVAL(SLE, Y, YP, S, N)
    XTE = 0.5 * (X(1) + X(N))
    YTE = 0.5 * (Y(1) + Y(N))
    CHORD = SQRT((XTE - XLE)**2 + (YTE - YLE)**2)

    CALL TECALC
    CALL APCALC
    !
    LGAMU = .FALSE.
    LQINU = .FALSE.
    LWAKE = .FALSE.
    LQAIJ = .FALSE.
    LADIJ = .FALSE.
    LWDIJ = .FALSE.
    LIPAN = .FALSE.
    LVCONV = .FALSE.
    LSCINI = .FALSE.
    !CC      LBLINI = .FALSE.
    !
    IF(LCONF) WRITE(*, 1200) N
    1200 FORMAT(/' Current airfoil nodes set from buffer airfoil nodes (', &
            I4, ' )')
    !
    RETURN
END
! ABCOPY


SUBROUTINE GETXYF(X, XP, Y, YP, S, N, TOPS, BOTS, XF, YF)
    use m_spline
    DIMENSION X(N), XP(N), Y(N), YP(N), S(N)
    !
    IF(XF .EQ. -999.0)&
            CALL ASKR('Enter flap hinge x location^', XF)
    !
    !---- find top and bottom y at hinge x location
    TOPS = S(1) + (X(1) - XF)
    BOTS = S(N) - (X(N) - XF)
    CALL SINVRT(TOPS, XF, X, XP, S, N)
    CALL SINVRT(BOTS, XF, X, XP, S, N)
    TOPY = SEVAL(TOPS, Y, YP, S, N)
    BOTY = SEVAL(BOTS, Y, YP, S, N)
    !
    WRITE(*, 1000) TOPY, BOTY
    1000 FORMAT(/'  Top    surface:  y =', F8.4, '     y/t = 1.0'&
            /'  Bottom surface:  y =', F8.4, '     y/t = 0.0')
    !
    IF(YF .EQ. -999.0)&
            CALL ASKR(&
                    'Enter flap hinge y location (or 999 to specify y/t)^', YF)
    !
    IF(YF .EQ. 999.0) THEN
        CALL ASKR('Enter flap hinge relative y/t location^', YREL)
        YF = TOPY * YREL + BOTY * (1.0 - YREL)
    ENDIF
    !
    RETURN
END
! GETXYF