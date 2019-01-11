!***********************************************************************
!    Module:  xmdes.f
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
SUBROUTINE MDES
    !------------------------------------
    !     Full-Inverse design routine.
    !     Based on circle plane mapping.
    !------------------------------------
        use m_spline
    use i_xfoil
    LOGICAL LCNPL, LRECALC
    !
    CHARACTER*4 COMAND, COMOLD
    CHARACTER*80 LINE
    !
    CHARACTER*128 COMARG, ARGOLD
    CHARACTER*1 CHKEY
    !
    REAL XBOX(2), YBOX(2)
    REAL XSP(IBX), YSP(IBX, IPX), YSPD(IBX, IPX)
    !
    DIMENSION IINPUT(20)
    DIMENSION RINPUT(20)
    LOGICAL ERROR, LPLNEW
    !
    SAVE COMOLD, ARGOLD
    !
    !---- statement function for compressible Karman-Tsien velocity
    QCOMP(G) = G * (1.0 - TKLAM) / (1.0 - TKLAM * (G / QINF)**2)
    !
    COMAND = '****'
    COMARG = ' '
    LRECALC = .FALSE.
    !
    IF(N.EQ.0) THEN
        WRITE(*, *)
        WRITE(*, *) '***  No airfoil available  ***'
        RETURN
    ENDIF
    !
    LCNPL = .FALSE.
    LSYM = .TRUE.
    !
    NTQSPL = 1
    IF(LQSLOP) NTQSPL = 4
    !
    1 CONTINUE
    !
    !---- see if current Qspec, if any, didn't come from Mixed-Inverse
    IF(NSP.NE.NC1) THEN
        LQSPEC = .FALSE.
        IQ1 = 1
        IQ2 = NC1
    ENDIF
    !
    !---- initialize Fourier transform arrays if it hasn't been done
    IF(.NOT.LEIW) CALL EIWSET(NC1)
    LEIW = .TRUE.
    !
    !---- if Qspec alpha has never been set, set it to current alpha
    IF(NQSP .EQ. 0) THEN
        IACQSP = 1
        ALQSP(1) = ALFA
        NQSP = 1
    ENDIF
    !
    IF(.NOT.LSCINI) THEN
        !------ initialize s(w) for current airfoil, generating its Cn coefficients
        CALL SCINIT(N, X, XP, Y, YP, S, SLE)
        LSCINI = .TRUE.
        !
        !------ set up to initialize Qspec to current conditions
        LQSPEC = .FALSE.
    ENDIF
    !
    !---- set initial Q for current alpha
    ALGAM = ALFA
    CALL MAPGAM(1, ALGAM, CLGAM, CMGAM)
    WRITE(*, 1150) ALGAM / DTOR, CLGAM
    !
    IF(.NOT.LQSPEC) THEN
        !------ set Cn coefficients from current Q
        CALL CNCALC(QGAMM, .FALSE.)
        !
        !------ set Qspec from Cn coefficients
        CALL QSPCIR
        WRITE(*, 1190)
    ENDIF
    !
    !====================================================
    !---- start of menu loop
    500  CONTINUE
    COMOLD = COMAND
    ARGOLD = COMARG
    !
    501  IF(LQSYM) THEN
        CALL ASKC('.MDESs^', COMAND, COMARG)
    ELSE
        CALL ASKC('.MDES^', COMAND, COMARG)
    ENDIF
    !
    505  CONTINUE
    !
    !---- process previous command ?
    IF(COMAND(1:1).EQ.'!') THEN
        IF(COMOLD.EQ.'****') THEN
            WRITE(*, *) 'Previous .MDES command not valid'
            GO TO 501
        ELSE
            COMAND = COMOLD
            COMARG = ARGOLD
            LRECALC = .TRUE.
        ENDIF
    ELSE
        LRECALC = .FALSE.
    ENDIF
    !
    IF(COMAND.EQ.'    ') THEN
        !----- just <return> was typed... clean up plotting and exit OPER
        RETURN
    ENDIF
    !
    !---- extract command line numeric arguments
    DO I = 1, 20
        IINPUT(I) = 0
        RINPUT(I) = 0.0
    ENDDO
    NINPUT = 0
    CALL GETINT(COMARG, IINPUT, NINPUT, ERROR)
    NINPUT = 0
    CALL GETFLT(COMARG, RINPUT, NINPUT, ERROR)
    !
    !--------------------------------------------------------
    IF(COMAND.EQ.'?   ') THEN
        WRITE(*, 1050)
        1050  FORMAT(&
                /'   <cr>   Return to Top Level'&
                /'   !      Redo previous command'&
                //'   INIT   Re-initialize mapping'&
                /'   QSET   Reset Qspec <== Q'&
                /'   AQ r.. Show/select alpha(s) for Qspec'&
                /'   CQ r.. Show/select  CL(s)   for Qspec'&
                //'   Symm   Toggle symmetry flag'&
                /'   TGAP r Set new TE gap'&
                /'   TANG r Set new TE angle'&
                /'   SMOO   Smooth Qspec inside target segment'&
                /'   Filt   Apply Hanning filter to entire Qspec'&
                /'   SLOP   Toggle modified-Qspec slope matching flag'&
                //'   eXec   Execute  full-inverse calculation'&
                //'   PERT   Perturb one Cn and generate geometry')
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'INIT') THEN
        LQSPEC = .FALSE.
        LSCINI = .FALSE.
        GO TO 1
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'QSET') THEN
        CALL CNCALC(QGAMM, .FALSE.)
        IF(LQSYM) CALL CNSYMM
        CALL QSPCIR
        !
        LCNPL = .FALSE.
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'AQ  ') THEN
        !----- set Qspec(s) for specified alphas
        IF(NINPUT.GE.1) THEN
            NQSP = MIN(NINPUT, IPX)
            DO K = 1, NQSP
                ALQSP(K) = RINPUT(K) * DTOR
            ENDDO
        ELSE
            WRITE(*, 1150) ALGAM / DTOR, CLGAM
            WRITE(*, 1161) (ALQSP(K) / DTOR, K=1, NQSP)
            161    WRITE(*, 1162)
            1161   FORMAT(/' Current Qspec alphas  =', 20F9.3)
            1162   FORMAT(' New alphas or <return>:  ', $)
            READ (*, 5000) LINE
            NTMP = IPX
            CALL GETFLT(LINE, W1, NTMP, ERROR)
            IF(ERROR) GO TO 161
            NTMP = MIN(NTMP, IPX)
            !
            !------ if just <return> was hit, don't do anything
            IF(NTMP .EQ. 0) GO TO 500
            !
            NQSP = NTMP
            DO K = 1, NQSP
                ALQSP(K) = W1(K) * DTOR
            ENDDO
        ENDIF
        !
        IACQSP = 1
        CALL QSPCIR
        !
        LCNPL = .FALSE.
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'CQ  ') THEN
        !----- set Qspec(s) for specified CLs
        IF(NINPUT.GE.1) THEN
            NQSP = MIN(NINPUT, IPX)
            DO K = 1, NQSP
                CLQSP(K) = RINPUT(K)
            ENDDO
        ELSE
            WRITE(*, 1150) ALGAM / DTOR, CLGAM
            WRITE(*, 1171) (CLQSP(K), K = 1, NQSP)
            171    WRITE(*, 1172)
            1171   FORMAT(/' Current Qspec CLs  =', 20F8.4)
            1172   FORMAT(' New CLs or <return>:  ', $)
            READ (*, 5000) LINE
            NTMP = IPX
            CALL GETFLT(LINE, W1, NTMP, ERROR)
            IF(ERROR) GO TO 171
            NTMP = MIN(NTMP, IPX)
            !
            !------ if just <return> was hit, don't do anything
            IF(NTMP .EQ. 0) GO TO 500
            !
            NQSP = NTMP
            DO K = 1, NQSP
                CLQSP(K) = W1(K)
            ENDDO
        ENDIF
        !
        IACQSP = 2
        CALL QSPCIR
        !
        LCNPL = .FALSE.
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'SYMM' .OR.&
            COMAND.EQ.'S   ') THEN
        LQSYM = .NOT.LQSYM
        IF(LQSYM) THEN
            WRITE(*, *) 'Qspec symmetry forcing enabled.'
            !cc       KQSP = 1
            !cc       CALL SYMQSP(KQSP)
            !cc       CALL CNCALC(QSPEC(1,KQSP),.FALSE.)
            CALL CNSYMM
            CALL QSPCIR
            !
            LCNPL = .FALSE.
        ELSE
            WRITE(*, *) 'Qspec symmetry forcing disabled.'
        ENDIF
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'TGAP') THEN
        CALL DZTSET(RINPUT, NINPUT)
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'TANG') THEN
        CALL AGTSET(RINPUT, NINPUT)
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'READ') THEN
        !----- read in Qspec
        KQSP = 1
        CALL GETVOV(KQSP)
        CALL CNCALC(QSPEC(1, KQSP), .FALSE.)
        IF(LQSYM) CALL CNSYMM
        LCNPL = .FALSE.
        !
        KQSP = 1
        CALL QSPINT(ALQSP(KQSP), QSPEC(1, KQSP), QINF, MINF, &
                CLQSP(KQSP), CMQSP(KQSP))
        WRITE(*, 1200) ALGAM / DTOR, CLGAM, CMGAM
        WRITE(*, 1210) KQSP, ALQSP(KQSP) / DTOR, CLQSP(KQSP), CMQSP(KQSP)
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'SMOO') THEN
        !----- smooth Qspec within target segment
        KQSP = KQTARG
        CALL SMOOQ(IQ1, IQ2, KQSP)
        CALL CNCALC(QSPEC(1, KQSP), LQSYM)
        CALL QSPCIR
        !
        WRITE(*, 1200) ALGAM / DTOR, CLGAM, CMGAM
        !
        DO KQSP = 1, NQSP
            CALL QSPINT(ALQSP(KQSP), QSPEC(1, KQSP), QINF, MINF, &
                    CLQ, CMQSP(KQSP))
            !
            !------- set new CL only if alpha is prescribed
            IF(IACQSP.EQ.1) CLQSP(KQSP) = CLQ
            !
            WRITE(*, 1210) KQSP, ALQSP(KQSP) / DTOR, CLQSP(KQSP), CMQSP(KQSP)
        ENDDO
        LQSPPL = .FALSE.
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'FILT' .OR.&
            COMAND.EQ.'F   ') THEN
        !----- apply modified Hanning filter to Cn coefficients
        CFILT = 0.2
        CALL CNFILT(CFILT)
        CALL PIQSUM
        CALL QSPCIR
        !
        WRITE(*, 1200) ALGAM / DTOR, CLGAM, CMGAM
        !
        DO KQSP = 1, NQSP
            CALL QSPINT(ALQSP(KQSP), QSPEC(1, KQSP), QINF, MINF, &
                    CLQ, CMQSP(KQSP))
            !
            !------- set new CL only if alpha is prescribed
            IF(IACQSP.EQ.1) CLQSP(KQSP) = CLQ
            !
            WRITE(*, 1210) KQSP, ALQSP(KQSP) / DTOR, CLQSP(KQSP), CMQSP(KQSP)
        ENDDO
        LQSPPL = .FALSE.
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'DUMP') THEN
        FNAME = COMARG
        IF(FNAME(1:1).EQ.' ')&
                CALL ASKS('Enter Cn output filename^', FNAME)
        !
        LU = 19
        OPEN(LU, FILE = FNAME, STATUS = 'UNKNOWN')
        CALL CNDUMP(LU)
        CLOSE(LU)
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'EXEC' .OR.&
            COMAND.EQ.'X   ') THEN
        !----- execute full-inverse calculation
        CALL MAPGEN(FFILT, NB, XB, YB)
        !
        !----- spline new buffer airfoil
        CALL SCALC(XB, YB, SB, NB)
        CALL SPLIND(XB, XBP, SB, NB, -999.0, -999.0)
        CALL SPLIND(YB, YBP, SB, NB, -999.0, -999.0)
        !
        CALL GEOPAR(XB, XBP, YB, YBP, SB, NB, W1, &
                SBLE, CHORDB, AREAB, RADBLE, ANGBTE, &
                EI11BA, EI22BA, APX1BA, APX2BA, &
                EI11BT, EI22BT, APX1BT, APX2BT, &
                THICKB, CAMBRB)
        !
        !
        LQSPPL = .FALSE.
        LGSAME = .FALSE.
        LCNPL = .FALSE.
        !
        WRITE(*, 1300)
        1300  FORMAT(//' New buffer airfoil generated'&
                /' Execute PANE at Top Level to set new current airfoil'/)
        !
        !--------------------------------------------------------
    ELSEIF(COMAND.EQ.'PERT') THEN
        CALL PERT(QSPEC(1, 1))
        !----- set Q(s) for changed Cn
        CALL QSPCIR
        !----- go generate perturbed geometry
        COMAND = 'EXEC'
        COMARG = ' '
        GO TO 505
        !
        !--------------------------------------------------------
    ELSE
        WRITE(*, 1100) COMAND
        1100  FORMAT(' Command ', A4, ' not recognized.  Type a " ? " for list.')
        COMAND = '****'
        !
    ENDIF
    !
    GO TO 500
    !
    !....................................................
    !
    1150 FORMAT(/' Current Q operating condition:', &
            '   alpha = ', F7.3, '     CL = ', F8.4)
    1190 FORMAT(/' Qspec initialized to current Q')
    1200 FORMAT(&
            /' Current :  alpha =', F9.4, '    CL =', F11.6, '    CM =', F11.6)
    1210 FORMAT(&
            ' Qspec', I2, &
            ' :  alpha =', F9.4, '    CL =', F11.6, '    CM =', F11.6)
    5000 FORMAT(A)
END
! MDES


SUBROUTINE DZTSET(RINPUT, NINPUT)
    use m_spline
    INCLUDE 'CIRCLE.INC'
    DIMENSION RINPUT(*)
    !
    IF(NINPUT.GE.2) THEN
        DXNEW = RINPUT(1)
        DYNEW = RINPUT(2)
    ELSE
        WRITE(*, 1170) REAL(DZTE), IMAG(DZTE)
        1170  FORMAT(/' Current TE gap  dx/c dy/c =', 2F7.4)
        CALL ASKR('Enter new TE gap dx/c^', DXNEW)
        CALL ASKR('Enter new TE gap dy/c^', DYNEW)
    ENDIF
    !
    DZTE = CMPLX(DXNEW, DYNEW)
    RETURN
END


SUBROUTINE AGTSET(RINPUT, NINPUT)
    use m_spline
    INCLUDE 'CIRCLE.INC'
    DIMENSION RINPUT(*)
    !
    IF(NINPUT.GE.2) THEN
        AGTED = RINPUT(1)
    ELSE
        WRITE(*, 1180) AGTE * 180.0
        1180  FORMAT(/' Current TE angle =', F7.3, ' deg.')
        CALL ASKR('Enter new TE angle (deg)^', AGTED)
    ENDIF
    !
    AGTE = AGTED / 180.0
    RETURN
END


SUBROUTINE MAPGAM(IAC, ALG, CLG, CMG)
    !--------------------------------------------
    !     Sets mapped Q for current airfoil
    !     for angle of attack or CL.
    !
    !       IAC=1: specified ALGAM
    !       IAC=2: specified CLGAM
    !--------------------------------------------
    use m_spline
    use i_xfoil
    !
    !---- calculate q(w), set number of circle points NSP
    CALL QCCALC(IAC, ALG, CLG, CMG, MINF, QINF, NSP, W1, W2, W5, W6)
    !
    !---- store q(w), s(w), x(w), y(w)
    CHX = XTE - XLE
    CHY = YTE - YLE
    CHSQ = CHX**2 + CHY**2
    DO 3 I = 1, NSP
        QGAMM(I) = W6(I)
        SSPEC(I) = W5(I)
        XIC = SEVAL(S(N) * SSPEC(I), X, XP, S, N)
        YIC = SEVAL(S(N) * SSPEC(I), Y, YP, S, N)
        XSPOC(I) = ((XIC - XLE) * CHX + (YIC - YLE) * CHY) / CHSQ
        YSPOC(I) = ((YIC - YLE) * CHX - (XIC - XLE) * CHY) / CHSQ
    3 CONTINUE
    SSPLE = SLE / S(N)
    !
    RETURN
END
! MAPGAM


SUBROUTINE QSPCIR
    !----------------------------------------------------
    !     Sets Qspec arrays for all design alphas or CLs
    !----------------------------------------------------
    use m_spline
    use i_xfoil
    !
    DO 10 KQSP = 1, NQSP
        CALL QCCALC(IACQSP, ALQSP(KQSP), CLQSP(KQSP), CMQSP(KQSP), &
                MINF, QINF, NSP, W1, W2, W5, QSPEC(1, KQSP))
        CALL SPLQSP(KQSP)
    10   CONTINUE
    LQSPEC = .TRUE.
    !
    RETURN
END


SUBROUTINE MAPGEN(FFILT, N, X, Y)
    !--------------------------------------------------------
    !     Calculates the geometry from the speed function
    !     Fourier coefficients Cn, modifying them as needed
    !     to achieve specified constraints.
    !--------------------------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    DIMENSION X(NC), Y(NC)
    !
    COMPLEX QQ(IMX / 4, IMX / 4), DCN(IMX / 4)
    !
    !---- preset rotation offset of airfoil so that initial angle is close
    !-    to the old airfoil's angle
    DX = XCOLD(2) - XCOLD(1)
    DY = YCOLD(2) - YCOLD(1)
    QIM0 = ATAN2(DX, -DY) + 0.5 * PI * (1.0 + AGTE)
    QIMOFF = QIM0 - IMAG(CN(0))
    CN(0) = CN(0) + CMPLX(0.0, QIMOFF)
    !
    !---- inverse-transform and calculate geometry ZC = z(w)
    !cc   CALL CNFILT(FFILT)
    CALL PIQSUM
    CALL ZCCALC(MCT)
    !
    !---- scale,rotate z(w) to get previous chord and orientation
    CALL ZCNORM(MCT)
    !
    !CCC---- put back rotation offset so speed routine QCCALC gets the right alpha
    !CC      CN(0) = CN(0) - CMPLX( 0.0 , QIMOFF )
    !
    !---- enforce Lighthill's first constraint
    CN(0) = CMPLX(0.0, IMAG(CN(0)))
    !
    !---- number of free coefficients
    NCN = 1
    !
    !---- Newton iteration loop for modified Cn's
    DO 100 ITERCN = 1, 10
        DO M = 1, NCN
            DO L = 1, NCN
                QQ(M, L) = 0.
            ENDDO
            DCN(M) = 0.
            QQ(M, M) = 1.0
        ENDDO
        !
        !------ fix TE gap
        M = 1
        DCN(M) = ZC(1) - ZC(NC) - DZTE
        DO L = 1, NCN
            QQ(M, L) = ZC_CN(1, L) - ZC_CN(NC, L)
        ENDDO
        !
        CALL CGAUSS(IMX / 4, NCN, QQ, DCN, 1)
        !
        DCNMAX = 0.
        DO M = 1, NCN
            CN(M) = CN(M) - DCN(M)
            DCNMAX = MAX(ABS(DCN(M)), DCNMAX)
        ENDDO
        !
        !cc     CALL CNFILT(FFILT)
        CALL PIQSUM
        !
        CALL ZCCALC(MCT)
        CALL ZCNORM(MCT)
        !
        WRITE(*, *) ITERCN, DCNMAX
        IF(DCNMAX.LE.5.0E-5) GO TO 101
    100 CONTINUE
    WRITE(*, *)
    WRITE(*, *) 'MAPGEN: Geometric constraints not fully converged'
    !
    101 CONTINUE
    !
    !---- return new airfoil coordinates
    N = NC
    DO 120 I = 1, NC
        X(I) = REAL(ZC(I))
        Y(I) = IMAG(ZC(I))
    120  CONTINUE
    !
    RETURN
END
! MAPGEN


SUBROUTINE SCINIT(N, X, XP, Y, YP, S, SLE)
    !----------------------------------------------------------
    !     Calculates the circle-plane coordinate s(w) = SC
    !     at each point of the current geometry.  
    !     A by-product is the complex-mapping coefficients Cn.
    !     (see CNCALC header for more info).
    !----------------------------------------------------------
    use m_spline
    DIMENSION X(N), XP(N), Y(N), YP(N), S(N)
    !
    INCLUDE 'CIRCLE.INC'
    COMPLEX DCN, ZLE, ZTE
    !c      DATA CEPS, SEPS / 1.0E-5, 5.0E-5 /
    DATA CEPS, SEPS / 1.0E-7, 5.0E-7 /
    !
    !---- set TE angle parameter
    AGTE = (ATAN2(XP(N), -YP(N))&
            - ATAN2(XP(1), -YP(1))) / PI - 1.0
    !
    !---- set surface angle at first point
    AG0 = ATAN2(XP(1), -YP(1))
    !
    !---- temporary offset Qo to make  Q(w)-Qo = 0  at  w = 0 , 2 pi
    !-     --- avoids Gibbs problems with Q(w)'s Fourier sine transform
    QIM0 = AG0 + 0.5 * PI * (1.0 + AGTE)
    !
    XLE = SEVAL(SLE, X, XP, S, N)
    YLE = SEVAL(SLE, Y, YP, S, N)
    !
    !---- save TE gap and airfoil chord
    DXTE = X(1) - X(N)
    DYTE = Y(1) - Y(N)
    DZTE = CMPLX(DXTE, DYTE)
    !
    CHORDX = 0.5 * (X(1) + X(N)) - XLE
    CHORDY = 0.5 * (Y(1) + Y(N)) - YLE
    CHORDZ = CMPLX(CHORDX, CHORDY)
    ZLEOLD = CMPLX(XLE, YLE)
    !
    WRITE(*, 1100) REAL(DZTE), IMAG(DZTE), AGTE * 180.0
    1100 FORMAT(/' Current TE gap  dx dy =', 2F7.4, &
            '    TE angle =', F7.3, ' deg.' /)
    WRITE(*, *) 'Initializing mapping coordinate ...'
    !
    !---- set approximate slope ds/dw at airfoil nose
    CVLE = CURV(SLE, X, XP, Y, YP, S, N) * S(N)
    CVABS = ABS(CVLE)
    DSDWLE = MAX(1.0E-3, 0.5 / CVABS)
    !
    TOPS = SLE / S(N)
    BOTS = (S(N) - SLE) / S(N)
    !
    !---- set initial top surface s(w)
    WWT = 1.0 - 2.0 * DSDWLE / TOPS
    DO 10 IC = 1, (NC - 1) / 2 + 1
        SC(IC) = TOPS * (1.0 - COS(WWT * WC(IC)))&
                / (1.0 - COS(WWT * PI))
    10 CONTINUE
    !
    !---- set initial bottom surface s(w)
    WWT = 1.0 - 2.0 * DSDWLE / BOTS
    DO 15 IC = (NC - 1) / 2 + 2, NC
        SC(IC) = 1.0&
                - BOTS * (1.0 - COS(WWT * (WC(NC) - WC(IC))))&
                        / (1.0 - COS(WWT * PI))
    15 CONTINUE
    !
    !---- iteration loop for s(w) array
    DO 500 IPASS = 1, 30
        !
        !---- calculate imaginary part of harmonic function  P(w) + iQ(w)
        DO 20 IC = 1, NC
            !
            SIC = S(1) + (S(N) - S(1)) * SC(IC)
            DXDS = DEVAL(SIC, X, XP, S, N)
            DYDS = DEVAL(SIC, Y, YP, S, N)
            !
            !------ set Q(w) - Qo   (Qo defined so that Q(w)-Qo = 0  at  w = 0 , 2 pi)
            QIM = ATAN2(DXDS, -DYDS)&
                    - 0.5 * (WC(IC) - PI) * (1.0 + AGTE)&
                    - QIM0
            !
            PIQ(IC) = CMPLX(0.0, QIM)
            !
        20 CONTINUE
        !
        !---- Fourier-decompose Q(w)
        CALL FTP
        !
        !---- zero out average real part and add on Qo we took out above
        CN(0) = CMPLX(0.0, IMAG(CN(0)) + QIM0)
        !
        !---- transform back to get entire  PIQ = P(w) + iQ(w)
        CALL PIQSUM
        !
        !---- save s(w) for monitoring of changes in s(w) by ZCCALC
        DO 30 IC = 1, NC
            SCOLD(IC) = SC(IC)
        30 CONTINUE
        !
        !---- correct n=1 complex coefficient Cn for proper TE gap
        DO 40 ITGAP = 1, 5
            CALL ZCCALC(1)
            !
            !------ set current LE,TE locations
            CALL ZLEFIND(ZLE, ZC, WC, NC, PIQ, AGTE)
            ZTE = 0.5 * (ZC(1) + ZC(NC))
            !
            DZWT = ABS(ZTE - ZLE) / ABS(CHORDZ)
            DCN = -(ZC(1) - ZC(NC) - DZWT * DZTE)&
                    / (ZC_CN(1, 1) - ZC_CN(NC, 1))
            CN(1) = CN(1) + DCN
            !
            CALL PIQSUM
            IF(ABS(DCN) .LT. CEPS) GO TO 41
        40 CONTINUE
        41 CONTINUE
        !
        DSCMAX = 0.
        DO 50 IC = 1, NC
            DSCMAX = MAX(DSCMAX, ABS(SC(IC) - SCOLD(IC)))
        50 CONTINUE
        !
        WRITE(*, *) IPASS, '     max(dw) =', DSCMAX
        IF(DSCMAX .LT. SEPS) GO TO 505
        !
    500 CONTINUE
    505 CONTINUE
    !
    !---- normalize final geometry
    CALL ZCNORM(1)
    !
    !---- set final  s(w), x(w), y(w)  arrays for old airfoil
    DO 510 IC = 1, NC
        SCOLD(IC) = SC(IC)
        XCOLD(IC) = REAL(ZC(IC))
        YCOLD(IC) = IMAG(ZC(IC))
    510 CONTINUE
    !
    DO 600 IC = 1, NC
        SINW = 2.0 * SIN(0.5 * WC(IC))
        SINWE = 0.
        IF(SINW.GT.0.0) SINWE = SINW**(1.0 - AGTE)
        !
        HWC = 0.5 * (WC(IC) - PI) * (1.0 + AGTE) - 0.5 * PI
        ZCOLDW(IC) = SINWE * EXP(PIQ(IC) + CMPLX(0.0, HWC))
    600 CONTINUE
    !
    QIMOLD = IMAG(CN(0))
    !
    !C---- print out Fourier coefficients
    !      write(*,*) ' '
    !      do 700 m=0, mc
    !        write(*,*) m, real(cn(m)), IMAG(cn(m))
    !        write(1,*) m, real(cn(m)), IMAG(cn(m))
    !cc 7000   format(1x,i3,2f10.6)
    !  700 continue
    !
    RETURN
END
! SCINIT



SUBROUTINE CNCALC(QC, LSYMM)
    !----------------------------------------------------------
    !     Calculates the complex Fourier coefficients Cn of
    !     the real part of the harmonic function P(w) + iQ(w)
    !     which is set from either the current surface speed
    !     function
    !                                                  e
    !                   2 cos(w/2 - alpha) [2 sin(w/2)]
    !       P(w) =  ln  -------------------------------
    !                               q(w)
    !
    !
    !     or the geometry function
    !
    !                                         e
    !                       z'(w) [2 sin(w/2)]
    !          P(w) =   ln  ------------------
    !                           2 sin(w/2)
    !
    !     depending on whether the speed q(w) or the
    !     geometry z(w) is specified for that particular
    !     value of w.  
    !     (z(w) option is currently implemented separately in SCINIT)
    !
    !     By Fourier-transforming P(w) into a sequence 
    !     of Fourier coefficients Cn, its complex conjugate 
    !     function Q(w) is automatically determined by an 
    !     inverse transformation in PIQSUM.  The overall 
    !     P(w) + iQ(w) then uniquely defines the overall 
    !     airfoil geometry, which is calculated in ZCCALC.
    !
    !     If LSYMM=t, then the Real(Cn) change from current
    !     Cn values is doubled, and Imag(Cn) is zeroed out.
    !----------------------------------------------------------
    use m_spline
    REAL QC(NC)
    LOGICAL LSYMM
    !
    INCLUDE 'CIRCLE.INC'
    DIMENSION QCW(ICX)
    !
    COMPLEX CNSAV(0:IMX)
    !
    !c      REAL WCJ(2)
    !
    IF(NC .GT. ICX) STOP 'CNCALC: Array overflow.'
    !
    !cC---- assume q(w) segment is entire airfoil
    !c      WCJ(1) = WC(1)
    !c      WCJ(2) = WC(NC)
    !cC
    !c      IF(LIQSET) THEN
    !cC----- set w at q(w) segment endpoints
    !c       WCJ(1) = WC(IQ1)
    !c       WCJ(2) = WC(IQ2)
    !c      ENDIF
    !
    !---- spline q(w)
    CALL SPLIND(QC, QCW, WC, NC, -999.0, -999.0)
    !
    !---- get approximate w value at stagnation point
    DO 10 IC = 2, NC
        IF(QC(IC).LT.0.0) GO TO 11
    10 CONTINUE
    11 WCLE = WC(IC)
    !
    !---- set exact numerical w value at stagnation point from splined q(w)
    CALL SINVRT(WCLE, 0.0, QC, QCW, WC, NC)
    !
    !---- set corresponding circle plane alpha
    ALFCIR = 0.5 * (WCLE - PI)
    !
    !---- calculate real part of harmonic function  P(w) + iQ(w)
    DO 120 IC = 2, NC - 1
        !
        COSW = 2.0 * COS(0.5 * WC(IC) - ALFCIR)
        SINW = 2.0 * SIN(0.5 * WC(IC))
        SINWE = SINW**AGTE
        !
        !c        IF(WC(IC).GE.WCJ(1) .AND. WC(IC).LE.WCJ(2)) THEN
        !
        !------- set P(w) from q(w)
        IF(ABS(COSW).LT.1.0E-4) THEN
            !-------- use asymptotic form near stagnation point
            PFUN = ABS(SINWE / QCW(IC))
        ELSE
            !-------- use actual expression
            PFUN = ABS(COSW * SINWE / QC(IC))
        ENDIF
        !
        !c        ELSE
        !cC
        !cC------- set P(w) from old geometry derivative z'(w)
        !c         PFUN = ABS( ZCOLDW(IC)*SINWE/SINW )
        !cC
        !c        ENDIF
        !
        PIQ(IC) = CMPLX(LOG(PFUN), 0.0)
        !
    120 CONTINUE
    !
    !---- extrapolate P(w) to TE
    PIQ(1) = 3.0 * PIQ(2) - 3.0 * PIQ(3) + PIQ(4)
    PIQ(NC) = 3.0 * PIQ(NC - 1) - 3.0 * PIQ(NC - 2) + PIQ(NC - 3)
    !
    DO 50 M = 0, MC
        CNSAV(M) = CN(M)
    50   CONTINUE
    !
    !---- Fourier-transform P(w) to get new Cn coefficients
    CALL FTP
    CN(0) = CMPLX(0.0, QIMOLD)
    !
    IF(LSYMM) THEN
        DO 60 M = 1, MC
            CNR = 2.0 * REAL(CN(M)) - REAL(CNSAV(M))
            CN(M) = CMPLX(CNR, 0.0)
        60     CONTINUE
    ENDIF
    !
    CALL PIQSUM
    !
    RETURN
END
! CNCALC


SUBROUTINE CNSYMM
    use m_spline
    INCLUDE 'CIRCLE.INC'
    !
    !---- eliminate imaginary (camber) parts of mapping coefficients
    DO 10 M = 1, MC
        CN(M) = CMPLX(REAL(CN(M)), 0.0)
    10 CONTINUE
    !
    CALL PIQSUM
    RETURN
END
! CNSYMM


SUBROUTINE PIQSUM
    !---------------------------------------------
    !     Inverse-transform to get back modified 
    !     speed function and its conjugate.
    !---------------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    COMPLEX ZSUM
    !
    DO 300 IC = 1, NC
        ZSUM = (0.0, 0.0)
        DO 310 M = 0, MC
            ZSUM = ZSUM + CN(M) * CONJG(EIW(IC, M))
        310   CONTINUE
        PIQ(IC) = ZSUM
    300 CONTINUE
    !
    RETURN
END
! PIQSUM


SUBROUTINE CNFILT(FFILT)
    !-------------------------------------
    !     Filters out upper harmonics 
    !     with modified Hanning filter.
    !-------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    !
    IF(FFILT.EQ.0.0) RETURN
    !
    DO 10 M = 0, MC
        FREQ = FLOAT(M) / FLOAT(MC)
        CWT = 0.5 * (1.0 + COS(PI * FREQ))
        CWTX = CWT
        IF(FFILT.GT.0.0) CWTX = CWT**FFILT
        CN(M) = CN(M) * CWTX
    10 CONTINUE
    !
    RETURN
END
! CNFILT


SUBROUTINE ZCCALC(MTEST)
    !--------------------------------------------------------
    !     Calculates the airfoil geometry z(w) from the
    !     harmonic function P(w) + iQ(w).  Also normalizes
    !     the coordinates to the old chord and calculates
    !     the geometry sensitivities dz/dCn  (1 < n < MTEST)
    !     for each point.
    !--------------------------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    COMPLEX DZDW1, DZDW2, DZ_PIQ1, DZ_PIQ2
    !
    !---- integrate upper airfoil surface coordinates from x,y = 4,0
    IC = 1
    ZC(IC) = (4.0, 0.0)
    DO 10 M = 1, MTEST
        ZC_CN(IC, M) = (0.0, 0.0)
    10 CONTINUE
    !
    SINW = 2.0 * SIN(0.5 * WC(IC))
    SINWE = 0.
    IF(SINW.GT.0.0) SINWE = SINW**(1.0 - AGTE)
    !
    HWC = 0.5 * (WC(IC) - PI) * (1.0 + AGTE) - 0.5 * PI
    DZDW1 = SINWE * EXP(PIQ(IC) + CMPLX(0.0, HWC))
    DO 20 IC = 2, NC
        !
        SINW = 2.0 * SIN(0.5 * WC(IC))
        SINWE = 0.
        IF(SINW.GT.0.0) SINWE = SINW**(1.0 - AGTE)
        !
        HWC = 0.5 * (WC(IC) - PI) * (1.0 + AGTE) - 0.5 * PI
        DZDW2 = SINWE * EXP(PIQ(IC) + CMPLX(0.0, HWC))
        !
        ZC(IC) = 0.5 * (DZDW1 + DZDW2) * DWC + ZC(IC - 1)
        DZ_PIQ1 = 0.5 * (DZDW1) * DWC
        DZ_PIQ2 = 0.5 * (DZDW2) * DWC
        !
        DO 210 M = 1, MTEST
            ZC_CN(IC, M) = DZ_PIQ1 * CONJG(EIW(IC - 1, M))&
                    + DZ_PIQ2 * CONJG(EIW(IC, M))&
                    + ZC_CN(IC - 1, M)
        210   CONTINUE
        !
        DZDW1 = DZDW2
    20 CONTINUE
    !
    !---- set arc length array s(w)
    SC(1) = 0.
    DO 50 IC = 2, NC
        SC(IC) = SC(IC - 1) + ABS(ZC(IC) - ZC(IC - 1))
    50 CONTINUE
    !
    !---- normalize arc length
    DO 60 IC = 1, NC
        SC(IC) = SC(IC) / SC(NC)
    60 CONTINUE
    !
    RETURN
END
! ZCCALC


SUBROUTINE ZCNORM(MTEST)
    !-----------------------------------------------
    !     Normalizes the complex airfoil z(w) to
    !     the old chord and angle, and resets the
    !     influence coefficients  dz/dCn .
    !-----------------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    COMPLEX DZDW1, DZDW2
    COMPLEX ZCNEW, ZLE, ZTE, ZC_ZTE, ZTE_CN(IMX / 4)
    !
    !---- find current LE location
    CALL ZLEFIND(ZLE, ZC, WC, NC, PIQ, AGTE)
    !
    !---- place leading edge at origin
    DO 60 IC = 1, NC
        ZC(IC) = ZC(IC) - ZLE
    60 CONTINUE
    !
    !---- set normalizing quantities and sensitivities
    ZTE = 0.5 * (ZC(1) + ZC(NC))
    DO 480 M = 1, MTEST
        ZTE_CN(M) = 0.5 * (ZC_CN(1, M) + ZC_CN(NC, M))
    480 CONTINUE
    !
    !---- normalize airfoil to proper chord, put LE at old position,
    !-    and set sensitivities dz/dCn for the rescaled coordinates
    DO 500 IC = 1, NC
        ZCNEW = CHORDZ * ZC(IC) / ZTE
        ZC_ZTE = -ZCNEW / ZTE
        ZC(IC) = ZCNEW
        DO 510 M = 1, MTEST
            ZC_CN(IC, M) = CHORDZ * ZC_CN(IC, M) / ZTE + ZC_ZTE * ZTE_CN(M)
        510   CONTINUE
    500 CONTINUE
    !
    !---- add on rotation to mapping coefficient so QCCALC gets the right alpha
    QIMOFF = -IMAG(LOG(CHORDZ / ZTE))
    CN(0) = CN(0) - CMPLX(0.0, QIMOFF)
    !
    !---- shift airfoil to put LE at old location
    DO 600 IC = 1, NC
        ZC(IC) = ZC(IC) + ZLEOLD
    600  CONTINUE
    !
    RETURN
END
! ZCNORM


SUBROUTINE QCCALC(ISPEC, ALFA, CL, CM, MINF, QINF, &
        NCIR, XCIR, YCIR, SCIR, QCIR)
    !---------------------------------------------------
    !     Calculates the surface speed from the complex
    !     speed function so that either a prescribed
    !     ALFA or CL is achieved, depending on whether 
    !     ISPEC=1 or 2.  The CL calculation uses the 
    !     transformed Karman-Tsien Cp.
    !---------------------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    COMPLEX DZ, ZA, EIA, CMT, CFT, CFT_A
    DIMENSION XCIR(NC), YCIR(NC), SCIR(NC), QCIR(NC)
    DIMENSION QC_A(ICX)
    REAL MINF
    DATA AEPS / 5.0E-7 /
    !
    !---- Karman-Tsien quantities
    BETA = SQRT(1.0 - MINF**2)
    BFAC = 0.5 * MINF**2 / (1.0 + BETA)
    !
    NCIR = NC
    !
    !---- Newton iteration loop (executed only once if alpha specified)
    DO 1 IPASS = 1, 10
        !
        !------ set alpha in the circle plane
        ALFCIR = ALFA - IMAG(CN(0))
        !
        CMT = (0.0, 0.0)
        CFT = (0.0, 0.0)
        CFT_A = (0.0, 0.0)
        !
        !------ set surface speed for current circle plane alpha
        DO 10 IC = 1, NC
            PPP = REAL(PIQ(IC))
            EPPP = EXP(-PPP)
            SINW = 2.0 * SIN(0.5 * WC(IC))
            !
            IF(AGTE.EQ.0.0) THEN
                SINWE = 1.0
            ELSE IF(SINW.GT.0.0) THEN
                SINWE = SINW**AGTE
            ELSE
                SINWE = 0.0
            ENDIF
            !
            QCIR(IC) = 2.0 * COS(0.5 * WC(IC) - ALFCIR) * SINWE * EPPP
            QC_A(IC) = 2.0 * SIN(0.5 * WC(IC) - ALFCIR) * SINWE * EPPP
            !
            XCIR(IC) = REAL(ZC(IC))
            YCIR(IC) = IMAG(ZC(IC))
            SCIR(IC) = SC(IC)
        10   CONTINUE
        !
        !------ integrate compressible  Cp dz  to get complex force  CL + iCD
        IC = 1
        CPINC1 = 1.0 - (QCIR(IC) / QINF)**2
        CPI_Q1 = -2.0 * QCIR(IC) / QINF**2
        CPCOM1 = CPINC1 / (BETA + BFAC * CPINC1)
        CPC_Q1 = (1.0 - BFAC * CPCOM1) / (BETA + BFAC * CPINC1) * CPI_Q1
        CPC_A1 = CPC_Q1 * QC_A(IC)
        DO 20 IC = 1, NC
            ICP = IC + 1
            IF(IC.EQ.NC) ICP = 1
            !
            CPINC2 = 1.0 - (QCIR(ICP) / QINF)**2
            CPI_Q2 = -2.0 * QCIR(ICP) / QINF**2
            CPCOM2 = CPINC2 / (BETA + BFAC * CPINC2)
            CPC_Q2 = (1.0 - BFAC * CPCOM2) / (BETA + BFAC * CPINC2) * CPI_Q2
            CPC_A2 = CPC_Q2 * QC_A(ICP)
            !
            ZA = (ZC(ICP) + ZC(IC)) * 0.5 - (0.25, 0.0)
            DZ = ZC(ICP) - ZC(IC)
            !
            CMT = CMT - 0.5 * (CPCOM1 + CPCOM2) * DZ * CONJG(ZA)&
                    + (CPCOM1 - CPCOM2) * DZ * CONJG(DZ) / 12.0
            CFT = CFT + 0.5 * (CPCOM1 + CPCOM2) * DZ
            CFT_A = CFT_A + 0.5 * (CPC_A1 + CPC_A2) * DZ
            !
            CPCOM1 = CPCOM2
            CPC_A1 = CPC_A2
        20   CONTINUE
        !
        !------ rotate force vector into freestream coordinates
        EIA = EXP(CMPLX(0.0, -ALFA))
        CFT = CFT * EIA
        CFT_A = CFT_A * EIA + CFT * (0.0, -1.0)
        !
        !------ lift is real part of complex force vector
        CLT = REAL(CFT)
        CLT_A = REAL(CFT_A)
        !
        !------ moment is real part of complex moment
        CM = REAL(CMT)
        !
        IF(ISPEC.EQ.1) THEN
            !------- if alpha is prescribed, we're done
            CL = CLT
            RETURN
        ELSE
            !------- adjust alpha with Newton-Raphson to get specified CL
            DALFA = (CL - CLT) / CLT_A
            ALFA = ALFA + DALFA
            IF(ABS(DALFA) .LT. AEPS) RETURN
        ENDIF
        !
    1 CONTINUE
    WRITE(*, *) 'QCCALC: CL convergence failed.  dAlpha =', DALFA
    !
    RETURN
END
! QCCALC



SUBROUTINE QSPINT(ALQSP, QSPEC, QINF, MINF, CLQSP, CMQSP)
    !--------------------------------------------
    !     Integrates circle-plane array surface 
    !     pressures to get CL and CM
    !--------------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    DIMENSION QSPEC(NC)
    REAL MINF
    !
    SA = SIN(ALQSP)
    CA = COS(ALQSP)
    !
    BETA = SQRT(1.0 - MINF**2)
    BFAC = 0.5 * MINF**2 / (1.0 + BETA)
    !
    CLQSP = 0.0
    CMQSP = 0.0
    !
    I = 1
    CQINC = 1.0 - (QSPEC(I) / QINF)**2
    CPQ1 = CQINC / (BETA + BFAC * CQINC)
    !
    DO 10 I = 1, NC
        IP = I + 1
        IF(I.EQ.NC) IP = 1
        !
        CQINC = 1.0 - (QSPEC(IP) / QINF)**2
        CPQ2 = CQINC / (BETA + BFAC * CQINC)
        !
        DX = (XCOLD(IP) - XCOLD(I)) * CA + (YCOLD(IP) - YCOLD(I)) * SA
        DY = (YCOLD(IP) - YCOLD(I)) * CA - (XCOLD(IP) - XCOLD(I)) * SA
        DU = CPQ2 - CPQ1
        !
        AX = 0.5 * (XCOLD(IP) + XCOLD(I)) * CA + 0.5 * (YCOLD(IP) + YCOLD(I)) * SA
        AY = 0.5 * (YCOLD(IP) + YCOLD(I)) * CA - 0.5 * (XCOLD(IP) + XCOLD(I)) * SA
        AQ = 0.5 * (CPQ2 + CPQ1)
        !
        CLQSP = CLQSP + DX * AQ
        CMQSP = CMQSP - DX * (AQ * (AX - 0.25) + DU * DX / 12.0)&
                - DY * (AQ * AY + DU * DY / 12.0)
        !
        CPQ1 = CPQ2
    10 CONTINUE
    !
    RETURN
END
! QSPINT


SUBROUTINE FTP
    !----------------------------------------------------------------
    !     Slow-Fourier-Transform P(w) using Trapezoidal integration.
    !----------------------------------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    COMPLEX ZSUM
    !
    DO 200 M = 0, MC
        ZSUM = (0.0, 0.0)
        DO 210 IC = 2, NC - 1
            ZSUM = ZSUM + PIQ(IC) * EIW(IC, M)
        210   CONTINUE
        CN(M) = (0.5 * (PIQ(1) * EIW(1, M) + PIQ(NC) * EIW(NC, M))&
                + ZSUM) * DWC / PI
    200 CONTINUE
    CN(0) = 0.5 * CN(0)
    !
    RETURN
END
! FTP


SUBROUTINE EIWSET(NC1)
    !----------------------------------------------------
    !     Calculates the uniformly-spaced circle-plane
    !     coordinate array WC (omega), and the
    !     corresponding complex unit numbers exp(inw)
    !     for Slow Fourier Transform operations.
    !----------------------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    !
    PI = 4.0 * ATAN(1.0)
    !
    !---- set requested number of points in circle plane
    NC = NC1
    MC = NC1 / 4
    MCT = NC1 / 16
    !
    IF(NC.GT.ICX) STOP 'EIWSET: Array overflow. Increase ICX.'
    !
    DWC = 2.0 * PI / FLOAT(NC - 1)
    !
    DO 10 IC = 1, NC
        WC(IC) = DWC * FLOAT(IC - 1)
    10 CONTINUE
    !
    !---- set  m = 0  numbers
    DO 20 IC = 1, NC
        EIW(IC, 0) = (1.0, 0.0)
    20 CONTINUE
    !
    !---- set  m = 1  numbers
    DO 30 IC = 1, NC
        EIW(IC, 1) = EXP(CMPLX(0.0, WC(IC)))
    30 CONTINUE
    !
    !---- set  m > 1  numbers by indexing appropriately from  m = 1  numbers
    DO 40 M = 2, MC
        DO 410 IC = 1, NC
            IC1 = M * (IC - 1)
            IC1 = MOD(IC1, (NC - 1)) + 1
            EIW(IC, M) = EIW(IC1, 1)
        410   CONTINUE
    40 CONTINUE
    !
    RETURN
END
! EIWSET



SUBROUTINE PERT(QSPEC)
    !--------------------------------------------------------
    !     Calculates the perturbed geometry resulting from
    !     one Cn mapping coefficient being perturbed by user.
    !--------------------------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    DIMENSION QSPEC(ICX)
    !
    COMPLEX QQ(IMX / 4, IMX / 4), DCN(IMX / 4)
    !
    !---- calculate mapping coefficients for initial airfoil shape
    CALL CNCALC(QSPEC, .FALSE.)
    !
    !---- preset rotation offset of airfoil so that initial angle is close
    !-    to the old airfoil's angle
    DX = XCOLD(2) - XCOLD(1)
    DY = YCOLD(2) - YCOLD(1)
    QIM0 = ATAN2(DX, -DY) + 0.5 * PI * (1.0 + AGTE)
    QIMOFF = QIM0 - IMAG(CN(0))
    CN(0) = CN(0) + CMPLX(0.0, QIMOFF)
    !
    WRITE(*, *)
    WRITE(*, *) 'Current mapping coefficients...'
    WRITE(*, *) '      n    Re(Cn)      Im(Cn)'
    !cc   DO M = 1, NC
    DO M = 1, MIN(NC, 32)
        WRITE(*, 1010) M, REAL(CN(M)), IMAG(CN(M))
        1010   FORMAT(4X, I4, 2F12.6)
    ENDDO
    !
    10   WRITE(*, 1050)
    1050 FORMAT(/4X, 'Enter  n, delta(Cnr), delta(Cni):  ', $)
    READ(*, *, ERR = 10) M, DCNR, DCNI
    IF(M.LE.0) THEN
        GO TO 10
    ELSEIF(M.GT.NC) THEN
        WRITE(*, *) 'Max number of modes is', NC
        GO TO 10
    ENDIF
    CN(M) = CN(M) + CMPLX(DCNR, DCNI)
    !
    !---- inverse-transform and calculate geometry
    !cc   CALL CNFILT(FFILT)
    CALL PIQSUM
    CALL ZCCALC(MCT)
    !
    !---- normalize chord and set exact previous alpha
    CALL ZCNORM(MCT)
    !
    !CC---- put back rotation offset so speed routine QCCALC gets the right alpha
    !CC      CN(0) = CN(0) - CMPLX( 0.0 , QIMOFF )

    !---- enforce Lighthill's first constraint
    CN(0) = CMPLX(0.0, IMAG(CN(0)))

    !---- number of free coefficients
    NCN = 1

    !---- Newton iteration loop for modified Cn's
    DO 100 ITERCN = 1, 10

        !------ fix TE gap
        M = 1
        DCN(M) = ZC(1) - ZC(NC) - DZTE
        DO L = 1, NCN
            QQ(M, L) = ZC_CN(1, L) - ZC_CN(NC, L)
        ENDDO
        !
        CALL CGAUSS(IMX / 4, NCN, QQ, DCN, 1)
        !
        DCNMAX = 0.
        DO M = 1, NCN
            CN(M) = CN(M) - DCN(M)
            DCNMAX = MAX(ABS(DCN(M)), DCNMAX)
        ENDDO
        !
        !cc     CALL CNFILT(FFILT)
        CALL PIQSUM
        !
        CALL ZCCALC(MCT)
        CALL ZCNORM(MCT)
        !
        WRITE(*, *) ITERCN, DCNMAX
        IF(DCNMAX.LE.5.0E-5) GO TO 101
    100  CONTINUE
    WRITE(*, *) 'TE gap,chord did not converge'
    101  CONTINUE
    RETURN
END
! PERT



SUBROUTINE CNDUMP(LU)
    !--------------------------------------------------------
    !     Writes out the Fourier coefficients Cn
    !--------------------------------------------------------
    use m_spline
    INCLUDE 'CIRCLE.INC'
    !
    do 700 m = 0, mc
        write(LU, 7000) m, real(cn(m)), imag(cn(m))&
                , real(piq(m + 1)), imag(piq(m + 1))
    700 continue
    !
    do 710 m = mc + 1, nc - 1
        write(LU, 7000) m, 0.0, 0.0&
                , real(piq(m + 1)), imag(piq(m + 1))
    710 continue
    !
    7000 format(1x, i3, 4f11.6)
    !
    RETURN
END


SUBROUTINE GETVOV(KQSP)
    use m_spline
    use i_xfoil
    !LED ENTIRE ROUTINE
    !
    KK = 0
    DO 5 I = 1, IQX
        W1(I) = 0.
        W2(I) = 0.
        W3(I) = 0.
    5 CONTINUE
    !
    LU = 2
    !
    CALL ASKS('Enter V/Vinf vs s data filename^', FNAME)
    OPEN(LU, FILE = FNAME, STATUS = 'OLD', ERR = 98)
    !
    !---- read the Qspec file
    DO 10 K = 1, IQX
        READ(LU, *, END = 11, ERR = 99) W1(K), W2(K)
    10 CONTINUE
    11 KK = K - 1
    CLOSE(LU)
    !
    !---- nondimensionalize S distances
    SSPAN = W1(KK) - W1(1)
    SSTART = W1(1)
    DO 15 K = 1, KK
        W1(K) = 1. - (W1(K) - SSTART) / SSPAN
    15 CONTINUE
    !
    !---- sort input points then, removing identical pairs
    CALL SORT(KK, W1, W2)
    !
    !---- spline input points
    CALL SPLIND(W2, W3, W1, KK, -999.0, -999.0)
    !
    !---- set Qspec array
    DO 20 I = 1, NSP
        SS = SSPEC(I)
        !
        !------ evaluate spline at SSPEC positions
        QSNEW = SEVAL(SS, W2, W3, W1, KK)
        !
        !------ set incompressible speed from new compressible speed
        QSPEC(I, KQSP) = QINCOM(QSNEW, QINF, TKLAM)
        !
    20 CONTINUE
    !
    !---- spline new Qspec array
    CALL SPLQSP(KQSP)
    !
    RETURN
    !
    98 WRITE(*, *) 'GETVOV: File OPEN error.'
    RETURN
    !
    99 WRITE(*, *) 'GETVOV: File READ error.'
    CLOSE(LU)
    RETURN
    !
END
! GETVOV


SUBROUTINE ZLEFIND(ZLE, ZC, WC, NC, PIQ, AGTE)
    use m_spline
    COMPLEX ZLE, ZC(*), PIQ(*)
    DIMENSION WC(*)
    !
    COMPLEX DZDW1, DZDW2, ZTE
    !
    !---- temporary work arrays for splining near leading edge
    PARAMETER (NTX = 33)
    DIMENSION XC(NTX), YC(NTX), XCW(NTX), YCW(NTX)
    !
    DATA  PI /3.1415926535897932384/
    !
    ZTE = 0.5 * (ZC(1) + ZC(NC))
    !
    !---- find point farthest from TE
    DMAX = 0.0
    DO 30 IC = 1, NC
        DIST = ABS(ZC(IC) - ZTE)
        !
        IF(DIST.GT.DMAX) THEN
            DMAX = DIST
            ICLE = IC
        ENDIF
    30 CONTINUE
    !
    !---- set restricted spline limits around leading edge
    IC1 = MAX(ICLE - (NTX - 1) / 2, 1)
    IC2 = MIN(ICLE + (NTX - 1) / 2, NC)
    !
    !---- set up derivatives at spline endpoints
    SINW = 2.0 * SIN(0.5 * WC(IC1))
    SINWE = SINW**(1.0 - AGTE)
    HWC = 0.5 * (WC(IC1) - PI) * (1.0 + AGTE) - 0.5 * PI
    DZDW1 = SINWE * EXP(PIQ(IC1) + CMPLX(0.0, HWC))
    !
    SINW = 2.0 * SIN(0.5 * WC(IC2))
    SINWE = SINW**(1.0 - AGTE)
    HWC = 0.5 * (WC(IC2) - PI) * (1.0 + AGTE) - 0.5 * PI
    DZDW2 = SINWE * EXP(PIQ(IC2) + CMPLX(0.0, HWC))
    !
    !---- fill temporary x,y coordinate arrays
    DO 40 IC = IC1, IC2
        I = IC - IC1 + 1
        XC(I) = REAL(ZC(IC))
        YC(I) = IMAG(ZC(IC))
    40 CONTINUE
    !
    !---- calculate spline near leading edge with derivative end conditions
    NIC = IC2 - IC1 + 1
    CALL SPLIND(XC, XCW, WC(IC1), NIC, REAL(DZDW1), REAL(DZDW2))
    CALL SPLIND(YC, YCW, WC(IC1), NIC, IMAG(DZDW1), IMAG(DZDW2))
    !
    XCTE = 0.5 * REAL(ZC(1) + ZC(NC))
    YCTE = 0.5 * IMAG(ZC(1) + ZC(NC))
    !
    !---- initial guess for leading edge coordinate
    WCLE = WC(ICLE)
    !
    !---- Newton loop for improved leading edge coordinate
    DO 50 ITCLE = 1, 10
        XCLE = SEVAL(WCLE, XC, XCW, WC(IC1), NIC)
        YCLE = SEVAL(WCLE, YC, YCW, WC(IC1), NIC)
        DXDW = DEVAL(WCLE, XC, XCW, WC(IC1), NIC)
        DYDW = DEVAL(WCLE, YC, YCW, WC(IC1), NIC)
        DXDD = D2VAL(WCLE, XC, XCW, WC(IC1), NIC)
        DYDD = D2VAL(WCLE, YC, YCW, WC(IC1), NIC)
        !
        XCHORD = XCLE - XCTE
        YCHORD = YCLE - YCTE
        !
        !------ drive dot product between chord line and LE tangent to zero
        RES = XCHORD * DXDW + YCHORD * DYDW
        RESW = DXDW * DXDW + DYDW * DYDW&
                + XCHORD * DXDD + YCHORD * DYDD
        !
        DWCLE = -RES / RESW
        WCLE = WCLE + DWCLE
        !
        IF(ABS(DWCLE).LT.1.0E-5) GO TO 51
    50 CONTINUE
    WRITE(*, *) 'ZLEFIND: LE location failed.'
    WCLE = WC(ICLE)
    51 CONTINUE
    !
    !---- set final leading edge point complex coordinate
    XCLE = SEVAL(WCLE, XC, XCW, WC(IC1), NIC)
    YCLE = SEVAL(WCLE, YC, YCW, WC(IC1), NIC)
    ZLE = CMPLX(XCLE, YCLE)
    !
    RETURN
END
! ZLEFIND
