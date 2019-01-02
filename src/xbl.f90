!***********************************************************************
!    Module:  xbl.f
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
SUBROUTINE SETBL
    !-------------------------------------------------
    !     Sets up the BL Newton system coefficients
    !     for the current BL variables and the edge
    !     velocities received from SETUP. The local
    !     BL system coefficients are then
    !     incorporated into the global Newton system.
    !-------------------------------------------------
    INCLUDE 'XFOIL.INC'
    INCLUDE 'XBL.INC'
    REAL USAV(IVX, 2)
    REAL U1_M(2 * IVX), U2_M(2 * IVX)
    REAL D1_M(2 * IVX), D2_M(2 * IVX)
    REAL ULE1_M(2 * IVX), ULE2_M(2 * IVX)
    REAL UTE1_M(2 * IVX), UTE2_M(2 * IVX)
    REAL MA_CLMR, MSQ_CLMR, MDI
    !
    !---- set the CL used to define Mach, Reynolds numbers
    IF(LALFA) THEN
        CLMR = CL
    ELSE
        CLMR = CLSPEC
    ENDIF
    !
    !---- set current MINF(CL)
    CALL MRCL(CLMR, MA_CLMR, RE_CLMR)
    MSQ_CLMR = 2.0 * MINF * MA_CLMR
    !
    !---- set compressibility parameter TKLAM and derivative TK_MSQ
    CALL COMSET
    !
    !---- set gas constant (= Cp/Cv)
    GAMBL = GAMMA
    GM1BL = GAMM1
    !
    !---- set parameters for compressibility correction
    QINFBL = QINF
    TKBL = TKLAM
    TKBL_MS = TKL_MSQ
    !
    !---- stagnation density and 1/enthalpy
    RSTBL = (1.0 + 0.5 * GM1BL * MINF**2) ** (1.0 / GM1BL)
    RSTBL_MS = 0.5 * RSTBL / (1.0 + 0.5 * GM1BL * MINF**2)
    !
    HSTINV = GM1BL * (MINF / QINFBL)**2 / (1.0 + 0.5 * GM1BL * MINF**2)
    HSTINV_MS = GM1BL * (1.0 / QINFBL)**2 / (1.0 + 0.5 * GM1BL * MINF**2)&
            - 0.5 * GM1BL * HSTINV / (1.0 + 0.5 * GM1BL * MINF**2)
    !
    !---- set Reynolds number based on freestream density, velocity, viscosity
    HERAT = 1.0 - 0.5 * QINFBL**2 * HSTINV
    HERAT_MS = - 0.5 * QINFBL**2 * HSTINV_MS
    !
    REYBL = REINF * SQRT(HERAT**3) * (1.0 + HVRAT) / (HERAT + HVRAT)
    REYBL_RE = SQRT(HERAT**3) * (1.0 + HVRAT) / (HERAT + HVRAT)
    REYBL_MS = REYBL * (1.5 / HERAT - 1.0 / (HERAT + HVRAT)) * HERAT_MS
    !
    IDAMPV = IDAMP
    !
    !---- save TE thickness
    DWTE = WGAP(1)
    !
    IF(.NOT.LBLINI) THEN
        !----- initialize BL by marching with Ue (fudge at separation)
        WRITE(*, *)
        WRITE(*, *) 'Initializing BL ...'
        CALL MRCHUE
        LBLINI = .TRUE.
    ENDIF
    !
    WRITE(*, *)
    !
    !---- march BL with current Ue and Ds to establish transition
    CALL MRCHDU
    !
    DO 5 IS = 1, 2
        DO 6 IBL = 2, NBL(IS)
            USAV(IBL, IS) = UEDG(IBL, IS)
        6   CONTINUE
    5 CONTINUE
    !
    CALL UESET
    !
    DO 7 IS = 1, 2
        DO 8 IBL = 2, NBL(IS)
            TEMP = USAV(IBL, IS)
            USAV(IBL, IS) = UEDG(IBL, IS)
            UEDG(IBL, IS) = TEMP
        8   CONTINUE
    7 CONTINUE
    !
    ILE1 = IPAN(2, 1)
    ILE2 = IPAN(2, 2)
    ITE1 = IPAN(IBLTE(1), 1)
    ITE2 = IPAN(IBLTE(2), 2)
    !
    JVTE1 = ISYS(IBLTE(1), 1)
    JVTE2 = ISYS(IBLTE(2), 2)
    !
    DULE1 = UEDG(2, 1) - USAV(2, 1)
    DULE2 = UEDG(2, 2) - USAV(2, 2)
    !
    !---- set LE and TE Ue sensitivities wrt all m values
    DO 10 JS = 1, 2
        DO 110 JBL = 2, NBL(JS)
            J = IPAN(JBL, JS)
            JV = ISYS(JBL, JS)
            ULE1_M(JV) = -VTI(2, 1) * VTI(JBL, JS) * DIJ(ILE1, J)
            ULE2_M(JV) = -VTI(2, 2) * VTI(JBL, JS) * DIJ(ILE2, J)
            UTE1_M(JV) = -VTI(IBLTE(1), 1) * VTI(JBL, JS) * DIJ(ITE1, J)
            UTE2_M(JV) = -VTI(IBLTE(2), 2) * VTI(JBL, JS) * DIJ(ITE2, J)
        110   CONTINUE
    10 CONTINUE
    !
    ULE1_A = UINV_A(2, 1)
    ULE2_A = UINV_A(2, 2)
    !
    TINDEX(1) = 0.0
    TINDEX(2) = 0.0
    !
    !**** Go over each boundary layer/wake
    DO 2000 IS = 1, 2
        !
        !---- there is no station "1" at similarity, so zero everything out
        DO 20 JS = 1, 2
            DO 210 JBL = 2, NBL(JS)
                JV = ISYS(JBL, JS)
                U1_M(JV) = 0.
                D1_M(JV) = 0.
            210   CONTINUE
        20 CONTINUE
        U1_A = 0.
        D1_A = 0.
        !
        DUE1 = 0.
        DDS1 = 0.
        !
        !---- similarity station pressure gradient parameter  x/u du/dx
        IBL = 2
        BULE = 1.0
        !
        AMCRIT = ACRIT(IS)
        !
        !---- set forced transition arc length position
        CALL XIFSET(IS)
        !
        TRAN = .FALSE.
        TURB = .FALSE.
        !
        !**** Sweep downstream setting up BL equation linearizations
        DO 1000 IBL = 2, NBL(IS)
            !
            IV = ISYS(IBL, IS)
            !
            SIMI = IBL.EQ.2
            WAKE = IBL.GT.IBLTE(IS)
            TRAN = IBL.EQ.ITRAN(IS)
            TURB = IBL.GT.ITRAN(IS)
            !
            I = IPAN(IBL, IS)
            !
            !---- set primary variables for current station
            XSI = XSSI(IBL, IS)
            IF(IBL.LT.ITRAN(IS)) AMI = CTAU(IBL, IS)
            IF(IBL.GE.ITRAN(IS)) CTI = CTAU(IBL, IS)
            UEI = UEDG(IBL, IS)
            THI = THET(IBL, IS)
            MDI = MASS(IBL, IS)
            !
            DSI = MDI / UEI
            !
            IF(WAKE) THEN
                IW = IBL - IBLTE(IS)
                DSWAKI = WGAP(IW)
            ELSE
                DSWAKI = 0.
            ENDIF
            !
            !---- set derivatives of DSI (= D2)
            D2_M2 = 1.0 / UEI
            D2_U2 = -DSI / UEI
            !
            DO 30 JS = 1, 2
                DO 310 JBL = 2, NBL(JS)
                    J = IPAN(JBL, JS)
                    JV = ISYS(JBL, JS)
                    U2_M(JV) = -VTI(IBL, IS) * VTI(JBL, JS) * DIJ(I, J)
                    D2_M(JV) = D2_U2 * U2_M(JV)
                310   CONTINUE
            30 CONTINUE
            D2_M(IV) = D2_M(IV) + D2_M2
            !
            U2_A = UINV_A(IBL, IS)
            D2_A = D2_U2 * U2_A
            !
            !---- "forced" changes due to mismatch between UEDG and USAV=UINV+dij*MASS
            DUE2 = UEDG(IBL, IS) - USAV(IBL, IS)
            DDS2 = D2_U2 * DUE2
            !
            CALL BLPRV(XSI, AMI, CTI, THI, DSI, DSWAKI, UEI)
            CALL BLKIN
            !
            !---- check for transition and set TRAN, XT, etc. if found
            IF(TRAN) THEN
                CALL TRCHEK
                AMI = AMPL2
            ENDIF
            IF(IBL.EQ.ITRAN(IS) .AND. .NOT.TRAN) THEN
                WRITE(*, *) 'SETBL: Xtr???  n1 n2: ', AMPL1, AMPL2
            ENDIF
            !
            !---- assemble 10x4 linearized system for dCtau, dTh, dDs, dUe, dXi
            !     at the previous "1" station and the current "2" station
            !
            IF(IBL.EQ.IBLTE(IS) + 1) THEN
                !
                !----- define quantities at start of wake, adding TE base thickness to Dstar
                TTE = THET(IBLTE(1), 1) + THET(IBLTE(2), 2)
                DTE = DSTR(IBLTE(1), 1) + DSTR(IBLTE(2), 2) + ANTE
                CTE = (CTAU(IBLTE(1), 1) * THET(IBLTE(1), 1)&
                        + CTAU(IBLTE(2), 2) * THET(IBLTE(2), 2)) / TTE
                CALL TESYS(CTE, TTE, DTE)
                !
                TTE_TTE1 = 1.0
                TTE_TTE2 = 1.0
                DTE_MTE1 = 1.0 / UEDG(IBLTE(1), 1)
                DTE_UTE1 = -DSTR(IBLTE(1), 1) / UEDG(IBLTE(1), 1)
                DTE_MTE2 = 1.0 / UEDG(IBLTE(2), 2)
                DTE_UTE2 = -DSTR(IBLTE(2), 2) / UEDG(IBLTE(2), 2)
                CTE_CTE1 = THET(IBLTE(1), 1) / TTE
                CTE_CTE2 = THET(IBLTE(2), 2) / TTE
                CTE_TTE1 = (CTAU(IBLTE(1), 1) - CTE) / TTE
                CTE_TTE2 = (CTAU(IBLTE(2), 2) - CTE) / TTE
                !
                !----- re-define D1 sensitivities wrt m since D1 depends on both TE Ds values
                DO 35 JS = 1, 2
                    DO 350 JBL = 2, NBL(JS)
                        J = IPAN(JBL, JS)
                        JV = ISYS(JBL, JS)
                        D1_M(JV) = DTE_UTE1 * UTE1_M(JV) + DTE_UTE2 * UTE2_M(JV)
                    350    CONTINUE
                35  CONTINUE
                D1_M(JVTE1) = D1_M(JVTE1) + DTE_MTE1
                D1_M(JVTE2) = D1_M(JVTE2) + DTE_MTE2
                !
                !----- "forced" changes from  UEDG --- USAV=UINV+dij*MASS  mismatch
                DUE1 = 0.
                DDS1 = DTE_UTE1 * (UEDG(IBLTE(1), 1) - USAV(IBLTE(1), 1))&
                        + DTE_UTE2 * (UEDG(IBLTE(2), 2) - USAV(IBLTE(2), 2))
                !
            ELSE
                !
                CALL BLSYS
                !
            ENDIF
            !
            !
            !---- Save wall shear and equil. max shear coefficient for plotting output
            TAU(IBL, IS) = 0.5 * R2 * U2 * U2 * CF2
            DIS(IBL, IS) = R2 * U2 * U2 * U2 * DI2 * HS2 * 0.5
            CTQ(IBL, IS) = CQ2
            DELT(IBL, IS) = DE2
            USLP(IBL, IS) = 1.60 / (1.0 + US2)
            !
            !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
            !      IF(WAKE) THEN
            !        ALD = DLCON
            !      ELSE
            !       ALD = 1.0
            !      ENDIF
            !C
            !      IF(TURB .AND. .NOT.WAKE) THEN
            !        GCC = GCCON
            !        HKC     = HK2 - 1.0 - GCC/RT2
            !        IF(HKC .LT. 0.01) THEN
            !         HKC = 0.01
            !        ENDIF
            !       ELSE
            !        HKC = HK2 - 1.0
            !       ENDIF
            !C
            !       HR = HKC     / (GACON*ALD*HK2)
            !       UQ = (0.5*CF2 - HR**2) / (GBCON*D2)
            !C
            !       IF(TURB) THEN
            !        IBLP = MIN(IBL+1,NBL(IS))
            !        IBLM = MAX(IBL-1,2      )
            !        DXSSI = XSSI(IBLP,IS) - XSSI(IBLM,IS)
            !        IF(DXXSI.EQ.0.0) DXSSI = 1.0
            !        GUXD(IBL,IS) = -LOG(UEDG(IBLP,IS)/UEDG(IBLM,IS)) / DXSSI
            !        GUXQ(IBL,IS) = -UQ
            !       ELSE
            !        GUXD(IBL,IS) = 0.0
            !        GUXQ(IBL,IS) = 0.0
            !       ENDIF
            !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
            !
            !---- set XI sensitivities wrt LE Ue changes
            IF(IS.EQ.1) THEN
                XI_ULE1 = SST_GO
                XI_ULE2 = -SST_GP
            ELSE
                XI_ULE1 = -SST_GO
                XI_ULE2 = SST_GP
            ENDIF
            !
            !---- stuff BL system coefficients into main Jacobian matrix
            !
            DO 40 JV = 1, NSYS
                VM(1, JV, IV) = VS1(1, 3) * D1_M(JV) + VS1(1, 4) * U1_M(JV)&
                        + VS2(1, 3) * D2_M(JV) + VS2(1, 4) * U2_M(JV)&
                        + (VS1(1, 5) + VS2(1, 5) + VSX(1))&
                                * (XI_ULE1 * ULE1_M(JV) + XI_ULE2 * ULE2_M(JV))
            40 CONTINUE
            !
            VB(1, 1, IV) = VS1(1, 1)
            VB(1, 2, IV) = VS1(1, 2)
            !
            VA(1, 1, IV) = VS2(1, 1)
            VA(1, 2, IV) = VS2(1, 2)
            !
            IF(LALFA) THEN
                VDEL(1, 2, IV) = VSR(1) * RE_CLMR + VSM(1) * MSQ_CLMR
            ELSE
                VDEL(1, 2, IV) = &
                        (VS1(1, 4) * U1_A + VS1(1, 3) * D1_A)&
                                + (VS2(1, 4) * U2_A + VS2(1, 3) * D2_A)&
                                + (VS1(1, 5) + VS2(1, 5) + VSX(1))&
                                * (XI_ULE1 * ULE1_A + XI_ULE2 * ULE2_A)
            ENDIF
            !
            VDEL(1, 1, IV) = VSREZ(1)&
                    + (VS1(1, 4) * DUE1 + VS1(1, 3) * DDS1)&
                    + (VS2(1, 4) * DUE2 + VS2(1, 3) * DDS2)&
                    + (VS1(1, 5) + VS2(1, 5) + VSX(1))&
                            * (XI_ULE1 * DULE1 + XI_ULE2 * DULE2)
            !
            !
            DO 50 JV = 1, NSYS
                VM(2, JV, IV) = VS1(2, 3) * D1_M(JV) + VS1(2, 4) * U1_M(JV)&
                        + VS2(2, 3) * D2_M(JV) + VS2(2, 4) * U2_M(JV)&
                        + (VS1(2, 5) + VS2(2, 5) + VSX(2))&
                                * (XI_ULE1 * ULE1_M(JV) + XI_ULE2 * ULE2_M(JV))
            50 CONTINUE
            !
            VB(2, 1, IV) = VS1(2, 1)
            VB(2, 2, IV) = VS1(2, 2)
            !
            VA(2, 1, IV) = VS2(2, 1)
            VA(2, 2, IV) = VS2(2, 2)
            !
            IF(LALFA) THEN
                VDEL(2, 2, IV) = VSR(2) * RE_CLMR + VSM(2) * MSQ_CLMR
            ELSE
                VDEL(2, 2, IV) = &
                        (VS1(2, 4) * U1_A + VS1(2, 3) * D1_A)&
                                + (VS2(2, 4) * U2_A + VS2(2, 3) * D2_A)&
                                + (VS1(2, 5) + VS2(2, 5) + VSX(2))&
                                * (XI_ULE1 * ULE1_A + XI_ULE2 * ULE2_A)
            ENDIF
            !
            VDEL(2, 1, IV) = VSREZ(2)&
                    + (VS1(2, 4) * DUE1 + VS1(2, 3) * DDS1)&
                    + (VS2(2, 4) * DUE2 + VS2(2, 3) * DDS2)&
                    + (VS1(2, 5) + VS2(2, 5) + VSX(2))&
                            * (XI_ULE1 * DULE1 + XI_ULE2 * DULE2)
            !
            !
            DO 60 JV = 1, NSYS
                VM(3, JV, IV) = VS1(3, 3) * D1_M(JV) + VS1(3, 4) * U1_M(JV)&
                        + VS2(3, 3) * D2_M(JV) + VS2(3, 4) * U2_M(JV)&
                        + (VS1(3, 5) + VS2(3, 5) + VSX(3))&
                                * (XI_ULE1 * ULE1_M(JV) + XI_ULE2 * ULE2_M(JV))
            60 CONTINUE
            !
            VB(3, 1, IV) = VS1(3, 1)
            VB(3, 2, IV) = VS1(3, 2)
            !
            VA(3, 1, IV) = VS2(3, 1)
            VA(3, 2, IV) = VS2(3, 2)
            !
            IF(LALFA) THEN
                VDEL(3, 2, IV) = VSR(3) * RE_CLMR + VSM(3) * MSQ_CLMR
            ELSE
                VDEL(3, 2, IV) = &
                        (VS1(3, 4) * U1_A + VS1(3, 3) * D1_A)&
                                + (VS2(3, 4) * U2_A + VS2(3, 3) * D2_A)&
                                + (VS1(3, 5) + VS2(3, 5) + VSX(3))&
                                * (XI_ULE1 * ULE1_A + XI_ULE2 * ULE2_A)
            ENDIF
            !
            VDEL(3, 1, IV) = VSREZ(3)&
                    + (VS1(3, 4) * DUE1 + VS1(3, 3) * DDS1)&
                    + (VS2(3, 4) * DUE2 + VS2(3, 3) * DDS2)&
                    + (VS1(3, 5) + VS2(3, 5) + VSX(3))&
                            * (XI_ULE1 * DULE1 + XI_ULE2 * DULE2)
            !
            !
            IF(IBL.EQ.IBLTE(IS) + 1) THEN
                !
                !----- redefine coefficients for TTE, DTE, etc
                VZ(1, 1) = VS1(1, 1) * CTE_CTE1
                VZ(1, 2) = VS1(1, 1) * CTE_TTE1 + VS1(1, 2) * TTE_TTE1
                VB(1, 1, IV) = VS1(1, 1) * CTE_CTE2
                VB(1, 2, IV) = VS1(1, 1) * CTE_TTE2 + VS1(1, 2) * TTE_TTE2
                !
                VZ(2, 1) = VS1(2, 1) * CTE_CTE1
                VZ(2, 2) = VS1(2, 1) * CTE_TTE1 + VS1(2, 2) * TTE_TTE1
                VB(2, 1, IV) = VS1(2, 1) * CTE_CTE2
                VB(2, 2, IV) = VS1(2, 1) * CTE_TTE2 + VS1(2, 2) * TTE_TTE2
                !
                VZ(3, 1) = VS1(3, 1) * CTE_CTE1
                VZ(3, 2) = VS1(3, 1) * CTE_TTE1 + VS1(3, 2) * TTE_TTE1
                VB(3, 1, IV) = VS1(3, 1) * CTE_CTE2
                VB(3, 2, IV) = VS1(3, 1) * CTE_TTE2 + VS1(3, 2) * TTE_TTE2
                !
            ENDIF
            !
            !---- turbulent intervals will follow if currently at transition interval
            IF(TRAN) THEN
                TURB = .TRUE.
                !
                !------ save transition location
                ITRAN(IS) = IBL
                TFORCE(IS) = TRFORC
                XSSITR(IS) = XT
                !
                !------ interpolate airfoil geometry to find transition x/c
                !-      (for user output)
                IF(IS.EQ.1) THEN
                    STR = SST - XT
                ELSE
                    STR = SST + XT
                ENDIF
                CHX = XTE - XLE
                CHY = YTE - YLE
                CHSQ = CHX**2 + CHY**2
                XTR = SEVAL(STR, X, XP, S, N)
                YTR = SEVAL(STR, Y, YP, S, N)
                XOCTR(IS) = ((XTR - XLE) * CHX + (YTR - YLE) * CHY) / CHSQ
                YOCTR(IS) = ((YTR - YLE) * CHX - (XTR - XLE) * CHY) / CHSQ
            ENDIF
            !
            TRAN = .FALSE.
            !
            IF(IBL.EQ.IBLTE(IS)) THEN
                !----- set "2" variables at TE to wake correlations for next station
                !
                TURB = .TRUE.
                WAKE = .TRUE.
                CALL BLVAR(3)
                CALL BLMID(3)
            ENDIF
            !
            DO 80 JS = 1, 2
                DO 810 JBL = 2, NBL(JS)
                    JV = ISYS(JBL, JS)
                    U1_M(JV) = U2_M(JV)
                    D1_M(JV) = D2_M(JV)
                810   CONTINUE
            80 CONTINUE
            !
            U1_A = U2_A
            D1_A = D2_A
            !
            DUE1 = DUE2
            DDS1 = DDS2
            !
            IF(IBL .EQ. ITRAN(IS) .AND. X2 .GT. X1) THEN
                IF(IS.EQ.1) THEN
                    TINDEX(IS) = FLOAT(IST - ITRAN(IS) + 3) - (XT - X1) / (X2 - X1)
                ELSE
                    TINDEX(IS) = FLOAT(IST + ITRAN(IS) - 2) + (XT - X1) / (X2 - X1)
                ENDIF
            ENDIF
            !
            !---- set BL variables for next station
            DO 190 ICOM = 1, NCOM
                COM1(ICOM) = COM2(ICOM)
            190 CONTINUE
            !
            !---- next streamwise station
        1000 CONTINUE
        !
        IF(TFORCE(IS)) THEN
            WRITE(*, 9100) IS, XOCTR(IS), ITRAN(IS)
            9100  FORMAT(1X, 'Side', I2, ' forced transition at x/c = ', F7.4, I5)
        ELSE
            WRITE(*, 9200) IS, XOCTR(IS), ITRAN(IS)
            9200  FORMAT(1X, 'Side', I2, '  free  transition at x/c = ', F7.4, I5)
        ENDIF
        !
        !---- next airfoil side
    2000 CONTINUE
    !
    RETURN
END


SUBROUTINE IBLSYS
    !---------------------------------------------
    !     Sets the BL Newton system line number
    !     corresponding to each BL station.
    !---------------------------------------------
    INCLUDE 'XFOIL.INC'
    INCLUDE 'XBL.INC'
    IV = 0
    DO 10 IS = 1, 2
        DO 110 IBL = 2, NBL(IS)
            IV = IV + 1
            ISYS(IBL, IS) = IV
        110   CONTINUE
    10 CONTINUE
    !
    NSYS = IV
    IF(NSYS.GT.2 * IVX) STOP '*** IBLSYS: BL system array overflow. ***'
    !
    RETURN
END


SUBROUTINE MRCHUE
    !----------------------------------------------------
    !     Marches the BLs and wake in direct mode using
    !     the UEDG array. If separation is encountered,
    !     a plausible value of Hk extrapolated from
    !     upstream is prescribed instead.  Continuous
    !     checking of transition onset is performed.
    !----------------------------------------------------
    INCLUDE 'XFOIL.INC'
    INCLUDE 'XBL.INC'
    LOGICAL DIRECT
    REAL MSQ
    !
    !---- shape parameters for separation criteria
    HLMAX = 3.8
    HTMAX = 2.5
    !
    DO 2000 IS = 1, 2
        !
        WRITE(*, *) '   side ', IS, ' ...'
        !
        AMCRIT = ACRIT(IS)
        !
        !---- set forced transition arc length position
        CALL XIFSET(IS)
        !
        !---- initialize similarity station with Thwaites' formula
        IBL = 2
        XSI = XSSI(IBL, IS)
        UEI = UEDG(IBL, IS)
        !      BULE = LOG(UEDG(IBL+1,IS)/UEI) / LOG(XSSI(IBL+1,IS)/XSI)
        !      BULE = MAX( -.08 , BULE )
        BULE = 1.0
        UCON = UEI / XSI**BULE
        TSQ = 0.45 / (UCON * (5.0 * BULE + 1.0) * REYBL) * XSI**(1.0 - BULE)
        THI = SQRT(TSQ)
        DSI = 2.2 * THI
        AMI = 0.0
        !
        !---- initialize Ctau for first turbulent station
        CTI = 0.03
        !
        TRAN = .FALSE.
        TURB = .FALSE.
        ITRAN(IS) = IBLTE(IS)
        !
        !---- march downstream
        DO 1000 IBL = 2, NBL(IS)
            IBM = IBL - 1
            !
            IW = IBL - IBLTE(IS)
            !
            SIMI = IBL.EQ.2
            WAKE = IBL.GT.IBLTE(IS)
            !
            !------ prescribed quantities
            XSI = XSSI(IBL, IS)
            UEI = UEDG(IBL, IS)
            !
            IF(WAKE) THEN
                IW = IBL - IBLTE(IS)
                DSWAKI = WGAP(IW)
            ELSE
                DSWAKI = 0.
            ENDIF
            !
            DIRECT = .TRUE.
            !
            !------ Newton iteration loop for current station
            DO 100 ITBL = 1, 25
                !
                !-------- assemble 10x3 linearized system for dCtau, dTh, dDs, dUe, dXi
                !         at the previous "1" station and the current "2" station
                !         (the "1" station coefficients will be ignored)
                !
                !
                CALL BLPRV(XSI, AMI, CTI, THI, DSI, DSWAKI, UEI)
                CALL BLKIN
                !
                !-------- check for transition and set appropriate flags and things
                IF((.NOT.SIMI) .AND. (.NOT.TURB)) THEN
                    CALL TRCHEK
                    AMI = AMPL2
                    !
                    IF(TRAN) THEN
                        ITRAN(IS) = IBL
                        IF(CTI.LE.0.0) THEN
                            CTI = 0.03
                            S2 = CTI
                        ENDIF
                    ELSE
                        ITRAN(IS) = IBL + 2
                    ENDIF
                    !
                    !
                ENDIF
                !
                IF(IBL.EQ.IBLTE(IS) + 1) THEN
                    TTE = THET(IBLTE(1), 1) + THET(IBLTE(2), 2)
                    DTE = DSTR(IBLTE(1), 1) + DSTR(IBLTE(2), 2) + ANTE
                    CTE = (CTAU(IBLTE(1), 1) * THET(IBLTE(1), 1)&
                            + CTAU(IBLTE(2), 2) * THET(IBLTE(2), 2)) / TTE
                    CALL TESYS(CTE, TTE, DTE)
                ELSE
                    CALL BLSYS
                ENDIF
                !

                IF(DIRECT) THEN
                    !
                    !--------- try direct mode (set dUe = 0 in currently empty 4th line)
                    VS2(4, 1) = 0.
                    VS2(4, 2) = 0.
                    VS2(4, 3) = 0.
                    VS2(4, 4) = 1.0
                    VSREZ(4) = 0.
                    !
                    !--------- solve Newton system for current "2" station
                    CALL GAUSS(4, 4, VS2, VSREZ, 1)
                    !
                    !--------- determine max changes and underrelax if necessary
                    DMAX = MAX(ABS(VSREZ(2) / THI), &
                            ABS(VSREZ(3) / DSI))
                    IF(IBL.LT.ITRAN(IS)) DMAX = MAX(DMAX, ABS(VSREZ(1) / 10.0))
                    IF(IBL.GE.ITRAN(IS)) DMAX = MAX(DMAX, ABS(VSREZ(1) / CTI))
                    !
                    RLX = 1.0
                    IF(DMAX.GT.0.3) RLX = 0.3 / DMAX
                    !
                    !--------- see if direct mode is not applicable
                    IF(IBL .NE. IBLTE(IS) + 1) THEN
                        !
                        !---------- calculate resulting kinematic shape parameter Hk
                        MSQ = UEI * UEI * HSTINV / (GM1BL * (1.0 - 0.5 * UEI * UEI * HSTINV))
                        HTEST = (DSI + RLX * VSREZ(3)) / (THI + RLX * VSREZ(2))
                        CALL HKIN(HTEST, MSQ, HKTEST, DUMMY, DUMMY)
                        !
                        !---------- decide whether to do direct or inverse problem based on Hk
                        IF(IBL.LT.ITRAN(IS)) HMAX = HLMAX
                        IF(IBL.GE.ITRAN(IS)) HMAX = HTMAX
                        DIRECT = HKTEST.LT.HMAX
                    ENDIF
                    !
                    IF(DIRECT) THEN
                        !---------- update as usual
                        !cc            IF(IBL.LT.ITRAN(IS)) AMI = AMI + RLX*VSREZ(1)
                        IF(IBL.GE.ITRAN(IS)) CTI = CTI + RLX * VSREZ(1)
                        THI = THI + RLX * VSREZ(2)
                        DSI = DSI + RLX * VSREZ(3)
                    ELSE
                        !---------- set prescribed Hk for inverse calculation at the current station
                        IF(IBL.LT.ITRAN(IS)) THEN
                            !----------- laminar case: relatively slow increase in Hk downstream
                            HTARG = HK1 + 0.03 * (X2 - X1) / T1
                        ELSE IF(IBL.EQ.ITRAN(IS)) THEN
                            !----------- transition interval: weighted laminar and turbulent case
                            HTARG = HK1 + (0.03 * (XT - X1) - 0.15 * (X2 - XT)) / T1
                        ELSE IF(WAKE) THEN
                            !----------- turbulent wake case:
                            !-           asymptotic wake behavior with approximate Backward Euler
                            CONST = 0.03 * (X2 - X1) / T1
                            HK2 = HK1
                            HK2 = HK2 - (HK2 + CONST * (HK2 - 1.0)**3 - HK1)&
                                    / (1.0 + 3.0 * CONST * (HK2 - 1.0)**2)
                            HK2 = HK2 - (HK2 + CONST * (HK2 - 1.0)**3 - HK1)&
                                    / (1.0 + 3.0 * CONST * (HK2 - 1.0)**2)
                            HK2 = HK2 - (HK2 + CONST * (HK2 - 1.0)**3 - HK1)&
                                    / (1.0 + 3.0 * CONST * (HK2 - 1.0)**2)
                            HTARG = HK2
                        ELSE
                            !----------- turbulent case: relatively fast decrease in Hk downstream
                            HTARG = HK1 - 0.15 * (X2 - X1) / T1
                        ENDIF
                        !
                        !---------- limit specified Hk to something reasonable
                        IF(WAKE) THEN
                            HTARG = MAX(HTARG, 1.01)
                        ELSE
                            HTARG = MAX(HTARG, HMAX)
                        ENDIF
                        !
                        WRITE(*, 1300) IBL, HTARG
                        1300       FORMAT(' MRCHUE: Inverse mode at', I4, '     Hk =', F8.3)
                        !
                        !---------- try again with prescribed Hk
                        GO TO 100
                        !
                    ENDIF
                    !
                ELSE
                    !
                    !-------- inverse mode (force Hk to prescribed value HTARG)
                    VS2(4, 1) = 0.
                    VS2(4, 2) = HK2_T2
                    VS2(4, 3) = HK2_D2
                    VS2(4, 4) = HK2_U2
                    VSREZ(4) = HTARG - HK2
                    !
                    CALL GAUSS(4, 4, VS2, VSREZ, 1)
                    !
                    !--------- added Ue clamp   MD  3 Apr 03
                    DMAX = MAX(ABS(VSREZ(2) / THI), &
                            ABS(VSREZ(3) / DSI), &
                            ABS(VSREZ(4) / UEI))
                    IF(IBL.GE.ITRAN(IS)) DMAX = MAX(DMAX, ABS(VSREZ(1) / CTI))
                    !
                    RLX = 1.0
                    IF(DMAX.GT.0.3) RLX = 0.3 / DMAX
                    !
                    !--------- update variables
                    !cc           IF(IBL.LT.ITRAN(IS)) AMI = AMI + RLX*VSREZ(1)
                    IF(IBL.GE.ITRAN(IS)) CTI = CTI + RLX * VSREZ(1)
                    THI = THI + RLX * VSREZ(2)
                    DSI = DSI + RLX * VSREZ(3)
                    UEI = UEI + RLX * VSREZ(4)
                    !
                ENDIF
                !
                !-------- eliminate absurd transients
                IF(IBL.GE.ITRAN(IS)) THEN
                    CTI = MIN(CTI, 0.30)
                    CTI = MAX(CTI, 0.0000001)
                ENDIF
                !
                IF(IBL.LE.IBLTE(IS)) THEN
                    HKLIM = 1.02
                ELSE
                    HKLIM = 1.00005
                ENDIF
                MSQ = UEI * UEI * HSTINV / (GM1BL * (1.0 - 0.5 * UEI * UEI * HSTINV))
                DSW = DSI - DSWAKI
                CALL DSLIM(DSW, THI, UEI, MSQ, HKLIM)
                DSI = DSW + DSWAKI
                !
                IF(DMAX.LE.1.0E-5) GO TO 110
                !
            100   CONTINUE
            WRITE(*, 1350) IBL, IS, DMAX
            1350   FORMAT(' MRCHUE: Convergence failed at', I4, '  side', I2, &
                    '    Res =', E12.4)
            !
            !------ the current unconverged solution might still be reasonable...
            !CC        IF(DMAX .LE. 0.1) GO TO 110
            IF(DMAX .LE. 0.1) GO TO 109
            !
            !------- the current solution is garbage --> extrapolate values instead
            IF(IBL.GT.3) THEN
                IF(IBL.LE.IBLTE(IS)) THEN
                    THI = THET(IBM, IS) * (XSSI(IBL, IS) / XSSI(IBM, IS))**0.5
                    DSI = DSTR(IBM, IS) * (XSSI(IBL, IS) / XSSI(IBM, IS))**0.5
                ELSE IF(IBL.EQ.IBLTE(IS) + 1) THEN
                    CTI = CTE
                    THI = TTE
                    DSI = DTE
                ELSE
                    THI = THET(IBM, IS)
                    RATLEN = (XSSI(IBL, IS) - XSSI(IBM, IS)) / (10.0 * DSTR(IBM, IS))
                    DSI = (DSTR(IBM, IS) + THI * RATLEN) / (1.0 + RATLEN)
                ENDIF
                IF(IBL.EQ.ITRAN(IS)) CTI = 0.05
                IF(IBL.GT.ITRAN(IS)) CTI = CTAU(IBM, IS)
                !
                UEI = UEDG(IBL, IS)
                IF(IBL.GT.2 .AND. IBL.LT.NBL(IS))&
                        UEI = 0.5 * (UEDG(IBL - 1, IS) + UEDG(IBL + 1, IS))
            ENDIF
            !
            109     CALL BLPRV(XSI, AMI, CTI, THI, DSI, DSWAKI, UEI)
            CALL BLKIN
            !
            !------- check for transition and set appropriate flags and things
            IF((.NOT.SIMI) .AND. (.NOT.TURB)) THEN
                CALL TRCHEK
                AMI = AMPL2
                IF(TRAN) ITRAN(IS) = IBL
                IF(.NOT.TRAN) ITRAN(IS) = IBL + 2
            ENDIF
            !
            !------- set all other extrapolated values for current station
            IF(IBL.LT.ITRAN(IS)) CALL BLVAR(1)
            IF(IBL.GE.ITRAN(IS)) CALL BLVAR(2)
            IF(WAKE) CALL BLVAR(3)
            !
            IF(IBL.LT.ITRAN(IS)) CALL BLMID(1)
            IF(IBL.GE.ITRAN(IS)) CALL BLMID(2)
            IF(WAKE) CALL BLMID(3)
            !
            !------ pick up here after the Newton iterations
            110   CONTINUE
            !
            !------ store primary variables
            IF(IBL.LT.ITRAN(IS)) CTAU(IBL, IS) = AMI
            IF(IBL.GE.ITRAN(IS)) CTAU(IBL, IS) = CTI
            THET(IBL, IS) = THI
            DSTR(IBL, IS) = DSI
            UEDG(IBL, IS) = UEI
            MASS(IBL, IS) = DSI * UEI
            TAU(IBL, IS) = 0.5 * R2 * U2 * U2 * CF2
            DIS(IBL, IS) = R2 * U2 * U2 * U2 * DI2 * HS2 * 0.5
            CTQ(IBL, IS) = CQ2
            DELT(IBL, IS) = DE2
            TSTR(IBL, IS) = HS2 * T2
            !
            !------ set "1" variables to "2" variables for next streamwise station
            CALL BLPRV(XSI, AMI, CTI, THI, DSI, DSWAKI, UEI)
            CALL BLKIN
            DO 310 ICOM = 1, NCOM
                COM1(ICOM) = COM2(ICOM)
            310   CONTINUE
            !
            !------ turbulent intervals will follow transition interval or TE
            IF(TRAN .OR. IBL.EQ.IBLTE(IS)) THEN
                TURB = .TRUE.
                !
                !------- save transition location
                TFORCE(IS) = TRFORC
                XSSITR(IS) = XT
            ENDIF
            !
            TRAN = .FALSE.
            !
            IF(IBL.EQ.IBLTE(IS)) THEN
                THI = THET(IBLTE(1), 1) + THET(IBLTE(2), 2)
                DSI = DSTR(IBLTE(1), 1) + DSTR(IBLTE(2), 2) + ANTE
            ENDIF
            !
        1000 CONTINUE
    2000 CONTINUE
    !
    RETURN
END


SUBROUTINE MRCHDU
    !----------------------------------------------------
    !     Marches the BLs and wake in mixed mode using
    !     the current Ue and Hk.  The calculated Ue
    !     and Hk lie along a line quasi-normal to the
    !     natural Ue-Hk characteristic line of the
    !     current BL so that the Goldstein or Levy-Lees
    !     singularity is never encountered.  Continuous
    !     checking of transition onset is performed.
    !----------------------------------------------------
    INCLUDE 'XFOIL.INC'
    INCLUDE 'XBL.INC'
    REAL VTMP(4, 5), VZTMP(4)
    REAL MSQ
    !cc   REAL MDI
    !
    DATA DEPS / 5.0E-6 /
    !
    !---- constant controlling how far Hk is allowed to deviate
    !-    from the specified value.
    SENSWT = 1000.0
    !
    DO 2000 IS = 1, 2
        !
        AMCRIT = ACRIT(IS)
        !
        !---- set forced transition arc length position
        CALL XIFSET(IS)
        !
        !---- set leading edge pressure gradient parameter  x/u du/dx
        IBL = 2
        XSI = XSSI(IBL, IS)
        UEI = UEDG(IBL, IS)
        !CC      BULE = LOG(UEDG(IBL+1,IS)/UEI) / LOG(XSSI(IBL+1,IS)/XSI)
        !CC      BULE = MAX( -.08 , BULE )
        BULE = 1.0
        !
        !---- old transition station
        ITROLD = ITRAN(IS)
        !
        TRAN = .FALSE.
        TURB = .FALSE.
        ITRAN(IS) = IBLTE(IS)
        !
        !---- march downstream
        DO 1000 IBL = 2, NBL(IS)
            IBM = IBL - 1
            !
            SIMI = IBL.EQ.2
            WAKE = IBL.GT.IBLTE(IS)
            !
            !------ initialize current station to existing variables
            XSI = XSSI(IBL, IS)
            UEI = UEDG(IBL, IS)
            THI = THET(IBL, IS)
            DSI = DSTR(IBL, IS)

            !CC        MDI = MASS(IBL,IS)
            !
            !------ fixed BUG   MD 7 June 99
            IF(IBL.LT.ITROLD) THEN
                AMI = CTAU(IBL, IS)
                CTI = 0.03
            ELSE
                CTI = CTAU(IBL, IS)
                IF(CTI.LE.0.0) CTI = 0.03
            ENDIF
            !
            !CC        DSI = MDI/UEI
            !
            IF(WAKE) THEN
                IW = IBL - IBLTE(IS)
                DSWAKI = WGAP(IW)
            ELSE
                DSWAKI = 0.
            ENDIF
            !
            IF(IBL.LE.IBLTE(IS)) DSI = MAX(DSI - DSWAKI, 1.02000 * THI) + DSWAKI
            IF(IBL.GT.IBLTE(IS)) DSI = MAX(DSI - DSWAKI, 1.00005 * THI) + DSWAKI
            !
            !------ Newton iteration loop for current station
            DO 100 ITBL = 1, 25
                !
                !-------- assemble 10x3 linearized system for dCtau, dTh, dDs, dUe, dXi
                !         at the previous "1" station and the current "2" station
                !         (the "1" station coefficients will be ignored)
                !
                CALL BLPRV(XSI, AMI, CTI, THI, DSI, DSWAKI, UEI)
                CALL BLKIN
                !
                !-------- check for transition and set appropriate flags and things
                IF((.NOT.SIMI) .AND. (.NOT.TURB)) THEN
                    CALL TRCHEK
                    AMI = AMPL2
                    IF(TRAN) ITRAN(IS) = IBL
                    IF(.NOT.TRAN) ITRAN(IS) = IBL + 2
                ENDIF
                !
                IF(IBL.EQ.IBLTE(IS) + 1) THEN
                    TTE = THET(IBLTE(1), 1) + THET(IBLTE(2), 2)
                    DTE = DSTR(IBLTE(1), 1) + DSTR(IBLTE(2), 2) + ANTE
                    CTE = (CTAU(IBLTE(1), 1) * THET(IBLTE(1), 1)&
                            + CTAU(IBLTE(2), 2) * THET(IBLTE(2), 2)) / TTE
                    CALL TESYS(CTE, TTE, DTE)
                ELSE
                    CALL BLSYS
                ENDIF
                !
                !-------- set stuff at first iteration...
                IF(ITBL.EQ.1) THEN
                    !
                    !--------- set "baseline" Ue and Hk for forming  Ue(Hk)  relation
                    UEREF = U2
                    HKREF = HK2
                    !
                    !--------- if current point IBL was turbulent and is now laminar, then...
                    IF(IBL.LT.ITRAN(IS) .AND. IBL.GE.ITROLD) THEN
                        !---------- extrapolate baseline Hk
                        UEM = UEDG(IBL - 1, IS)
                        DSM = DSTR(IBL - 1, IS)
                        THM = THET(IBL - 1, IS)
                        MSQ = UEM * UEM * HSTINV / (GM1BL * (1.0 - 0.5 * UEM * UEM * HSTINV))
                        CALL HKIN(DSM / THM, MSQ, HKREF, DUMMY, DUMMY)
                    ENDIF
                    !
                    !--------- if current point IBL was laminar, then...
                    IF(IBL.LT.ITROLD) THEN
                        !---------- reinitialize or extrapolate Ctau if it's now turbulent
                        IF(TRAN) CTAU(IBL, IS) = 0.03
                        IF(TURB) CTAU(IBL, IS) = CTAU(IBL - 1, IS)
                        IF(TRAN .OR. TURB) THEN
                            CTI = CTAU(IBL, IS)
                            S2 = CTI
                        ENDIF
                    ENDIF
                    !
                ENDIF
                !
                !
                IF(SIMI .OR. IBL.EQ.IBLTE(IS) + 1) THEN
                    !
                    !--------- for similarity station or first wake point, prescribe Ue
                    VS2(4, 1) = 0.
                    VS2(4, 2) = 0.
                    VS2(4, 3) = 0.
                    VS2(4, 4) = U2_UEI
                    VSREZ(4) = UEREF - U2
                    !
                ELSE
                    !
                    !********* calculate Ue-Hk characteristic slope
                    !
                    DO 20 K = 1, 4
                        VZTMP(K) = VSREZ(K)
                        DO 201 L = 1, 5
                            VTMP(K, L) = VS2(K, L)
                        201        CONTINUE
                    20      CONTINUE
                    !
                    !--------- set unit dHk
                    VTMP(4, 1) = 0.
                    VTMP(4, 2) = HK2_T2
                    VTMP(4, 3) = HK2_D2
                    VTMP(4, 4) = HK2_U2 * U2_UEI
                    VZTMP(4) = 1.0
                    !
                    !--------- calculate dUe response
                    CALL GAUSS(4, 4, VTMP, VZTMP, 1)
                    !
                    !--------- set  SENSWT * (normalized dUe/dHk)
                    SENNEW = SENSWT * VZTMP(4) * HKREF / UEREF
                    IF(ITBL.LE.5) THEN
                        SENS = SENNEW
                    ELSE IF(ITBL.LE.15) THEN
                        SENS = 0.5 * (SENS + SENNEW)
                    ENDIF
                    !
                    !--------- set prescribed Ue-Hk combination
                    VS2(4, 1) = 0.
                    VS2(4, 2) = HK2_T2 * HKREF
                    VS2(4, 3) = HK2_D2 * HKREF
                    VS2(4, 4) = (HK2_U2 * HKREF + SENS / UEREF) * U2_UEI
                    VSREZ(4) = -(HKREF**2) * (HK2 / HKREF - 1.0)&
                            - SENS * (U2 / UEREF - 1.0)
                    !
                ENDIF
                !
                !-------- solve Newton system for current "2" station
                CALL GAUSS(4, 4, VS2, VSREZ, 1)
                !
                !-------- determine max changes and underrelax if necessary
                !-------- (added Ue clamp   MD  3 Apr 03)
                DMAX = MAX(ABS(VSREZ(2) / THI), &
                        ABS(VSREZ(3) / DSI), &
                        ABS(VSREZ(4) / UEI))
                IF(IBL.GE.ITRAN(IS)) DMAX = MAX(DMAX, ABS(VSREZ(1) / (10.0 * CTI)))
                !
                RLX = 1.0
                IF(DMAX.GT.0.3) RLX = 0.3 / DMAX
                !
                !-------- update as usual
                IF(IBL.LT.ITRAN(IS)) AMI = AMI + RLX * VSREZ(1)
                IF(IBL.GE.ITRAN(IS)) CTI = CTI + RLX * VSREZ(1)
                THI = THI + RLX * VSREZ(2)
                DSI = DSI + RLX * VSREZ(3)
                UEI = UEI + RLX * VSREZ(4)
                !
                !-------- eliminate absurd transients
                IF(IBL.GE.ITRAN(IS)) THEN
                    CTI = MIN(CTI, 0.30)
                    CTI = MAX(CTI, 0.0000001)
                ENDIF
                !
                IF(IBL.LE.IBLTE(IS)) THEN
                    HKLIM = 1.02
                ELSE
                    HKLIM = 1.00005
                ENDIF
                MSQ = UEI * UEI * HSTINV / (GM1BL * (1.0 - 0.5 * UEI * UEI * HSTINV))
                DSW = DSI - DSWAKI
                CALL DSLIM(DSW, THI, UEI, MSQ, HKLIM)
                DSI = DSW + DSWAKI
                !
                IF(DMAX.LE.DEPS) GO TO 110
                !
            100   CONTINUE
            !
            WRITE(*, 1350) IBL, IS, DMAX
            1350   FORMAT(' MRCHDU: Convergence failed at', I4, '  side', I2, &
                    '    Res =', E12.4)
            !
            !------ the current unconverged solution might still be reasonable...
            !CC        IF(DMAX .LE. 0.1) GO TO 110
            IF(DMAX .LE. 0.1) GO TO 109
            !
            !------- the current solution is garbage --> extrapolate values instead
            IF(IBL.GT.3) THEN
                IF(IBL.LE.IBLTE(IS)) THEN
                    THI = THET(IBM, IS) * (XSSI(IBL, IS) / XSSI(IBM, IS))**0.5
                    DSI = DSTR(IBM, IS) * (XSSI(IBL, IS) / XSSI(IBM, IS))**0.5
                    UEI = UEDG(IBM, IS)
                ELSE IF(IBL.EQ.IBLTE(IS) + 1) THEN
                    CTI = CTE
                    THI = TTE
                    DSI = DTE
                    UEI = UEDG(IBM, IS)
                ELSE
                    THI = THET(IBM, IS)
                    RATLEN = (XSSI(IBL, IS) - XSSI(IBM, IS)) / (10.0 * DSTR(IBM, IS))
                    DSI = (DSTR(IBM, IS) + THI * RATLEN) / (1.0 + RATLEN)
                    UEI = UEDG(IBM, IS)
                ENDIF
                IF(IBL.EQ.ITRAN(IS)) CTI = 0.05
                IF(IBL.GT.ITRAN(IS)) CTI = CTAU(IBM, IS)
            ENDIF
            !
            109     CALL BLPRV(XSI, AMI, CTI, THI, DSI, DSWAKI, UEI)
            CALL BLKIN
            !
            !------- check for transition and set appropriate flags and things
            IF((.NOT.SIMI) .AND. (.NOT.TURB)) THEN
                CALL TRCHEK
                AMI = AMPL2
                IF(TRAN) ITRAN(IS) = IBL
                IF(.NOT.TRAN) ITRAN(IS) = IBL + 2
            ENDIF
            !
            !------- set all other extrapolated values for current station
            IF(IBL.LT.ITRAN(IS)) CALL BLVAR(1)
            IF(IBL.GE.ITRAN(IS)) CALL BLVAR(2)
            IF(WAKE) CALL BLVAR(3)
            !
            IF(IBL.LT.ITRAN(IS)) CALL BLMID(1)
            IF(IBL.GE.ITRAN(IS)) CALL BLMID(2)
            IF(WAKE) CALL BLMID(3)
            !
            !------ pick up here after the Newton iterations
            110   CONTINUE
            !
            SENS = SENNEW
            !
            !------ store primary variables
            IF(IBL.LT.ITRAN(IS)) CTAU(IBL, IS) = AMI
            IF(IBL.GE.ITRAN(IS)) CTAU(IBL, IS) = CTI
            THET(IBL, IS) = THI
            DSTR(IBL, IS) = DSI
            UEDG(IBL, IS) = UEI
            MASS(IBL, IS) = DSI * UEI
            TAU(IBL, IS) = 0.5 * R2 * U2 * U2 * CF2
            DIS(IBL, IS) = R2 * U2 * U2 * U2 * DI2 * HS2 * 0.5
            CTQ(IBL, IS) = CQ2
            DELT(IBL, IS) = DE2
            TSTR(IBL, IS) = HS2 * T2
            !
            !------ set "1" variables to "2" variables for next streamwise station
            CALL BLPRV(XSI, AMI, CTI, THI, DSI, DSWAKI, UEI)
            CALL BLKIN
            DO 310 ICOM = 1, NCOM
                COM1(ICOM) = COM2(ICOM)
            310   CONTINUE
            !
            !
            !------ turbulent intervals will follow transition interval or TE
            IF(TRAN .OR. IBL.EQ.IBLTE(IS)) THEN
                TURB = .TRUE.
                !
                !------- save transition location
                TFORCE(IS) = TRFORC
                XSSITR(IS) = XT
            ENDIF
            !
            TRAN = .FALSE.
            !
        1000 CONTINUE
        !
    2000 CONTINUE
    !
    RETURN
END


SUBROUTINE XIFSET(IS)
    !-----------------------------------------------------
    !     Sets forced-transition BL coordinate locations.
    !-----------------------------------------------------
    INCLUDE 'XFOIL.INC'
    INCLUDE 'XBL.INC'
    !
    IF(XSTRIP(IS).GE.1.0) THEN
        XIFORC = XSSI(IBLTE(IS), IS)
        RETURN
    ENDIF
    !
    CHX = XTE - XLE
    CHY = YTE - YLE
    CHSQ = CHX**2 + CHY**2
    !
    !---- calculate chord-based x/c, y/c
    DO 10 I = 1, N
        W1(I) = ((X(I) - XLE) * CHX + (Y(I) - YLE) * CHY) / CHSQ
        W2(I) = ((Y(I) - YLE) * CHX - (X(I) - XLE) * CHY) / CHSQ
    10   CONTINUE
    !
    CALL SPLIND(W1, W3, S, N, -999.0, -999.0)
    CALL SPLIND(W2, W4, S, N, -999.0, -999.0)
    !
    IF(IS.EQ.1) THEN
        !
        !----- set approximate arc length of forced transition point for SINVRT
        STR = SLE + (S(1) - SLE) * XSTRIP(IS)
        !
        !----- calculate actual arc length
        CALL SINVRT(STR, XSTRIP(IS), W1, W3, S, N)
        !
        !----- set BL coordinate value
        XIFORC = MIN((SST - STR), XSSI(IBLTE(IS), IS))
        !
    ELSE
        !----- same for bottom side
        !
        STR = SLE + (S(N) - SLE) * XSTRIP(IS)
        CALL SINVRT(STR, XSTRIP(IS), W1, W3, S, N)
        XIFORC = MIN((STR - SST), XSSI(IBLTE(IS), IS))
        !
    ENDIF
    !
    IF(XIFORC .LT. 0.0) THEN
        WRITE(*, 1000) IS
        1000  FORMAT(/' ***  Stagnation point is past trip on side', I2, '  ***')
        XIFORC = XSSI(IBLTE(IS), IS)
    ENDIF
    !
    RETURN
END


SUBROUTINE UPDATE
    !------------------------------------------------------------------
    !      Adds on Newton deltas to boundary layer variables.
    !      Checks for excessive changes and underrelaxes if necessary.
    !      Calculates max and rms changes.
    !      Also calculates the change in the global variable "AC".
    !        If LALFA=.TRUE. , "AC" is CL
    !        If LALFA=.FALSE., "AC" is alpha
    !------------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    REAL UNEW(IVX, 2), U_AC(IVX, 2)
    REAL QNEW(IQX), Q_AC(IQX)
    EQUIVALENCE (VA(1, 1, 1), UNEW(1, 1)) ,&
            (VB(1, 1, 1), QNEW(1))
    EQUIVALENCE (VA(1, 1, IVX), U_AC(1, 1)) ,&
            (VB(1, 1, IVX), Q_AC(1))
    REAL MSQ
    !
    !---- max allowable alpha changes per iteration
    DALMAX = 0.5*DTOR
    DALMIN = -0.5*DTOR
    !
    !---- max allowable CL change per iteration
    DCLMAX = 0.5
    DCLMIN = -0.5
    IF(MATYP.NE.1) DCLMIN = MAX(-0.5, -0.9*CL)
    !
    HSTINV = GAMM1*(MINF/QINF)**2 / (1.0 + 0.5*GAMM1*MINF**2)
    !
    !---- calculate new Ue distribution assuming no under-relaxation
    !-    also set the sensitivity of Ue wrt to alpha or Re
    DO 1 IS = 1, 2
        DO 10 IBL = 2, NBL(IS)
            I = IPAN(IBL, IS)
            !
            DUI = 0.
            DUI_AC = 0.
            DO 100 JS = 1, 2
                DO 1000 JBL = 2, NBL(JS)
                    J = IPAN(JBL, JS)
                    JV = ISYS(JBL, JS)
                    UE_M = -VTI(IBL, IS)*VTI(JBL, JS)*DIJ(I, J)
                    DUI = DUI    + UE_M*(MASS(JBL, JS)+VDEL(3, 1, JV))
                    DUI_AC = DUI_AC + UE_M*(-VDEL(3, 2, JV))
                1000       CONTINUE
            100     CONTINUE
            !
            !-------- UINV depends on "AC" only if "AC" is alpha
            IF(LALFA) THEN
                UINV_AC = 0.
            ELSE
                UINV_AC = UINV_A(IBL, IS)
            ENDIF
            !
            UNEW(IBL, IS) = UINV(IBL, IS) + DUI
            U_AC(IBL, IS) = UINV_AC      + DUI_AC
            !
        10   CONTINUE
    1 CONTINUE
    !
    !---- set new Qtan from new Ue with appropriate sign change
    DO 2 IS = 1, 2
        DO 20 IBL = 2, IBLTE(IS)
            I = IPAN(IBL, IS)
            QNEW(I) = VTI(IBL, IS)*UNEW(IBL, IS)
            Q_AC(I) = VTI(IBL, IS)*U_AC(IBL, IS)
        20   CONTINUE
    2 CONTINUE
    !
    !---- calculate new CL from this new Qtan
    SA = SIN(ALFA)
    CA = COS(ALFA)
    !
    BETA = SQRT(1.0 - MINF**2)
    BETA_MSQ = -0.5/BETA
    !
    BFAC     = 0.5*MINF**2 / (1.0 + BETA)
    BFAC_MSQ = 0.5         / (1.0 + BETA)&
            - BFAC        / (1.0 + BETA) * BETA_MSQ
    !
    CLNEW = 0.
    CL_A = 0.
    CL_MS = 0.
    CL_AC = 0.
    !
    I = 1
    CGINC = 1.0 - (QNEW(I)/QINF)**2
    CPG1 = CGINC / (BETA + BFAC*CGINC)
    CPG1_MS = -CPG1/(BETA + BFAC*CGINC)*(BETA_MSQ + BFAC_MSQ*CGINC)
    !
    CPI_Q = -2.0*QNEW(I)/QINF**2
    CPC_CPI = (1.0 - BFAC*CPG1)/ (BETA + BFAC*CGINC)
    CPG1_AC = CPC_CPI*CPI_Q*Q_AC(I)
    !
    DO 3 I = 1, N
        IP = I+1
        IF(I.EQ.N) IP = 1
        !
        CGINC = 1.0 - (QNEW(IP)/QINF)**2
        CPG2 = CGINC / (BETA + BFAC*CGINC)
        CPG2_MS = -CPG2/(BETA + BFAC*CGINC)*(BETA_MSQ + BFAC_MSQ*CGINC)
        !
        CPI_Q = -2.0*QNEW(IP)/QINF**2
        CPC_CPI = (1.0 - BFAC*CPG2)/ (BETA + BFAC*CGINC)
        CPG2_AC = CPC_CPI*CPI_Q*Q_AC(IP)
        !
        DX =  (X(IP) - X(I))*CA + (Y(IP) - Y(I))*SA
        DX_A = -(X(IP) - X(I))*SA + (Y(IP) - Y(I))*CA
        !
        AG = 0.5*(CPG2    + CPG1)
        AG_MS = 0.5*(CPG2_MS + CPG1_MS)
        AG_AC = 0.5*(CPG2_AC + CPG1_AC)
        !
        CLNEW = CLNEW + DX  *AG
        CL_A = CL_A  + DX_A*AG
        CL_MS = CL_MS + DX  *AG_MS
        CL_AC = CL_AC + DX  *AG_AC
        !
        CPG1 = CPG2
        CPG1_MS = CPG2_MS
        CPG1_AC = CPG2_AC
    3 CONTINUE
    !
    !---- initialize under-relaxation factor
    RLX = 1.0
    !
    IF(LALFA) THEN
        !===== alpha is prescribed: AC is CL
        !
        !----- set change in Re to account for CL changing, since Re = Re(CL)
        DAC = (CLNEW - CL) / (1.0 - CL_AC - CL_MS*2.0*MINF*MINF_CL)
        !
        !----- set under-relaxation factor if Re change is too large
        IF(RLX*DAC .GT. DCLMAX) RLX = DCLMAX/DAC
        IF(RLX*DAC .LT. DCLMIN) RLX = DCLMIN/DAC
        !
    ELSE
        !===== CL is prescribed: AC is alpha
        !
        !----- set change in alpha to drive CL to prescribed value
        DAC = (CLNEW - CLSPEC) / (0.0 - CL_AC - CL_A)
        !
        !----- set under-relaxation factor if alpha change is too large
        IF(RLX*DAC .GT. DALMAX) RLX = DALMAX/DAC
        IF(RLX*DAC .LT. DALMIN) RLX = DALMIN/DAC
        !
    ENDIF
    !
    RMSBL = 0.
    RMXBL = 0.
    !
    DHI = 1.5
    DLO = -.5
    !
    !---- calculate changes in BL variables and under-relaxation if needed
    DO 4 IS = 1, 2
        DO 40 IBL = 2, NBL(IS)
            IV = ISYS(IBL, IS)
            !


            !-------- set changes without underrelaxation
            DCTAU = VDEL(1, 1, IV) - DAC*VDEL(1, 2, IV)
            DTHET = VDEL(2, 1, IV) - DAC*VDEL(2, 2, IV)
            DMASS = VDEL(3, 1, IV) - DAC*VDEL(3, 2, IV)
            DUEDG = UNEW(IBL, IS) + DAC*U_AC(IBL, IS)  -  UEDG(IBL, IS)
            DDSTR = (DMASS - DSTR(IBL, IS)*DUEDG)/UEDG(IBL, IS)
            !
            !-------- normalize changes
            IF(IBL.LT.ITRAN(IS)) DN1 = DCTAU / 10.0
            IF(IBL.GE.ITRAN(IS)) DN1 = DCTAU / CTAU(IBL, IS)
            DN2 = DTHET / THET(IBL, IS)
            DN3 = DDSTR / DSTR(IBL, IS)
            DN4 = ABS(DUEDG)/0.25
            !
            !-------- accumulate for rms change
            RMSBL = RMSBL + DN1**2 + DN2**2 + DN3**2 + DN4**2
            !
            !-------- see if Ctau needs underrelaxation
            RDN1 = RLX*DN1
            IF(ABS(DN1) .GT. ABS(RMXBL)) THEN
                RMXBL = DN1
                IF(IBL.LT.ITRAN(IS)) VMXBL = 'n'
                IF(IBL.GE.ITRAN(IS)) VMXBL = 'C'
                IMXBL = IBL
                ISMXBL = IS
            ENDIF
            IF(RDN1 .GT. DHI) RLX = DHI/DN1
            IF(RDN1 .LT. DLO) RLX = DLO/DN1
            !
            !-------- see if Theta needs underrelaxation
            RDN2 = RLX*DN2
            IF(ABS(DN2) .GT. ABS(RMXBL)) THEN
                RMXBL = DN2
                VMXBL = 'T'
                IMXBL = IBL
                ISMXBL = IS
            ENDIF
            IF(RDN2 .GT. DHI) RLX = DHI/DN2
            IF(RDN2 .LT. DLO) RLX = DLO/DN2
            !
            !-------- see if Dstar needs underrelaxation
            RDN3 = RLX*DN3
            IF(ABS(DN3) .GT. ABS(RMXBL)) THEN
                RMXBL = DN3
                VMXBL = 'D'
                IMXBL = IBL
                ISMXBL = IS
            ENDIF
            IF(RDN3 .GT. DHI) RLX = DHI/DN3
            IF(RDN3 .LT. DLO) RLX = DLO/DN3
            !
            !-------- see if Ue needs underrelaxation
            RDN4 = RLX*DN4
            IF(ABS(DN4) .GT. ABS(RMXBL)) THEN
                RMXBL = DUEDG
                VMXBL = 'U'
                IMXBL = IBL
                ISMXBL = IS
            ENDIF
            IF(RDN4 .GT. DHI) RLX = DHI/DN4
            IF(RDN4 .LT. DLO) RLX = DLO/DN4
            !
        40   CONTINUE
    4 CONTINUE
    !
    !---- set true rms change
    RMSBL = SQRT(RMSBL / (4.0*FLOAT(NBL(1)+NBL(2))))
    !
    !
    IF(LALFA) THEN
        !----- set underrelaxed change in Reynolds number from change in lift
        CL = CL + RLX*DAC
    ELSE
        !----- set underrelaxed change in alpha
        ALFA = ALFA + RLX*DAC
        ADEG = ALFA/DTOR
    ENDIF
    !
    !---- update BL variables with underrelaxed changes
    DO 5 IS = 1, 2
        DO 50 IBL = 2, NBL(IS)
            IV = ISYS(IBL, IS)
            !
            DCTAU = VDEL(1, 1, IV) - DAC*VDEL(1, 2, IV)
            DTHET = VDEL(2, 1, IV) - DAC*VDEL(2, 2, IV)
            DMASS = VDEL(3, 1, IV) - DAC*VDEL(3, 2, IV)
            DUEDG = UNEW(IBL, IS) + DAC*U_AC(IBL, IS)  -  UEDG(IBL, IS)
            DDSTR = (DMASS - DSTR(IBL, IS)*DUEDG)/UEDG(IBL, IS)
            !
            CTAU(IBL, IS) = CTAU(IBL, IS) + RLX*DCTAU
            THET(IBL, IS) = THET(IBL, IS) + RLX*DTHET
            DSTR(IBL, IS) = DSTR(IBL, IS) + RLX*DDSTR
            UEDG(IBL, IS) = UEDG(IBL, IS) + RLX*DUEDG
            !
            IF(IBL.GT.IBLTE(IS)) THEN
                IW = IBL - IBLTE(IS)
                DSWAKI = WGAP(IW)
            ELSE
                DSWAKI = 0.
            ENDIF
            !
            !-------- eliminate absurd transients
            IF(IBL.GE.ITRAN(IS))&
                    CTAU(IBL, IS) = MIN(CTAU(IBL, IS) , 0.25)
            !
            IF(IBL.LE.IBLTE(IS)) THEN
                HKLIM = 1.02
            ELSE
                HKLIM = 1.00005
            ENDIF
            MSQ = UEDG(IBL, IS)**2*HSTINV&
                    / (GAMM1*(1.0 - 0.5*UEDG(IBL, IS)**2*HSTINV))
            DSW = DSTR(IBL, IS) - DSWAKI
            CALL DSLIM(DSW, THET(IBL, IS), UEDG(IBL, IS), MSQ, HKLIM)
            DSTR(IBL, IS) = DSW + DSWAKI
            !
            !-------- set new mass defect (nonlinear update)
            MASS(IBL, IS) = DSTR(IBL, IS) * UEDG(IBL, IS)
            !
        50   CONTINUE
        !
        !------ make sure there are no "islands" of negative Ue
        DO IBL = 3, IBLTE(IS)
            IF(UEDG(IBL-1, IS) .GT. 0.0 .AND.&
                    UEDG(IBL, IS) .LE. 0.0) THEN
                UEDG(IBL, IS) = UEDG(IBL-1, IS)
                MASS(IBL, IS) = DSTR(IBL, IS) * UEDG(IBL, IS)
            ENDIF
        ENDDO
    5 CONTINUE
    !
    !
    !---- equate upper wake arrays to lower wake arrays
    DO 6 KBL = 1, NBL(2)-IBLTE(2)
        CTAU(IBLTE(1)+KBL, 1) = CTAU(IBLTE(2)+KBL, 2)
        THET(IBLTE(1)+KBL, 1) = THET(IBLTE(2)+KBL, 2)
        DSTR(IBLTE(1)+KBL, 1) = DSTR(IBLTE(2)+KBL, 2)
        UEDG(IBLTE(1)+KBL, 1) = UEDG(IBLTE(2)+KBL, 2)
        TAU(IBLTE(1)+KBL, 1) = TAU(IBLTE(2)+KBL, 2)
        DIS(IBLTE(1)+KBL, 1) = DIS(IBLTE(2)+KBL, 2)
        CTQ(IBLTE(1)+KBL, 1) = CTQ(IBLTE(2)+KBL, 2)
        DELT(IBLTE(1)+KBL, 1) = DELT(IBLTE(2)+KBL, 2)
        TSTR(IBLTE(1)+KBL, 1) = TSTR(IBLTE(2)+KBL, 2)
    6 CONTINUE
    !
    RETURN
END



SUBROUTINE DSLIM(DSTR, THET, UEDG, MSQ, HKLIM)
    IMPLICIT REAL (A-H, M, O-Z)
    !
    H = DSTR/THET
    CALL HKIN(H, MSQ, HK, HK_H, HK_M)
    !
    DH = MAX(0.0, HKLIM-HK) / HK_H
    DSTR = DSTR + DH*THET
    !
    RETURN
END



SUBROUTINE BLPINI
    INCLUDE 'BLPAR.INC'
    !
    SCCON = 5.6
    GACON = 6.70
    GBCON = 0.75
    GCCON = 18.0
    DLCON = 0.9
    !
    CTRCON = 1.8
    CTRCEX = 3.3
    !
    DUXCON = 1.0
    !
    CTCON = 0.5/(GACON**2 * GBCON)
    !
    CFFAC = 1.0
    !
    RETURN
END
