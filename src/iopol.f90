!***********************************************************************
!    Module:  iopol.f
! 
!    Copyright (C) 2000 Mark Drela, Harold Youngren
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

SUBROUTINE POLREAD(LU, FNPOL, ERROR, &
        NAX, NA, CPOL, &
        REYN1, MACH1, ACRIT, XTRIP, &
        PTRAT, ETAP, &
        NAME, IRETYP, IMATYP, &
        ISX, NBL, CPOLSD, &
        CODE, VERSION)
    INCLUDE 'PINDEX.INC'
    CHARACTER*(*) FNPOL, NAME
    LOGICAL ERROR
    CHARACTER*(*) CODE
    REAL CPOL(NAX, IPTOT), CPOLSD(NAX, ISX, JPTOT)
    REAL MACH1, ACRIT(ISX), XTRIP(ISX)
    !--------------------------------------------------------
    !     Reads in polar save file
    !
    !  Input:
    !     LU      logical unit to use for reading
    !     FNPOL   name of polar file to be read,
    !               if FNPOL(1:1).eq.' ', unit LU will be read
    !               if it is already open
    !     NAX     polar point array dimension
    !     ISX     airfoil side array dimension
    !
    !  Output:
    !     ERROR   T if a READ error occurred
    !     NA      number polar points
    !     CPOL    polar coefficients and parameters
    !     REYN1   Reynolds number for CL=1
    !     MACH1   Mach number for CL=1
    !     ACRIT   Critical amplification ratio
    !     XTRIP   Trip locations
    !     PTRAT   Actuator disk total-pressure ratio
    !     ETAP    Actuator disk thermal efficiency
    !     NAME    airfoil name string
    !     IRETYP  flag giving type of Re variation with CL
    !     IMATYP  flag giving type of Ma variation with CL
    !     NBL     number of airfoil elements
    !     CPOLSD  airfoil side-related parameters
    !     CODE    code used to compute polar
    !     VERSION code version
    !--------------------------------------------------------
    CHARACTER*128 LINE
    CHARACTER*1 DUMMY
    REAL RINP(0:IPTOT + 2 * JPTOT)
    !
    INTEGER IPOL(IPTOT), ISPOL(2, IPTOT)
    INTEGER ITMP(IPTOT + 2 * JPTOT), ITMP0(IPTOT + 2 * JPTOT)
    LOGICAL LOPEN, LHEAD, LDLAB
    LOGICAL LIRE, LIMA, LJNC, LJTP
    CHARACTER*20 CPNAME
    !
    !
    ERROR = .FALSE.
    LHEAD = .TRUE.
    !
    NA = 0
    NBL = 1
    !
    !      KCH = 0
    !      KMC = 0
    !
    NIPOL = 0
    DO IP = 1, IPTOT
        IPOL(IP) = 0
    ENDDO
    DO JP = 1, JPTOT
        ISPOL(1, JP) = 0
        ISPOL(2, JP) = 0
    ENDDO
    !
    !---- assume Re,Mach will not be given in header
    IRETYP = 0
    IMATYP = 0
    !
    !---- do we have to open the file?
    LOPEN = FNPOL .NE. ' '
    !
    IF(LOPEN) OPEN(LU, FILE = FNPOL, STATUS = 'OLD', ERR = 90)
    !
    !=============================================================
    !---- start data reading loop
    500  CONTINUE
    READ(LU, 1000, END = 80) LINE
    IF(LINE.EQ.' ') GO TO 500
    !
    IF(LHEAD) THEN
        !----- parse to get header info
        !
        !----- assume this will be the data-label line
        LDLAB = .TRUE.
        !
        !--------------------------------------------
        K = INDEX(LINE, 'Version')
        IF(K.NE.0) THEN
            !------ code,version line
            DO K1 = 1, 128
                IF(LINE(K1:K1).NE.' ') GO TO 10
            ENDDO
            !
            10     CONTINUE
            IF(K.GT.K1) THEN
                CODE = LINE(K1:K - 1)
                READ(LINE(K + 7:128), *, ERR = 11) VERSION
            ENDIF
            11     CONTINUE
            LDLAB = .FALSE.
        ENDIF
        !
        !--------------------------------------------
        KF = INDEX(LINE, 'for:')
        IF(KF.NE.0) THEN
            !------ airfoil name line
            NAME = LINE(KF + 5:128)
            LDLAB = .FALSE.
        ENDIF
        !
        !--------------------------------------------
        KE = INDEX(LINE, 'elements')
        IF(KE.GT.0) THEN
            !------ element-number line
            READ(LINE(KE - 4:KE - 1), *, ERR = 60) NBL
            !------ truncate name line to eliminate elements #
            NAME = LINE(KF + 5:KE - 4)
            !
            IF(2 * NBL .GT. ISX) THEN
                NBL = ISX / 2
                WRITE(*, *)&
                        'POLREAD: Number of elements set to array limit', NBL
            ENDIF
            LDLAB = .FALSE.
        ENDIF
        !
        !--------------------------------------------
        KR = INDEX(LINE, 'Reynolds number')
        KM = INDEX(LINE, 'Mach number')
        !
        IF(KR.NE.0) THEN
            !------ Re-type line
            IF(KM.GT.KR) THEN
                KEND = KM - 1
            ELSE
                KEND = 128
            ENDIF
            IF    (INDEX(LINE(KR:KEND), 'fixed').NE.0) THEN
                IRETYP = 1
            ELSEIF(INDEX(LINE(KR:KEND), '1/sqrt(CL)').NE.0) THEN
                IRETYP = 2
            ELSEIF(INDEX(LINE(KR:KEND), '1/CL').NE.0) THEN
                IRETYP = 3
            ENDIF
            LDLAB = .FALSE.
        ENDIF
        !
        IF(KM.NE.0) THEN
            !------ Ma-type line
            IF(KR.GT.KM) THEN
                KEND = KR - 1
            ELSE
                KEND = 128
            ENDIF
            IF    (INDEX(LINE(KM:KEND), 'fixed').NE.0) THEN
                IMATYP = 1
            ELSEIF(INDEX(LINE(KM:KEND), '1/sqrt(CL)').NE.0) THEN
                IMATYP = 2
            ELSEIF(INDEX(LINE(KM:KEND), '1/CL').NE.0) THEN
                IMATYP = 3
            ENDIF
            LDLAB = .FALSE.
        ENDIF
        !
        !--------------------------------------------
        !---- find specified BL trip location
        K = INDEX(LINE, 'xtrf')
        IF(K.NE.0) THEN
            !------ new style xtrip line
            KT = INDEX(LINE, '(top)')
            KB = INDEX(LINE, '(bottom)')
            KE = INDEX(LINE, 'element ')
            !--- check for old style trip line
            KS = INDEX(LINE, '(suc')
            KP = INDEX(LINE, '(pre')
            !
            IF(KE.NE.0) THEN
                READ(LINE(KE + 7:KE + 12), *, ERR = 21) N
            ELSE
                N = 1
            ENDIF
            IF(N.LE.NBL) THEN
                IS1 = 2 * N - 1
                IS2 = 2 * N
                XTRIP(IS1) = 1.0
                XTRIP(IS2) = 1.0
                IF(KT.GT.0)  READ(LINE(K + 6:KT - 1), *, ERR = 21) XTRIP(IS1)
                IF(KB.GT.KT) READ(LINE(KT + 5:KB - 1), *, ERR = 21) XTRIP(IS2)
                IF(KS.GT.0)  READ(LINE(K + 6:KS - 1), *, ERR = 21) XTRIP(IS1)
                IF(KP.GT.KS) READ(LINE(KS + 5:KP - 1), *, ERR = 21) XTRIP(IS2)
            ENDIF
            21     CONTINUE
            LDLAB = .FALSE.
        ENDIF
        !
        !--------------------------------------------
        K = INDEX(LINE, 'Mach =')
        IF(K.NE.0) THEN
            READ(LINE(K + 6:128), *, ERR = 31) MACH1
            31     CONTINUE
            LDLAB = .FALSE.
        ENDIF
        !
        !--------------------------------------------
        K = INDEX(LINE, 'Re =')
        IF(K.NE.0) THEN
            READ(LINE(K + 4:128), *, ERR = 32) REYN1
            REYN1 = REYN1 * 1.0E6
            32     CONTINUE
            LDLAB = .FALSE.
        ENDIF
        !
        !--------------------------------------------
        K = INDEX(LINE, 'Ncrit =')
        IF(K.NE.0) THEN
            NINP = 2
            CALL GETFLT(LINE(k + 7:128), RINP(1), NINP, ERROR)
            IF(NINP.LE.0 .OR. ERROR) GO TO 33
            !
            IF(NINP.EQ.1) THEN
                ACRIT(1) = RINP(1)
                ACRIT(2) = RINP(1)
            ELSE
                ACRIT(1) = RINP(1)
                ACRIT(2) = RINP(2)
            ENDIF
            33     CONTINUE
            LDLAB = .FALSE.
        ENDIF
        !
        !--------------------------------------------
        K = INDEX(LINE, 'pi_p =')
        IF(K.NE.0) THEN
            READ(LINE(K + 6:128), *, ERR = 34) PTRAT
            34     CONTINUE
            LDLAB = .FALSE.
        ENDIF
        !
        !--------------------------------------------
        K = INDEX(LINE, 'eta_p =')
        IF(K.NE.0) THEN
            READ(LINE(K + 7:128), *, ERR = 35) ETAP
            35     CONTINUE
            LDLAB = .FALSE.
        ENDIF
        !
        !--------------------------------------------
        IF(LDLAB .AND. NIPOL.EQ.0) THEN
            !------ process line for possible data labels
            DO IP = 1, IPTOT
                CALL STRIP(CPOLNAME(IP), NNAME)
                !
                !-------- mark this parameter for reading
                K = INDEX(LINE, CPOLNAME(IP)(1:NNAME))
                ITMP0(IP) = K
                ITMP(IP) = K
            ENDDO
            !
            DO JP = 1, JPTOT
                CALL STRIP(CPOLSNAME(JP), NNAME)
                !
                CPNAME = 'Top ' // CPOLSNAME(JP)
                K1 = INDEX(LINE, CPNAME(1:NNAME + 4))
                CPNAME = 'Top_' // CPOLSNAME(JP)
                K2 = INDEX(LINE, CPNAME(1:NNAME + 4))
                ITMP0(IPTOT + JP) = MAX(K1, K2)
                ITMP (IPTOT + JP) = MAX(K1, K2)
                !
                CPNAME = 'Bot ' // CPOLSNAME(JP)
                K1 = INDEX(LINE, CPNAME(1:NNAME + 4))
                CPNAME = 'Bot_' // CPOLSNAME(JP)
                K2 = INDEX(LINE, CPNAME(1:NNAME + 4))
                ITMP0(IPTOT + JP + JPTOT) = MAX(K1, K2)
                ITMP (IPTOT + JP + JPTOT) = MAX(K1, K2)
            ENDDO
            !
            !------ bubble-sort data label positions in line string
            DO IPASS = 1, IPTOT + 2 * JPTOT
                DO IP = 1, IPTOT + 2 * JPTOT - 1
                    IF(ITMP(IP).GT.ITMP(IP + 1)) THEN
                        ITMPP1 = ITMP(IP + 1)
                        ITMP(IP + 1) = ITMP(IP)
                        ITMP(IP) = ITMPP1
                    ENDIF
                ENDDO
            ENDDO
            !
            !------ assign data position to each parameter
            DO IPT = 1, IPTOT + 2 * JPTOT
                IF(ITMP(IPT).GT.0) THEN
                    NIPOL = NIPOL + 1
                    DO IP = 1, IPTOT
                        IF(ITMP(IPT).EQ.ITMP0(IP)) IPOL(IP) = NIPOL
                    ENDDO
                    DO JP = 1, JPTOT
                        IF(ITMP(IPT).EQ.ITMP0(IPTOT + JP)) ISPOL(1, JP) = NIPOL
                        IF(ITMP(IPT).EQ.ITMP0(IPTOT + JPTOT + JP)) ISPOL(2, JP) = NIPOL
                    ENDDO
                ENDIF
            ENDDO
            !
        ENDIF
        !
        !--------------------------------------------
        IF(INDEX(LINE, '-----').NE.0) THEN
            LHEAD = .FALSE.
        ENDIF
        !
        !--------------------------------------------------------------
    ELSE
        !----- read polar data lines
        IA = NA + 1
        !
        NINP = IPTOT + 2 * JPTOT
        CALL GETFLT(LINE, RINP(1), NINP, ERROR)
        IF(ERROR) GO TO 90
        !
        DO IP = 1, IPTOT
            CPOL(IA, IP) = RINP(IPOL(IP))
        ENDDO
        !
        DO JP = 1, JPTOT
            DO N = 1, NBL
                IS1 = 2 * N - 1
                IS2 = 2 * N
                CPOLSD(IA, IS1, JP) = RINP(ISPOL(1, JP) + 2 * (N - 1))
                CPOLSD(IA, IS2, JP) = RINP(ISPOL(2, JP) + 2 * (N - 1))
            ENDDO
        ENDDO
        !
        ACL = MAX(CPOL(IA, ICL), 0.001)
        !
        !
        !----- try to find Re, Ma, Ncrit, Xtrip  in polar data
        LIRE = .FALSE.
        LIMA = .FALSE.
        LJNC = .FALSE.
        LJTP = .FALSE.
        DO KP = 1, NIPOL
            IF(IPOL(KP) .EQ. IRE) LIRE = .TRUE.
            IF(IPOL(KP) .EQ. IMA) LIMA = .TRUE.
            IF(ISPOL(1, KP) .EQ. JNC) LJNC = .TRUE.
            IF(ISPOL(1, KP) .EQ. JTP) LJTP = .TRUE.
        ENDDO
        !
        IF(.NOT. LIRE) THEN
            !------ Re was not in polar data... set using header info
            IF    (IRETYP.EQ.1) THEN
                CPOL(IA, IRE) = REYN1
            ELSEIF(IRETYP.EQ.2) THEN
                CPOL(IA, IRE) = REYN1 / SQRT(ACL)
            ELSEIF(IRETYP.EQ.3) THEN
                CPOL(IA, IRE) = REYN1 / ACL
            ENDIF
        ENDIF
        !
        IF(.NOT. LIMA) THEN
            !------ Mach was not in polar data... set using header info
            IF    (IMATYP.EQ.1) THEN
                CPOL(IA, IMA) = MACH1
            ELSEIF(IMATYP.EQ.2) THEN
                CPOL(IA, IMA) = MACH1 / SQRT(ACL)
            ELSEIF(IMATYP.EQ.3) THEN
                CPOL(IA, IMA) = MACH1 / ACL
            ENDIF
        ENDIF
        !
        IF(.NOT. LJNC) THEN
            !------ Ncrit was not in polar data... set using header info
            DO IS = 1, 2 * NBL
                CPOLSD(IA, IS, JNC) = ACRIT(IS)
            ENDDO
        ENDIF
        !
        IF(.NOT. LJTP) THEN
            !------ set trip data using header info
            DO IS = 1, 2 * NBL
                CPOLSD(IA, IS, JTP) = XTRIP(IS)
            ENDDO
        ENDIF
        !
        NA = IA
    ENDIF
    !
    60   CONTINUE
    !---- go read next line
    GO TO 500
    !=============================================================
    !
    80   CONTINUE
    !---- if file was opened here, then close it
    IF(LOPEN) CLOSE(LU)
    RETURN
    !
    90   CONTINUE
    IF(LOPEN) CLOSE(LU)
    ERROR = .TRUE.
    RETURN
    !
    !..........................................
    1000 FORMAT(A)
END
! POLREAD


SUBROUTINE POLWRIT(LU, FNPOL, ERROR, LHEAD, &
        NAX, IA1, IA2, CPOL, IPOL, NIPOL, &
        REYN1, MACH1, ACRIT, XTRIP, &
        PTRAT, ETAP, &
        NAME, IRETYP, IMATYP, &
        ISX, NBL, CPOLSD, JPOL, NJPOL, &
        CODE, VERSION, LQUERY)
    INCLUDE 'PINDEX.INC'
    CHARACTER*(*) FNPOL, NAME
    LOGICAL ERROR, LHEAD, LQUERY
    CHARACTER*(*) CODE
    REAL CPOL(NAX, IPTOT), CPOLSD(NAX, ISX, JPTOT)
    REAL MACH1, ACRIT(ISX), XTRIP(ISX)
    INTEGER IPOL(IPTOT), JPOL(JPTOT)
    !--------------------------------------------------------
    !     Writes polar save file
    !
    !  Input:
    !     LU       logical unit to use for writing
    !     FNPOL    name of polar file to be read,
    !                if FNPOL(1:1).eq.' ', unit LU is assumed
    !                to be already open
    !     NAX      polar point array dimension
    !     ISX      airfoil side array dimension
    !     IA1,IA2  only polar points IA1..IA2 are written
    !     CPOL     polar coefficients and parameters
    !     IPOL(.)  indices of data quantities to be written
    !     NIPOL    number  of data quantities to be written
    !     REYN1    Reynolds number for CL=1
    !     MACH1    Mach number for CL=1
    !     ACRIT    Critical amplification ratio
    !     XTRIP    Trip locations
    !     PTRAT   Actuator disk total-pressure ratio
    !     ETAP    Actuator disk thermal efficiency
    !     NAME     airfoil name string
    !     IRETYP   flag giving type of Re variation with CL
    !     IMATYP   flag giving type of Ma variation with CL
    !     NBL      number of airfoil elements
    !     CPOLSD   airfoil side-related parameters
    !     JPOL(.)  indices of side data quantities to be written
    !     NJPOL    number  of side data quantities to be written
    !     LHEAD    T if header and column label are to be written
    !     CODE     code used to compute polar
    !     VERSION  code version
    !     LQUERY   if T, asks permission to overwrite existing file
    !
    !  Output:
    !     ERROR   T if a OPER or WRITE error occurred
    !--------------------------------------------------------
    CHARACTER*29 LINE1, LINE2
    CHARACTER*128 LINEL, LINED, LINEF
    CHARACTER*1 ANS
    LOGICAL LOPEN
    !
    ERROR = .FALSE.
    !
    !---- do we have to open the file?
    LOPEN = FNPOL .NE. ' '
    !
    IF(LOPEN) THEN
        OPEN(LU, FILE = FNPOL, STATUS = 'OLD', ERR = 20)
        !
        IF(LQUERY) THEN
            WRITE(*, *)
            WRITE(*, *) 'Output file exists.  Overwrite?  Y'
            READ(*, 1000) ANS
            !
            IF(INDEX('Nn', ANS).EQ.0) GO TO 22
            !
            CLOSE(LU)
            WRITE(*, *) 'Polar file not saved'
            RETURN
        ENDIF
        !
        20    OPEN(LU, FILE = FNPOL, STATUS = 'UNKNOWN', ERR = 90)
        22    REWIND(LU)
    ENDIF
    !
    IF(LHEAD) THEN
        WRITE(LU, *) ' '
        WRITE(LU, 8000) CODE, VERSION
        WRITE(LU, *) ' '
        IF(NBL.EQ.1) THEN
            WRITE(LU, 9001) NAME
        ELSE
            WRITE(LU, 9002) NAME, NBL
        ENDIF
        !
        IFFBC = 0
        ISMOM = 0
        !
        IF(IFFBC.NE.0 .AND. ISMOM.NE.0) THEN
            IF(IFFBC.EQ.1)  LINE1 = ' Solid wall far field        '
            IF(IFFBC.EQ.2)  LINE1 = ' Vortex + doublet far field  '
            IF(IFFBC.EQ.3)  LINE1 = ' Constant pressure far field '
            IF(IFFBC.EQ.4)  LINE1 = ' Supersonic wave far field   '
            IF(IFFBC.GE.5)  LINE1 = '                             '
            IF(ISMOM.EQ.1)  LINE2 = '   S-momentum conserved      '
            IF(ISMOM.EQ.2)  LINE2 = '   Entropy conserved         '
            IF(ISMOM.EQ.3)  LINE2 = '   Entropy conserved near LE '
            IF(ISMOM.EQ.4)  LINE2 = '   S-mom conserved at shocks '
            IF(ISMOM.GE.5)  LINE2 = '                             '
            WRITE(LU, 9006) LINE1, LINE2
            9006   FORMAT(1X, 3X, 2A29)
        ENDIF
        !
        WRITE(LU, *) ' '
        !
        LINE1 = ' '
        LINE2 = ' '
        IF(IRETYP.EQ.1) LINE1 = ' Reynolds number fixed       '
        IF(IRETYP.EQ.2) LINE1 = ' Reynolds number ~ 1/sqrt(CL)'
        IF(IRETYP.EQ.3) LINE1 = ' Reynolds number ~ 1/CL      '
        IF(IMATYP.EQ.1) LINE2 = '   Mach number fixed         '
        IF(IMATYP.EQ.2) LINE2 = '   Mach number ~ 1/sqrt(CL)  '
        IF(IMATYP.EQ.3) LINE2 = '   Mach number ~ 1/CL        '
        WRITE(LU, 9005) IRETYP, IMATYP, LINE1, LINE2
        !
        WRITE(LU, *) ' '
        DO N = 1, NBL
            IS1 = 2 * N - 1
            IS2 = 2 * N
            IF(NBL.EQ.1) THEN
                WRITE(LU, 9011) XTRIP(IS1), XTRIP(IS2)
            ELSE
                WRITE(LU, 9012) XTRIP(IS1), XTRIP(IS2), N
            ENDIF
        ENDDO
        WRITE(LU, 9015) MACH1, REYN1 / 1.0E6, ACRIT(1), ACRIT(2)
        IF(PTRAT .NE. 0.0) WRITE(LU, 9017) PTRAT, ETAP
        WRITE(LU, *) ' '
        !
        LINEL = ' '
        LINED = ' '
        !
        KL = 1
        KD = 1
        !
        DO 30 KP = 1, NIPOL
            IP = IPOL(KP)
            IF(IP.EQ.0) GO TO 30
            !
            KDOT = INDEX(CPOLFORM(IP), '.')
            IF(KDOT.EQ.0) KDOT = LEN(CPOLFORM(IP))
            READ(CPOLFORM(IP)(2:KDOT - 1), *, ERR = 95) NFORM
            !
            CALL STRIP(CPOLNAME(IP), NNAME)
            NBLANK = MAX((NFORM - NNAME + 2) / 2, 0)
            !
            LINEL(KL + 1 + NBLANK:KL + NNAME + NBLANK) = CPOLNAME(IP)(1:NNAME)
            KL = KL + NFORM
            !
            LINED(KD + 2:KD + NFORM) = '--------------------------------'
            KD = KD + NFORM
        30    CONTINUE
        !
        DO 32 KP = 1, NJPOL
            JP = JPOL(KP)
            IF(JP.EQ.0) GO TO 32
            !
            KDOT = INDEX(CPOLSFORM(JP), '.')
            IF(KDOT.EQ.0) KDOT = LEN(CPOLSFORM(JP))
            READ(CPOLSFORM(JP)(2:KDOT - 1), *, ERR = 95) NFORM
            !
            CALL STRIP(CPOLSNAME(JP), NNAME)
            NBLANK = MAX((NFORM - NNAME - 2) / 2, 0)
            !
            DO N = 1, NBL
                LINEL(KL + 1 + NBLANK:KL + 4 + NNAME + NBLANK) = &
                        'Top_' // CPOLSNAME(JP)(1:NNAME)
                KL = KL + NFORM
                !
                LINED(KD + 2:KD + NFORM) = '--------------------------------'
                KD = KD + NFORM
                !
                LINEL(KL + 1 + NBLANK:KL + 4 + NNAME + NBLANK) = &
                        'Bot_' // CPOLSNAME(JP)(1:NNAME)
                KL = KL + NFORM
                !
                LINED(KD + 2:KD + NFORM) = '--------------------------------'
                KD = KD + NFORM
            ENDDO
        32    CONTINUE
        !
        !
        !
        !       LINEL =
        !     & '  alpha     CL        CD       CDp       CM    Top_Xtr Bot_Xtr'
        !CC     1234567890123456789012345678901234567890123456789012345678901234567890
        !       K = 62
        !
        !
        !       LINEL =
        !     & ' ------- -------- --------- --------- -------- ------- -------'
        !CC       3.453   1.3750   0.00921   0.00512  -0.1450  0.9231  0.5382
        !CC       3.453   1.3750   0.00921   0.00213  -0.1450  0.9231  0.5382
        !       K = 62

        WRITE(LU, 1000) LINEL(1:KL)
        WRITE(LU, 1000) LINED(1:KD)
        !
    ENDIF
    !

    LINEF = '(1X'
    KF = 3
    DO KP = 1, NIPOL
        IP = IPOL(KP)
        NF = LEN(CPOLFORM(IP))
        !
        LINEF(KF + 1:KF + NF + 1) = ',' // CPOLFORM(IP)
        KF = KF + NF + 1
    ENDDO
    DO KP = 1, NJPOL
        JP = JPOL(KP)
        NF = LEN(CPOLSFORM(JP))
        !
        DO N = 1, NBL
            LINEF(KF + 1:KF + NF + 1) = ',' // CPOLSFORM(JP)
            KF = KF + NF + 1
            !
            LINEF(KF + 1:KF + NF + 1) = ',' // CPOLSFORM(JP)
            KF = KF + NF + 1
        ENDDO
    ENDDO
    LINEF(KF + 1:KF + 1) = ')'
    KF = KF + 1
    !
    !
    DO 40 IA = IA1, IA2
        WRITE(LU, LINEF)&
                (CPOL(IA, IPOL(KP)), KP = 1, NIPOL), &
                ((CPOLSD(IA, IS, JPOL(KP)), IS = 1, 2 * NBL), KP = 1, NJPOL)
    40 CONTINUE
    !
    !
    !---- if file was opened here, then close it
    IF(LOPEN) CLOSE(LU)
    RETURN
    !
    90   CONTINUE
    ERROR = .TRUE.
    RETURN
    !
    95   CONTINUE
    WRITE(*, *) '? Bad CPOLFORM set up in PINDEX.INC'
    STOP
    !
    !......................................................................
    1000 FORMAT(A)
    8000 FORMAT(7X, A, 9X, 'Version', F5.2)
    9001 FORMAT(1X, 'Calculated polar for: ', A)
    9002 FORMAT(1X, 'Calculated polar for: ', A, I4, ' elements')
    9005 FORMAT(1X, I1, I2, 2A29)
    9011 FORMAT(1X, &
            'xtrf = ', F7.3, ' (top)    ', F9.3, ' (bottom)  ')
    9012 FORMAT(1X, &
            'xtrf = ', F7.3, ' (top)    ', F9.3, ' (bottom)     element', I3)
    9015 FORMAT(1X, &
            'Mach = ', F7.3, 5X, 'Re = ', F9.3, ' e 6', 5X, 'Ncrit = ', 20F7.3)
    9017 FORMAT(1X, &
            'pi_p = ', F7.4, 5X, 'eta_p = ', F9.4)
    !CC      3.453   1.3750   0.00921     0.500  -0.1450  0.9231  0.5382 -0.00942
    !CC      3.453   1.3750   0.00921     0.500  -0.1450  0.9231  0.5382
END


SUBROUTINE POLREF(LU, FNREF, ERROR, &
        NFX, NF, XYREF, LABREF)
    INCLUDE 'PINDEX.INC'
    CHARACTER*(*) FNREF, LABREF
    LOGICAL ERROR
    DIMENSION NF(4)
    DIMENSION XYREF(NFX, 2, 4)
    !--------------------------------------------------------
    !     Reads in polar reference data file
    !
    !  Input:
    !     LU      logical unit to use for reading
    !     FNREF   name of polar file to be read,
    !               if FNREF(1:1).eq.' ', unit LU is assumed
    !               to be already open
    !     NFX     polar point array dimension
    !
    !  Output:
    !     ERROR      T if a READ error occurred
    !     NF(.)      number of points in each data block
    !     XYREF(...) reference polar data
    !     LABREF(.)  reference polar label
    !--------------------------------------------------------
    LOGICAL LOPEN
    CHARACTER*80 LINE
    !
    ERROR = .FALSE.
    LOPEN = FNREF(1:1) .NE. ' '
    IF(LOPEN) OPEN(LU, FILE = FNREF, STATUS = 'OLD', ERR = 900)
    !
    !---- try to read data label
    READ(LU, 1000, END = 900) LINE
    1000 FORMAT(A)
    !
    !---- set data label if present
    IF(LINE(1:1).EQ.'#') THEN
        LABREF = LINE(2:80)
    ELSE
        LABREF = ' '
        REWIND(LU)
    ENDIF
    !
    DO 100 K = 1, 4
        DO 10 I = 1, NFX
            READ(LU, *, END = 11, ERR = 900) XYREF(I, 1, K), XYREF(I, 2, K)
            IF(XYREF(I, 1, K) .EQ. 999.0) GO TO 11
        10   CONTINUE
        11   NF(K) = I - 1
    100 CONTINUE
    IF(LOPEN) CLOSE(LU)
    RETURN
    !
    900 CONTINUE
    ERROR = .TRUE.
    !
    RETURN
END
! POLREF