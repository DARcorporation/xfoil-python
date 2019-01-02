!***********************************************************************
!    Module:  xpol.f
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


SUBROUTINE PLRSET(IP)
    !--------------------------------------------------------------
    !     Selects slot IP for saving polar.
    !     Resets all parameters if necessary.
    !--------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    LOGICAL ERROR
    !
    IF(IP <= 0) THEN
        !----- invalid polar index
        RETURN
        !
    ELSEIF(IP >= 1 .AND. IP <= NPOL) THEN
        WRITE(*, *)
        WRITE(*, *) 'Existing stored polar is chosen for appending...'
        NIPOL = NIPOL0
        IF(LCMINP) THEN
            NIPOL = NIPOL + 1
            IPOL(NIPOL) = IMC
        ENDIF
        IF(LHMOMP) THEN
            NIPOL = NIPOL + 1
            IPOL(NIPOL) = ICH
        ENDIF
        CALL POLWRIT(6, ' ', ERROR, .TRUE., &
                NAX, 1, NAPOL(IP), CPOL(1, 1, IP), IPOL, NIPOL, &
                REYNP1(IP), MACHP1(IP), ACRITP(1, IP), XSTRIPP(1, IP), &
                PTRATP(IP), ETAPP(IP), &
                NAMEPOL(IP), IRETYP(IP), IMATYP(IP), &
                ISX, 1, CPOLSD(1, 1, 1, IP), JPOL, NJPOL, &
                'XFOIL', VERSION, .FALSE.)
        NIPOL = NIPOL0
        !
        !----- check if geometries differ...
        IF(N /= NXYPOL(IP)) GO TO 10
        SIZREF = S(N) - S(1)
        DO I = 1, N
            DSQ = (X(I) - CPOLXY(I, 1, IP))**2 + (Y(I) - CPOLXY(I, 2, IP))**2
            DSFRAC = SQRT(DSQ) / SIZREF
            IF(DSFRAC > 0.00001) GO TO 10
        ENDDO
        GO TO 20
        !
        10    WRITE(*, *) 'Current airfoil differs from airfoil of stored polar'
        WRITE(*, 1100)
        1100  FORMAT(&
                /'   - - - - - - - - - - - - - - - - - - - - - - - - - - - -'&
                /'    0  abort polar accumulation'&
                /'    1  compute with current airfoil'&
                /'    2  compute with stored  airfoil', &
                ' (overwrite current airfoil)')
        CALL ASKI('   Select action^', IOPT)
        IF(IOPT == 0) THEN
            IP = 0
            RETURN
        ELSEIF(IOPT == 1) THEN
            CONTINUE
        ELSEIF(IOPT == 2) THEN
            CALL APCOPY(IP)
        ENDIF
        !
        20    CONTINUE
        WRITE(*, *)
        WRITE(*, *) 'Setting current parameters to those of stored polar'
        !
        NAME = NAMEPOL(IP)
        CALL STRIP(NAME, NNAME)
        !
        RETYP = IRETYP(IP)
        MATYP = IMATYP(IP)
        !
        MINF1 = MACHP1(IP)
        REINF1 = REYNP1(IP)
        !
        ACRIT(1) = ACRITP(1, IP)
        ACRIT(2) = ACRITP(2, IP)
        !
        XSTRIP(1) = XSTRIPP(1, IP)
        XSTRIP(2) = XSTRIPP(2, IP)
        !
    ELSE
        !----- new polar slot is chosen
        NPOL = IP
        !
        NAPOL(IP) = 0
        !
        NAMEPOL(IP) = NAME
        IRETYP(IP) = RETYP
        IMATYP(IP) = MATYP
        !
        IF(LVISC) THEN
            REYNP1(IP) = REINF1
        ELSE
            REYNP1(IP) = 0.
        ENDIF
        MACHP1(IP) = MINF1
        !
        ACRITP(1, IP) = ACRIT(1)
        ACRITP(2, IP) = ACRIT(2)
        !
        XSTRIPP(1, IP) = XSTRIP(1)
        XSTRIPP(2, IP) = XSTRIP(2)
        !
        NXYPOL(IP) = N
        DO I = 1, N
            CPOLXY(I, 1, IP) = X(I)
            CPOLXY(I, 2, IP) = Y(I)
        ENDDO
        !
        WRITE(*, 2100) IP, NAMEPOL(IP)
        2100  FORMAT(/' Polar', I3, ' newly created for accumulation'&
                /' Airfoil archived with polar: ', A)
    ENDIF
    !
END
! PLRSET


SUBROUTINE APCOPY(IP)
    INCLUDE 'XFOIL.INC'
    !
    NOLD = N

    N = NXYPOL(IP)
    DO I = 1, N
        X(I) = CPOLXY(I, 1, IP)
        Y(I) = CPOLXY(I, 2, IP)
    ENDDO
    NAME = NAMEPOL(IP)
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
    IF(NOLD /= N) LBLINI = .FALSE.
    !
    RETURN
END
! APCOPY



SUBROUTINE PLRINI(LU, IP)
    !--------------------------------------------------------------
    !     Checks or initializes a polar save file.
    !
    !     If file PFNAME(IP) exists, it is checked for consistency 
    !        with current parameters.  Polar saving is enabled
    !        only if file parameters match current parameters.
    !
    !     If file PFNAME(IP) doesn't exist, a new one is set up by 
    !        writing a header to it, and polar saving is enabled.
    !--------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    CHARACTER*128 LINE, LINEL, PROMPT
    !
    LOGICAL NAMDIF, ERROR
    !
    INTEGER NBLP(ISX, IPX)
    !
    REAL RINP(IPTOT)
    !
    CALL STRIP(PFNAME(IP), NPF)
    IF(NPF == 0) THEN
        PROMPT = 'Enter  polar save filename'&
                // '  OR  <return> for no file^'
    ELSE
        WRITE(*, *) 'Default polar save filename:  ', PFNAME(IP)(1:NPF)
        PROMPT = 'Enter  new filename'&
                // '  OR  "none"'&
                // '  OR  <return> for default^'
    ENDIF
    !
    CALL ASKS(PROMPT, FNAME)
    CALL STRIP(FNAME, NFN)
    !
    IF(NFN == 0) THEN
        FNAME = PFNAME(IP)
        NFN = NPF
    ELSEIF(INDEX('NONEnone', FNAME(1:4)) /= 0) THEN
        NFN = 0
    ENDIF
    !
    IF(NFN == 0) THEN
        LPFILE = .FALSE.
        WRITE(*, *)
        WRITE(*, *) 'Polar save file will NOT be written'
        RETURN
    ENDIF
    !
    !---- no valid file yet
    LPFILE = .FALSE.
    !
    !---- try reading the polar file to see if it exists
    OPEN(LU, FILE = FNAME, STATUS = 'OLD', ERR = 60)
    CALL POLREAD(LU, ' ', ERROR, &
            NAX, NAPOL(IP), CPOL(1, 1, IP), &
            REYNP1(IP), MACHP1(IP), ACRITP(1, IP), XSTRIPP(1, IP), &
            PTRATP(IP), ETAPP(IP), &
            NAMEPOL(IP), IRETYP(IP), IMATYP(IP), &
            ISX, NBLP(1, IP), CPOLSD(1, 1, 1, IP), &
            CODEPOL(IP), VERSPOL(IP))
    IF(ERROR) GO TO 90
    CLOSE(LU)
    PFNAME(IP) = FNAME
    !
    CALL STRIP(NAMEPOL(IP), NNAMEP)
    !
    !---- check to see if the names are different
    IF(NNAME /= NNAMEP) THEN
        NAMDIF = .TRUE.
    ELSE
        NAMDIF = .FALSE.
        DO K = 1, NNAME
            IF(NAME(K:K) /= NAMEPOL(IP)(K:K)) NAMDIF = .TRUE.
        ENDDO
    ENDIF
    !
    !---- check if the polar save file is for the same airfoil and conditions
    IF(NAMDIF                   .OR.&
            REYNP1(IP) /= REINF1   .OR.&
            MACHP1(IP) /= MINF1    .OR.&
            IRETYP(IP) /= RETYP    .OR.&
            IMATYP(IP) /= MATYP    .OR.&
            ACRITP(1, IP) /= ACRIT(1)    .OR.&
            ACRITP(2, IP) /= ACRIT(2)    .OR.&
            XSTRIPP(1, IP) /= XSTRIP(1)  .OR.&
            XSTRIPP(2, IP) /= XSTRIP(2)) THEN
        !
        WRITE(*, 6600)  NAME, NAMEPOL(IP), &
                REINF1, REYNP1(IP), &
                MINF1, MACHP1(IP), &
                RETYP, IRETYP(IP), &
                MATYP, IMATYP(IP), &
                ACRIT(1), ACRITP(1, IP), &
                ACRIT(2), ACRITP(2, IP), &
                XSTRIP(1), XSTRIPP(1, IP), &
                XSTRIP(2), XSTRIPP(2, IP)
        !
        6600  FORMAT(&
                /'               Current                         Save file'&
                /'           ------------------              ------------------'&
                /' name  :   ', A, A&
                /' Re    :   ', F12.0, 20X, F12.0&
                /' Mach  :   ', F12.4, 20X, F12.4&
                /' Retyp :   ', I7, 25X, I7&
                /' Matyp :   ', I7, 25X, I7&
                /' NcritT:   ', F12.4, 20X, F12.4&
                /' NcritB:   ', F12.4, 20X, F12.4&
                /' xtr T :   ', F12.4, 20X, F12.4&
                /' xtr B :   ', F12.4, 20X, F12.4)
        !
        WRITE(*, *)
        WRITE(*, *)&
                'Current parameters different from old save file values.'
        CALL ASKL&
                ('Set current parameters to old save file values ?^', OK)
        !
        IF(OK) THEN
            NAME = NAMEPOL(IP)
            NNAME = NNAMEP
            REINF1 = REYNP1(IP)
            MINF1 = MACHP1(IP)
            RETYP = IRETYP(IP)
            MATYP = IMATYP(IP)
            ACRIT(1) = ACRITP(1, IP)
            ACRIT(2) = ACRITP(2, IP)
            XSTRIP(1) = XSTRIPP(1, IP)
            XSTRIP(2) = XSTRIPP(2, IP)
        ELSE
            WRITE(*, *)
            WRITE(*, *) 'Old polar save file NOT available for appending'
            RETURN
        ENDIF
    ENDIF
    !
    !---- display polar save file just read in
    WRITE(*, *)
    WRITE(*, *) 'Old polar save file read in ...'
    CALL POLWRIT(6, ' ', ERROR, .TRUE., &
            NAX, 1, NAPOL(IP), CPOL(1, 1, IP), IPOL, NIPOL, &
            REYNP1(IP), MACHP1(IP), ACRITP(1, IP), XSTRIPP(1, IP), &
            PTRATP(IP), ETAPP(IP), &
            NAMEPOL(IP), IRETYP(IP), IMATYP(IP), &
            ISX, 1, CPOLSD(1, 1, 1, IP), JPOL, NJPOL, &
            CODEPOL(IP), VERSPOL(IP), .FALSE.)
    !
    !---- enable writing to the save file
    LPFILE = .TRUE.
    WRITE(*, *)
    WRITE(*, *) 'Old polar save file available for appending'
    RETURN
    !
    !
    !---- the polar save file doesn't exist, so write new header
    60 CONTINUE
    NIPOL = NIPOL0
    IF(LCMINP) THEN
        NIPOL = NIPOL + 1
        IPOL(NIPOL) = IMC
    ENDIF
    IF(LHMOMP) THEN
        NIPOL = NIPOL + 1
        IPOL(NIPOL) = ICH
    ENDIF
    !
    OPEN(LU, FILE = FNAME, STATUS = 'NEW', ERR = 80)
    IA1 = 0
    IA2 = -1
    CALL POLWRIT(LU, ' ', ERROR, .TRUE., &
            NAX, IA1, IA2, CPOL(1, 1, IP), IPOL, NIPOL, &
            REYNP1(IP), MACHP1(IP), ACRITP(1, IP), XSTRIPP(1, IP), &
            PTRATP(IP), ETAPP(IP), &
            NAMEPOL(IP), IRETYP(IP), IMATYP(IP), &
            ISX, 1, CPOLSD(1, 1, 1, IP), JPOL, NJPOL, &
            'XFOIL', VERSION, .FALSE.)
    CLOSE(LU)
    PFNAME(IP) = FNAME
    !
    NIPOL = NIPOL0
    !
    !---- enable writing to the save file
    LPFILE = .TRUE.
    WRITE(*, *)
    WRITE(*, *) 'New polar save file available'
    RETURN
    !
    !---- the polar save file doesn't exist, so write new header
    80 WRITE(*, *) 'New polar save file OPEN error'
    RETURN
    !
    !---- READ error trap
    90 WRITE(*, *) 'Old polar save file READ error'
    CLOSE(LU)
    RETURN
    !
    !..........................................
    1000 FORMAT(A)
    1010 FORMAT(22X, A32)
    1020 FORMAT(8X, F7.3, 10X, F9.3)
    1030 FORMAT(8X, F7.3, 10X, F9.3, 17X, F7.3)
END
! PLRINI



SUBROUTINE PLXINI(LU, IP)
    !--------------------------------------------------------------
    !     Checks or initializes a polar dump file.
    !
    !     If file PFNAMX(IP) exists, it is checked for consistency 
    !        with current parameters.  Polar dumping is enabled
    !        only if file parameters match current parameters.
    !
    !     If file PFNAMX(IP) doesn't exist, a new one is set up by 
    !        writing a header to it, and polar dumping is enabled.
    !--------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    CHARACTER*128 PROMPT
    !
    CHARACTER*32 NAMEX
    REAL MACHX
    REAL ACRITX(ISX)
    INTEGER RETYPX, MATYPX
    LOGICAL NAMDIF
    !
    CALL STRIP(PFNAMX(IP), NPF)
    IF(NPF == 0) THEN
        PROMPT = 'Enter  polar dump filename'&
                // '  OR  <return> for no file^'
    ELSE
        WRITE(*, *) 'Default polar dump filename:  ', PFNAMX(IP)(1:NPF)
        PROMPT = 'Enter  new filename'&
                // '  OR  "none"'&
                // '  OR  <return> for default^'
    ENDIF
    !
    CALL ASKS(PROMPT, FNAME)
    CALL STRIP(FNAME, NFN)
    !
    IF(INDEX('NONEnone', FNAME(1:4)) /= 0) NFN = 0
    !
    IF(NFN == 0) THEN
        LPFILX = .FALSE.
        WRITE(*, *)
        WRITE(*, *) 'Polar dump file will NOT be written'
        RETURN
    ENDIF
    !
    !---- no valid dump file yet
    LPFILX = .FALSE.
    !
    !---- try reading the unformatted polar dump file to see if it exists
    OPEN(LU, FILE = FNAME, &
            STATUS = 'UNKNOWN', FORM = 'UNFORMATTED', ERR = 80)
    READ(LU, ERR = 90, END = 60) NAMEX
    !
    !---- if we got to here, it exists, so read the header
    READ(LU) MACHX, REYNX, ACRITX(1), ACRITX(2)
    READ(LU) MATYPX, RETYPX
    READ(LU) IIX, ILEX, ITEX, IIBX
    !
    REYNX = REYNX * 1.0E6
    !
    !---- set polar dump file pointer at the end
    45 READ(LU, END = 46) DUMMY
    GO TO 45
    !
    46 CLOSE(LU)
    PFNAMX(IP) = FNAME
    !
    CALL STRIP(NAMEX, NNAMEX)
    !
    !---- check to see if the names are different
    IF(NNAME /= NNAMEX) THEN
        NAMDIF = .TRUE.
    ELSE
        NAMDIF = .FALSE.
        DO K = 1, NNAME
            IF(NAME(K:K) /= NAMEX(K:K)) NAMDIF = .TRUE.
        end do
    ENDIF
    !
    !---- check if the polar save file is for the same airfoil and conditions
    IF(NAMDIF               .OR.&
            REYNX /= REINF1   .OR.&
            MACHX /= MINF1    .OR.&
            RETYPX /= RETYP    .OR.&
            MATYPX /= MATYP    .OR.&
            ACRITX(1) /= ACRIT(1) .OR.&
            ACRITX(2) /= ACRIT(2)) THEN
        !
        WRITE(*, 6600) NAMEX, NAME, &
                REYNX, REINF1, &
                MACHX, MINF1, &
                RETYPX, RETYP, &
                MATYPX, MATYP, &
                ACRITX(1), ACRIT(1), &
                ACRITX(1), ACRIT(2)
        !
        6600  FORMAT(&
                /'               Dump file                       Current'&
                /'             ------------                    ------------'&
                /' name  :   ', A, A&
                /' Re    :   ', F12.0, 20X, F12.0&
                /' Mach  :   ', F12.4, 20X, F12.4&
                /' Retyp :   ', I7, 25X, I7&
                /' Matyp :   ', I7, 25X, I7&
                /' NcritT:   ', F12.4, 20X, F12.4&
                /' NcritB:   ', F12.4, 20X, F12.4)
        !
        WRITE(*, *)
        WRITE(*, *)&
                'Current parameters different from old dump file values.'
        CALL ASKL&
                ('Set current parameters to old dump file values ?^', OK)
        !
        IF(OK) THEN
            NAME = NAMEX
            NNAME = NNAMEX
            MINF1 = MACHX
            REINF1 = REYNX
            RETYP = RETYPX
            MATYP = MATYPX
            ACRIT(1) = ACRITX(1)
            ACRIT(2) = ACRITX(2)
        ELSE
            WRITE(*, *)
            WRITE(*, *) 'Old polar dump file NOT available for appending'
            RETURN
        ENDIF
    ENDIF
    !
    !---- enable writing to the save file
    LPFILX = .TRUE.
    WRITE(*, *)
    WRITE(*, *) 'Old polar dump file available for appending'
    RETURN
    !
    !
    !---- the polar dump file doesn't exist, so write new header
    60 CONTINUE
    WRITE(LU) NAME, 'XFOIL   ', VERSION
    WRITE(LU) MINF1, REINF1 / 1.0E6, ACRIT(1), ACRIT(2)
    WRITE(LU) MATYP, RETYP
    WRITE(LU) 0, 0, 0, N
    WRITE(LU) (X(I), Y(I), I = 1, N)
    !
    70 CONTINUE
    !
    CLOSE(LU)
    PFNAMX(IP) = FNAME
    !
    !---- enable writing to the save file
    LPFILX = .TRUE.
    WRITE(*, *)
    WRITE(*, *) 'New polar dump file available'
    RETURN
    !
    !---- OPEN error trap
    80 WRITE(*, 1080) FNAME
    RETURN
    !
    !---- READ error trap
    90 WRITE(*, *) 'Polar dump file READ error'
    CLOSE(LU)
    RETURN
    !..........................................
    1080 FORMAT(' OPEN error on polar dump file ', A48)
END
! PLXINI



SUBROUTINE PLRADD(LU, IP)
    INCLUDE 'XFOIL.INC'
    LOGICAL ERROR
    !
    !c      WRITE(*,1000) CL, CD, CM
    !c 1000 FORMAT(/' CL =', F7.3, '    Cd =', F9.5, '    Cm =', F8.4)
    !
    !---- add point to storage arrays
    IF(IP == 0) THEN
        WRITE(*, *) 'No active polar is declared. Point not stored.'
        !
    ELSE
        IF(NAPOL(IP) == NAX) THEN
            WRITE(*, *) 'Polar storage arrays full. Point not stored'
            !
        ELSE
            NAPOL(IP) = NAPOL(IP) + 1
            !
            !------ store current point
            IF(LVISC) THEN
                CDTOT = CD
                CDV = CD
                RE = REINF
            ELSE
                CDTOT = 0.
                CDV = 0.
                RE = 0.
            ENDIF
            !
            IA = NAPOL(IP)
            CPOL(IA, IAL, IP) = ADEG
            CPOL(IA, ICL, IP) = CL
            CPOL(IA, ICD, IP) = CDTOT
            CPOL(IA, ICM, IP) = CM
            CPOL(IA, ICP, IP) = CDP
            CPOL(IA, ICV, IP) = CDV
            CPOL(IA, IMA, IP) = MINF
            CPOL(IA, IRE, IP) = RE
            DO IS = 1, 2
                IF(LVISC) THEN
                    XOCT = XOCTR(IS)
                ELSE
                    XOCT = 0.
                ENDIF
                CPOLSD(IA, IS, JNC, IP) = ACRIT(IS)
                CPOLSD(IA, IS, JTP, IP) = XSTRIP(IS)
                CPOLSD(IA, IS, JTN, IP) = XOCT
                CPOLSD(IA, IS, JTI, IP) = TINDEX(IS)
            ENDDO
            !
            IF(LFLAP) THEN
                CALL MHINGE
                CPOL(IA, ICH, IP) = HMOM
            ELSE
                CPOL(IA, ICH, IP) = 0.
            ENDIF
            CPOL(IA, IMC, IP) = CPMN
            !
            WRITE(*, 1100) IP
            1100   FORMAT(/' Point added to stored polar', I3)
        ENDIF
    ENDIF
    !
    !---- add point to save file
    IF(LPFILE) THEN
        NIPOL = NIPOL0
        IF(LCMINP) THEN
            NIPOL = NIPOL + 1
            IPOL(NIPOL) = IMC
        ENDIF
        IF(LHMOMP) THEN
            NIPOL = NIPOL + 1
            IPOL(NIPOL) = ICH
        ENDIF
        !
        OPEN(LU, FILE = PFNAME(IP), STATUS = 'OLD')
        CALL BOTTOM(LU)
        IA = NAPOL(IP)
        CALL POLWRIT(LU, ' ', ERROR, .FALSE., &
                NAX, IA, IA, CPOL(1, 1, IP), IPOL, NIPOL, &
                REYNP1(IP), MACHP1(IP), ACRITP(1, IP), XSTRIPP(1, IP), &
                PTRATP(IP), ETAPP(IP), &
                NAMEPOL(IP), IRETYP(IP), IMATYP(IP), &
                ISX, 1, CPOLSD(1, 1, 1, IP), JPOL, NJPOL, &
                'XFOIL', VERSION, .FALSE.)
        CLOSE(LU)
        NIPOL = NIPOL0
        WRITE(*, 1200) PFNAME(IP)
        1200  FORMAT(' Point written to save file  ', A48)
    ELSE
        WRITE(*, 1300)
        1300  FORMAT(' Save file unspecified or not available')
    ENDIF
    !
    !ccC---- sort polar in increasing alpha
    !cc      IDSORT = IAL
    !cc      CALL PLRSRT(IP,IDSORT)
    !
    RETURN
END
! PLRADD


SUBROUTINE PLXADD(LU, IP)
    INCLUDE 'XFOIL.INC'
    INTEGER NSIDE(2)
    !
    DIMENSION XX(IVX, 2), CP(IVX, 2), CF(IVX, 2)
    !
    IF(.NOT.LPFILX) THEN
        WRITE(*, 1050)
        1050  FORMAT(' Dump file unspecified or not available')
        RETURN
    ENDIF
    !
    BETA = SQRT(1.0 - MINF**2)
    BFAC = 0.5 * MINF**2 / (1.0 + BETA)
    !
    OPEN(LU, FILE = PFNAMX(IP), STATUS = 'OLD', FORM = 'UNFORMATTED')
    CALL BOTTOMX(LU)
    !
    !---- write integrated forces to unformatted dump file
    IF(LVISC) THEN
        CDTOT = CD
        XT1 = XOCTR(1)
        XT2 = XOCTR(2)
    ELSE
        CDTOT = 0.
        XT1 = 0.
        XT2 = 0.
    ENDIF
    WRITE(LU) ALFA / DTOR, CL, CDTOT, 0.0, CM, XT1, XT2
    !
    NSIDE(1) = IBLTE(1) + (NBL(2) - IBLTE(2))
    NSIDE(2) = NBL(2)
    !
    NSIDE(1) = MAX(NSIDE(1), 2)
    NSIDE(2) = MAX(NSIDE(2), 2)
    !
    !---- write indexing info
    WRITE(LU) NSIDE(1), NSIDE(2), IBLTE(1), IBLTE(2)
    !
    QUE = 0.5 * QINF**2
    !
    !---- set stagnation point quantities
    IBL = 1
    XX(IBL, 1) = SEVAL(SST, X, XP, S, N)
    CP(IBL, 1) = 1.0 / (BETA + BFAC)
    CF(IBL, 1) = 0.0
    THET(IBL, 1) = 0.5 * (THET(2, 1) + THET(2, 2))
    DSTR(IBL, 1) = 0.5 * (DSTR(2, 1) + DSTR(2, 2))
    CTAU(IBL, 1) = 0.0
    !
    XX(IBL, 2) = XX(IBL, 1)
    CP(IBL, 2) = CP(IBL, 1)
    CF(IBL, 2) = CF(IBL, 1)
    THET(IBL, 2) = THET(IBL, 1)
    DSTR(IBL, 2) = DSTR(IBL, 1)
    CTAU(IBL, 2) = CTAU(IBL, 1)
    !
    !---- set BL and wake quantities
    DO IS = 1, 2
        DO IBL = 2, NSIDE(IS)
            I = IPAN(IBL, IS)
            XX(IBL, IS) = X(I)
            CP(IBL, IS) = CPV(I)
            CF(IBL, IS) = TAU(IBL, IS) / QUE
        ENDDO
    end do
    !
    DO IS = 1, 2
        WRITE(LU) (XX(IBL, IS), CP(IBL, IS), THET(IBL, IS), DSTR(IBL, IS), &
                CF(IBL, IS), CTAU(IBL, IS), IBL = 1, NSIDE(IS))
    ENDDO
    !
    CLOSE(LU)
    WRITE(*, 1100) PFNAMX(IP)
    1100 FORMAT(' Point written to dump file ', A48)
    RETURN
    !
END
! PLXADD



SUBROUTINE PLRSRT(IP, IDSORT)
    INCLUDE 'XFOIL.INC'
    DIMENSION INDX(NAX), ATMP(NAX)
    !
    !---- sort polar in increasing variable IDSORT
    CALL HSORT(NAPOL(IP), CPOL(1, IDSORT, IP), INDX)
    !
    !---- do the actual reordering
    DO ID = 1, IPTOT
        CALL ASORT(NAPOL(IP), CPOL(1, ID, IP), INDX, ATMP)
    ENDDO
    DO ID = 1, JPTOT
        DO IS = 1, 2
            CALL ASORT(NAPOL(IP), CPOLSD(1, IS, ID, IP), INDX, ATMP)
        ENDDO
    ENDDO
    !
    RETURN
END
! PLRSRT



SUBROUTINE PLRSUM(IP1, IP2, IPACTT)
    !---------------------------------------------
    !     Prints summary of polars IP1..IP2
    !---------------------------------------------
    INCLUDE 'XFOIL.INC'
    CHARACTER*5 CLTYP(3)
    CHARACTER*1 CACC, CFIL
    !
    DATA CLTYP / '     ', '/sqCL', '/CL  ' /
    !
    1100 FORMAT(1X, A, A)
    WRITE(*, *)
    WRITE(*, 1100)&
            '       airfoil                    Re           Mach     ', &
            '  NcritT  NcritB  XtripT  XtripB       file'
    WRITE(*, 1100)&
            '      ------------------------  ------------  ----------', &
            '  ------  ------  ------  ------    -------------------'
    !CC     >  10  NACA 0012 (mod)           1.232e6/sqCL  0.781/sqCL
    !CC         9.00    9.00   1.000   1.000
    !CC     1234567890123456789012345678901234567890123456789012345678901234567890
    !
    DO IP = IP1, IP2
        IF(IP == IPACTT) THEN
            CACC = '>'
            IF(LPFILE) THEN
                CFIL = '>'
            ELSE
                CFIL = ' '
            ENDIF
        ELSE
            CACC = ' '
            CFIL = ' '
        ENDIF
        !
        IRET = IRETYP(IP)
        IMAT = IMATYP(IP)
        !
        IF(REYNP1(IP) > 0.0) THEN
            IEXP = INT(LOG10(REYNP1(IP)))
            IEXP = MAX(MIN(IEXP, 9), 0)
            RMAN = REYNP1(IP) / 10.0**IEXP
        ELSE
            RMAN = 0.0
        ENDIF
        !
        CALL STRIP(PFNAME(IP), NPF)
        WRITE(*, 1200) CACC, IP, NAMEPOL(IP), &
                RMAN, IEXP, CLTYP(IRET), MACHP1(IP), CLTYP(IMAT), &
                ACRITP(1, IP), ACRITP(2, IP), &
                XSTRIPP(1, IP), XSTRIPP(2, IP), &
                CFIL, PFNAME(IP)(1:NPF)
        1200   FORMAT(1X, A1, I3, 2X, A24, F7.3, 'e', I1, A5, F7.3, A5, &
                2F8.2, 2F8.3, 2X, A1, 1X, A)
    ENDDO
    !
    RETURN
END
! PLRSUM



SUBROUTINE PRFSUM(IR1, IR2)
    !---------------------------------------------
    !     Prints summary of reference polars IR1..IR2
    !---------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    1100 FORMAT(1X, A, A)
    WRITE(*, *)
    WRITE(*, 1100) '       reference polar                          '
    WRITE(*, 1100) '      ------------------------------------------'
    !CC                  123456789012345678901234567890123456789012345678
    !
    DO IR = IR1, IR2
        WRITE(*, 1200) IR, NAMEREF(IR)
        1200   FORMAT(1X, 1X, I3, 2X, A48)
    ENDDO
    !
    RETURN
END
! PRFSUM



SUBROUTINE PLRCOP(IP1, IP2)
    !---------------------------------------------
    !     Copies polar in slot IP1 into slot IP2
    !---------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    NAMEPOL(IP2) = NAMEPOL(IP1)
    CODEPOL(IP2) = CODEPOL(IP1)
    VERSPOL(IP2) = VERSPOL(IP1)
    PFNAME(IP2) = PFNAME(IP1)
    PFNAMX(IP2) = PFNAMX(IP1)
    !
    MACHP1(IP2) = MACHP1(IP1)
    REYNP1(IP2) = REYNP1(IP1)
    !
    IMATYP(IP2) = IMATYP(IP1)
    IRETYP(IP2) = IRETYP(IP1)

    ACRITP(1, IP2) = ACRITP(1, IP1)
    ACRITP(2, IP2) = ACRITP(2, IP1)
    !
    XSTRIPP(1, IP2) = XSTRIPP(1, IP1)
    XSTRIPP(2, IP2) = XSTRIPP(2, IP1)
    !
    NAPOL(IP2) = NAPOL(IP1)
    DO IA = 1, NAPOL(IP2)
        DO ID = 1, IPTOT
            CPOL(IA, ID, IP2) = CPOL(IA, ID, IP1)
        ENDDO
        DO ID = 1, JPTOT
            CPOLSD(IA, 1, ID, IP2) = CPOLSD(IA, 1, ID, IP1)
            CPOLSD(IA, 2, ID, IP2) = CPOLSD(IA, 2, ID, IP1)
        ENDDO
    ENDDO
    !
    NXYPOL(IP2) = NXYPOL(IP1)
    DO I = 1, NXYPOL(IP1)
        CPOLXY(I, 1, IP2) = CPOLXY(I, 1, IP1)
        CPOLXY(I, 2, IP2) = CPOLXY(I, 2, IP1)
    ENDDO
    !
    RETURN
END
! PLRCOP




SUBROUTINE PRFCOP(IR1, IR2)
    !---------------------------------------------
    !     Copies reference polar in slot IR1 into slot IR2
    !---------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    NAMEREF(IR2) = NAMEREF(IR1)
    !
    DO K = 1, 4
        NDREF(K, IR2) = NDREF(K, IR1)
    ENDDO
    !
    DO IS = 1, 2
        DO K = 1, 4
            DO IA = 1, NDREF(K, IR2)
                CPOLREF(IA, IS, K, IR2) = CPOLREF(IA, IS, K, IR1)
            ENDDO
        ENDDO
    ENDDO
    !
    RETURN
END
! PRFCOP


SUBROUTINE POLAXI(CPOLPLF, XCDWID, XALWID, XOCWID)
    !-------------------------------------------
    !     Gets polar plot axis limits from user
    !-------------------------------------------
    INCLUDE 'PINDEX.INC'
    DIMENSION CPOLPLF(3, *)
    !
    LOGICAL ERROR
    CHARACTER*5 CVAR(4)
    DATA CVAR / 'Alpha', '  CL ', '  CD ', ' -CM ' /
    !
    WRITE(*, *) 'Enter new axis annotations,', &
            ' or <return> to leave unchanged...'
    WRITE(*, *)
    !
    DO KV = 1, 4
        5      WRITE(*, 1200) CVAR(KV), (CPOLPLF(J, KV), J = 1, 3)
        1200   FORMAT(3X, A, '  min, max, delta:', 3F11.5)
        CALL READR(3, CPOLPLF(1, KV), ERROR)
        IF(ERROR) THEN
            WRITE(*, *) 'READ error.  Enter again.'
            GO TO 5
        ENDIF
    ENDDO
    !
    !C---- widths of plot boxes in polar plot page
    !      XCDWID = 0.45
    !      XALWID = 0.25
    !      XOCWID = 0.20
    !
    RETURN
END
! POLAXI



SUBROUTINE BOTTOM(LU)
    CHARACTER*1 DUMMY
    !
    10   READ(LU, 1000, END = 90, ERR = 90) DUMMY
    1000 FORMAT(A)
    GO TO 10
    !
    90   RETURN
END


SUBROUTINE BOTTOMX(LU)
    CHARACTER*1 DUMMY
    !
    10   READ(LU, END = 90, ERR = 90) DUMMY
    GO TO 10
    !
    90   RETURN
END

