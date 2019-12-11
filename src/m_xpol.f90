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

!*==PLRSET.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module m_xpol
contains
    subroutine plrset(Ip)
        use m_userio, only: aski, strip
        use m_iopol, only: polwrit
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ip
        intent (inout) Ip
        !
        ! Local variables
        !
        real :: dsfrac, dsq, sizref
        logical :: error
        integer :: i, iopt
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
        !--------------------------------------------------------------
        !     Selects slot IP for saving polar.
        !     Resets all parameters if necessary.
        !--------------------------------------------------------------
        !
        if (Ip<=0) then
            !----- invalid polar index
            return
            !
        elseif (Ip>=1 .and. Ip<=NPOl) then
            if (show_output) then
                write (*, *)
                write (*, *) 'Existing stored polar is chosen for appending...'
            endif
            NIPol = NIPol0
            if (LCMinp) then
                NIPol = NIPol + 1
                IPOl(NIPol) = IMC
            endif
            if (LHMomp) then
                NIPol = NIPol + 1
                IPOl(NIPol) = ICH
            endif
            call polwrit(6, ' ', error, .true., NAX, 1, NAPol(Ip), &
                    CPOl(1, 1, Ip), IPOl, NIPol, REYnp1(Ip), MAChp1(Ip), ACRitp(1, Ip), &
                    & XSTripp(1, Ip), PTRatp(Ip), ETApp(Ip), &
                    NAMepol(Ip), IREtyp(Ip), IMAtyp(Ip), ISX, 1, CPOlsd(1, 1, 1, Ip), JPOl, NJPol, &
                    &'XFOIL', VERsion, .false.)
            NIPol = NIPol0
            !
            !----- check if geometries differ...
            if (N==NXYpol(Ip)) then
                sizref = S(N) - S(1)
                do i = 1, N
                    dsq = (X(i) - CPOlxy(i, 1, Ip))**2 + (Y(i) - CPOlxy(i, 2, Ip))**2
                    dsfrac = sqrt(dsq) / sizref
                    if (dsfrac>0.00001) goto 50
                enddo
                goto 100
            endif
            !
            50   if (show_output) write (*, *) 'Current airfoil differs from airfoil of stored polar'
            if (show_output) write (*, 99001)
            99001 format (&
                    /'   - - - - - - - - - - - - - - - - - - - - - - - - - - - -'&
                    /'    0  abort polar accumulation'&
                    /'    1  compute with current airfoil'&
                    /'    2  compute with stored  airfoil', &
                    ' (overwrite current airfoil)')
            call aski('   Select action^', iopt)
            if (iopt==0) then
                Ip = 0
                return
            elseif (iopt==2) then
                call apcopy(Ip)
            endif
            !
            100  if (show_output) write (*, *)
            if (show_output) write (*, *) 'Setting current parameters to those of stored polar'
            !
            NAMe = NAMepol(Ip)
            call strip(NAMe, NNAme)
            !
            RETyp = IREtyp(Ip)
            MATyp = IMAtyp(Ip)
            !
            MINf1 = MAChp1(Ip)
            REInf1 = REYnp1(Ip)
            !
            ACRit(1) = ACRitp(1, Ip)
            ACRit(2) = ACRitp(2, Ip)
            !
            XSTrip(1) = XSTripp(1, Ip)
            XSTrip(2) = XSTripp(2, Ip)
            !
        else
            !----- new polar slot is chosen
            NPOl = Ip
            !
            NAPol(Ip) = 0
            !
            NAMepol(Ip) = NAMe
            IREtyp(Ip) = RETyp
            IMAtyp(Ip) = MATyp
            !
            if (LVIsc) then
                REYnp1(Ip) = REInf1
            else
                REYnp1(Ip) = 0.
            endif
            MAChp1(Ip) = MINf1
            !
            ACRitp(1, Ip) = ACRit(1)
            ACRitp(2, Ip) = ACRit(2)
            !
            XSTripp(1, Ip) = XSTrip(1)
            XSTripp(2, Ip) = XSTrip(2)
            !
            NXYpol(Ip) = N
            do i = 1, N
                CPOlxy(i, 1, Ip) = X(i)
                CPOlxy(i, 2, Ip) = Y(i)
            enddo
            !
            if (show_output) write (*, 99002) Ip, NAMepol(Ip)
            99002 format (/' Polar', i3, ' newly created for accumulation'/' Airfoil archived with polar: ', a)
        endif
        !
    end subroutine plrset
    !*==APCOPY.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PLRSET


    subroutine apcopy(Ip)
        use s_xfoil, only: tecalc
        use m_xpanel, only: apcalc, ncalc
        use m_xgeom, only: lefind
        use m_spline, only: seval, segspl, scalc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ip
        intent (in) Ip
        !
        ! Local variables
        !
        integer :: i, nold
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
        nold = N

        N = NXYpol(Ip)
        do i = 1, N
            X(i) = CPOlxy(i, 1, Ip)
            Y(i) = CPOlxy(i, 2, Ip)
        enddo
        NAMe = NAMepol(Ip)
        !
        call scalc(X, Y, S, N)
        call segspl(X, XP, S, N)
        call segspl(Y, YP, S, N)
        call ncalc(X, Y, S, N, NX, NY)
        call lefind(SLE, X, XP, Y, YP, S, N)
        XLE = seval(SLE, X, XP, S, N)
        YLE = seval(SLE, Y, YP, S, N)
        XTE = 0.5 * (X(1) + X(N))
        YTE = 0.5 * (Y(1) + Y(N))
        CHOrd = sqrt((XTE - XLE)**2 + (YTE - YLE)**2)
        call tecalc
        call apcalc
        !
        LGAmu = .false.
        LQInu = .false.
        LWAke = .false.
        LQAij = .false.
        LADij = .false.
        LWDij = .false.
        LIPan = .false.
        LVConv = .false.
        LSCini = .false.
        if (nold/=N) LBLini = .false.
        !
    end subroutine apcopy
    !*==PLRINI.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! APCOPY



    subroutine plrini(Lu, Ip)
        use m_userio, only: asks, askl, strip
        use m_iopol, only: polread, polwrit
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ip, Lu
        intent (in) Ip
        !
        ! Local variables
        !
        logical :: error, namdif
        integer :: ia1, ia2, k, nfn, nnamep, npf
        integer, dimension(ISX, IPX) :: nblp
        character(128) :: prompt
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
        !
        !
        !
        !
        call strip(PFName(Ip), npf)
        if (npf==0) then
            prompt = 'Enter  polar save filename' // '  OR  <return> for no file^'
        else
            if (show_output) write (*, *) 'Default polar save filename:  ', PFName(Ip)(1:npf)
            prompt = 'Enter  new filename' // '  OR  "none"' // '  OR  <return> for default^'
        endif
        !
        call asks(prompt, FNAme)
        call strip(FNAme, nfn)
        !
        if (nfn==0) then
            FNAme = PFName(Ip)
            nfn = npf
        elseif (index('NONEnone', FNAme(1:4))/=0) then
            nfn = 0
        endif
        !
        if (nfn==0) then
            LPFile = .false.
            if (show_output) then
                write (*, *)
                write (*, *) 'Polar save file will NOT be written'
            endif
            return
        endif
        !
        !---- no valid file yet
        LPFile = .false.
        !
        !---- try reading the polar file to see if it exists
        open (Lu, file = FNAme, status = 'OLD', err = 100)
        call polread(Lu, ' ', error, NAX, NAPol(Ip), &
                CPOl(1, 1, Ip), REYnp1(Ip), MAChp1(Ip), ACRitp(1, Ip), XSTripp(1, Ip), PTRatp(Ip), &
                & ETApp(Ip), NAMepol(Ip), IREtyp(Ip), IMAtyp(Ip), &
                ISX, nblp(1, Ip), CPOlsd(1, 1, 1, Ip), CODepol(Ip), VERspol(Ip))
        if (error) then
            !
            !---- READ error trap
            if (show_output) write (*, *) 'Old polar save file READ error'
            close (Lu)
            goto 99999
        else
            close (Lu)
            PFName(Ip) = FNAme
            !
            call strip(NAMepol(Ip), nnamep)
            !
            !---- check to see if the names are different
            if (NNAme/=nnamep) then
                namdif = .true.
            else
                namdif = .false.
                do k = 1, NNAme
                    if (NAMe(k:k)/=NAMepol(Ip)(k:k)) namdif = .true.
                enddo
            endif
            !
            !---- check if the polar save file is for the same airfoil and conditions
            if (namdif .or. REYnp1(Ip)/=REInf1 .or. MAChp1(Ip)/=MINf1 .or. &
                    & IREtyp(Ip)/=RETyp .or. IMAtyp(Ip)/=MATyp .or. &
                    & ACRitp(1, Ip)/=ACRit(1) .or. ACRitp(2, Ip)/=ACRit(2) .or.&
                    & XSTripp(1, Ip)/=XSTrip(1) .or. XSTripp(2, Ip)/=XSTrip(2))&
                    & then
                !
                if (show_output) then
                    write (*, 99001) NAMe, NAMepol(Ip), REInf1, REYnp1(Ip), MINf1, MAChp1(Ip), RETyp, IREtyp(Ip), MATyp, &
                            & IMAtyp(Ip), ACRit(1), ACRitp(1, Ip), ACRit(2), ACRitp(2, Ip), XSTrip(1), XSTripp(1, Ip), &
                            & XSTrip(2), XSTripp(2, Ip)
                    99001  format (&
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
                end if
                !
                !
                if (show_output) then
                    write (*, *)
                    write (*, *) 'Current parameters different from old save file values.'
                endif
                call askl('Set current parameters to old save file values ?^', OK)
                !
                if (OK) then
                    NAMe = NAMepol(Ip)
                    NNAme = nnamep
                    REInf1 = REYnp1(Ip)
                    MINf1 = MAChp1(Ip)
                    RETyp = IREtyp(Ip)
                    MATyp = IMAtyp(Ip)
                    ACRit(1) = ACRitp(1, Ip)
                    ACRit(2) = ACRitp(2, Ip)
                    XSTrip(1) = XSTripp(1, Ip)
                    XSTrip(2) = XSTripp(2, Ip)
                else
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'Old polar save file NOT available for appending'
                    endif
                    return
                endif
            endif
            !
            !---- display polar save file just read in
            if (show_output) then
                write (*, *)
                write (*, *) 'Old polar save file read in ...'
            endif
            call polwrit(6, ' ', error, .true., NAX, 1, NAPol(Ip), &
                    CPOl(1, 1, Ip), IPOl, NIPol, REYnp1(Ip), MAChp1(Ip), ACRitp(1, Ip), &
                    & XSTripp(1, Ip), PTRatp(Ip), ETApp(Ip), &
                    NAMepol(Ip), IREtyp(Ip), IMAtyp(Ip), ISX, 1, CPOlsd(1, 1, 1, Ip), JPOl, NJPol, &
                    & CODepol(Ip), VERspol(Ip), .false.)
            !
            !---- enable writing to the save file
            LPFile = .true.
            if (show_output) then
                write (*, *)
                write (*, *) 'Old polar save file available for appending'
            endif
            return
        endif
        !
        !
        !---- the polar save file doesn't exist, so write new header
        100  NIPol = NIPol0
        if (LCMinp) then
            NIPol = NIPol + 1
            IPOl(NIPol) = IMC
        endif
        if (LHMomp) then
            NIPol = NIPol + 1
            IPOl(NIPol) = ICH
        endif
        !
        open (Lu, file = FNAme, status = 'NEW', err = 200)
        ia1 = 0
        ia2 = -1
        call polwrit(Lu, ' ', error, .true., NAX, ia1, ia2, &
                CPOl(1, 1, Ip), IPOl, NIPol, REYnp1(Ip), MAChp1(Ip), ACRitp(1, Ip), XSTripp(1, Ip), &
                & PTRatp(Ip), ETApp(Ip), &
                NAMepol(Ip), IREtyp(Ip), IMAtyp(Ip), ISX, 1, CPOlsd(1, 1, 1, Ip), JPOl, NJPol, 'XFOIL', VERsion, &
                & .false.)
        close (Lu)
        PFName(Ip) = FNAme
        !
        NIPol = NIPol0
        !
        !---- enable writing to the save file
        LPFile = .true.
        if (show_output) then
            write (*, *)
            write (*, *) 'New polar save file available'
        endif
        return
        !
        !---- the polar save file doesn't exist, so write new header
        200  if (show_output) write (*, *) 'New polar save file OPEN error'
        !
        !..........................................
    99999 end subroutine plrini
    !*==PLXINI.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PLRINI



    subroutine plxini(Lu, Ip)
        use m_userio, only: asks, askl, strip
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ip, Lu
        intent (in) Ip, Lu
        !
        ! Local variables
        !
        real, dimension(ISX) :: acritx
        real :: dummy, machx, reynx
        integer :: i, iibx, iix, ilex, itex, k, matypx, nfn, nnamex, npf, retypx
        logical :: namdif
        character(32) :: namex
        character(128) :: prompt
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
        !
        !
        call strip(PFNamx(Ip), npf)
        if (npf==0) then
            prompt = 'Enter  polar dump filename' // '  OR  <return> for no file^'
        else
            if (show_output) write (*, *) 'Default polar dump filename:  ', PFNamx(Ip)(1:npf)
            prompt = 'Enter  new filename' // '  OR  "none"' // '  OR  <return> for default^'
        endif
        !
        call asks(prompt, FNAme)
        call strip(FNAme, nfn)
        !
        if (index('NONEnone', FNAme(1:4))/=0) nfn = 0
        !
        if (nfn==0) then
            LPFilx = .false.
            if (show_output) then
                write (*, *)
                write (*, *) 'Polar dump file will NOT be written'
            endif
            return
        endif
        !
        !---- no valid dump file yet
        LPFilx = .false.
        !
        !---- try reading the unformatted polar dump file to see if it exists
        open (Lu, file = FNAme, status = 'UNKNOWN', form = 'UNFORMATTED', err = 300)
        read (Lu, err = 400, end = 200) namex
        !
        !---- if we got to here, it exists, so read the header
        read (Lu) machx, reynx, acritx(1), acritx(2)
        read (Lu) matypx, retypx
        read (Lu) iix, ilex, itex, iibx
        !
        reynx = reynx * 1.0E6
        do
            !
            !---- set polar dump file pointer at the end
            read (Lu, end = 100) dummy
        enddo
        !
        100  close (Lu)
        PFNamx(Ip) = FNAme
        !
        call strip(namex, nnamex)
        !
        !---- check to see if the names are different
        if (NNAme/=nnamex) then
            namdif = .true.
        else
            namdif = .false.
            do k = 1, NNAme
                if (NAMe(k:k)/=namex(k:k)) namdif = .true.
            enddo
        endif
        !
        !---- check if the polar save file is for the same airfoil and conditions
        if (namdif .or. reynx/=REInf1 .or. machx/=MINf1 .or. retypx/=RETyp .or. matypx/=MATyp .or. acritx(1)/=ACRit(1) .or.&
                & acritx(2)/=ACRit(2)) then
            !
            if (show_output) write (*, 99001) namex, NAMe, reynx, REInf1, machx, MINf1, retypx, RETyp, matypx, MATyp, acritx(1), &
                    & ACRit(1), acritx(1), ACRit(2)
            !
            99001 format (&
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
            if (show_output) then
                write (*, *)
                write (*, *) 'Current parameters different from old dump file values.'
            endif
            call askl('Set current parameters to old dump file values ?^', OK)
            !
            if (OK) then
                NAMe = namex
                NNAme = nnamex
                MINf1 = machx
                REInf1 = reynx
                RETyp = retypx
                MATyp = matypx
                ACRit(1) = acritx(1)
                ACRit(2) = acritx(2)
            else
                if (show_output) then
                    write (*, *)
                    write (*, *) 'Old polar dump file NOT available for appending'
                endif
                return
            endif
        endif
        !
        !---- enable writing to the save file
        LPFilx = .true.
        if (show_output) then
            write (*, *)
            write (*, *) 'Old polar dump file available for appending'
        endif
        return
        !
        !
        !---- the polar dump file doesn't exist, so write new header
        200  write (Lu) NAMe, 'XFOIL   ', VERsion
        write (Lu) MINf1, REInf1 / 1.0E6, ACRit(1), ACRit(2)
        write (Lu) MATyp, RETyp
        write (Lu) 0, 0, 0, N
        write (Lu) (X(i), Y(i), i = 1, N)
        !
        !
        close (Lu)
        PFNamx(Ip) = FNAme
        !
        !---- enable writing to the save file
        LPFilx = .true.
        if (show_output) then
            write (*, *)
            write (*, *) 'New polar dump file available'
        endif
        return
        !
        !---- OPEN error trap
        300  if (show_output) write (*, 99002) FNAme
        !..........................................
        99002 format (' OPEN error on polar dump file ', a48)
        return
        !
        !---- READ error trap
        400  if (show_output) write (*, *) 'Polar dump file READ error'
        close (Lu)
    end subroutine plxini
    !*==PLRADD.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PLXINI



    subroutine plradd(Lu, Ip)
        use m_iopol, only: polwrit
        use i_xfoil
        use s_xoper, only: mhinge
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ip, Lu
        intent (in) Ip
        !
        ! Local variables
        !
        real :: cdtot, cdv, re, xoct
        logical :: error
        integer :: ia, is
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
        !c      WRITE(*,1000) CL, CD, CM
        !c 1000 FORMAT(/' CL =', F7.3, '    Cd =', F9.5, '    Cm =', F8.4)
        !
        !---- add point to storage arrays
        if (Ip==0) then
            if (show_output) write (*, *) 'No active polar is declared. Point not stored.'
            !
        elseif (NAPol(Ip)==NAX) then
            if (show_output) write (*, *) 'Polar storage arrays full. Point not stored'
            !
        else
            NAPol(Ip) = NAPol(Ip) + 1
            !
            !------ store current point
            if (LVIsc) then
                cdtot = CD
                cdv = CD
                re = REInf
            else
                cdtot = 0.
                cdv = 0.
                re = 0.
            endif
            !
            ia = NAPol(Ip)
            CPOl(ia, IAL, Ip) = ADEg
            CPOl(ia, ICL, Ip) = CL
            CPOl(ia, ICD, Ip) = cdtot
            CPOl(ia, ICM, Ip) = CM
            CPOl(ia, ICP, Ip) = CDP
            CPOl(ia, ICV, Ip) = cdv
            CPOl(ia, IMA, Ip) = MINf
            CPOl(ia, IRE, Ip) = re
            do is = 1, 2
                if (LVIsc) then
                    xoct = XOCtr(is)
                else
                    xoct = 0.
                endif
                CPOlsd(ia, is, JNC, Ip) = ACRit(is)
                CPOlsd(ia, is, JTP, Ip) = XSTrip(is)
                CPOlsd(ia, is, JTN, Ip) = xoct
                CPOlsd(ia, is, JTI, Ip) = TINdex(is)
            enddo
            !
            if (LFLap) then
                call mhinge
                CPOl(ia, ICH, Ip) = HMOm
            else
                CPOl(ia, ICH, Ip) = 0.
            endif
            CPOl(ia, IMC, Ip) = CPMn
            !
            if (show_output) write (*, 99001) Ip
            99001 format (/' Point added to stored polar', i3)
        endif
        !
        !---- add point to save file
        if (LPFile) then
            NIPol = NIPol0
            if (LCMinp) then
                NIPol = NIPol + 1
                IPOl(NIPol) = IMC
            endif
            if (LHMomp) then
                NIPol = NIPol + 1
                IPOl(NIPol) = ICH
            endif
            !
            open (Lu, file = PFName(Ip), status = 'OLD')
            call bottom(Lu)
            ia = NAPol(Ip)
            call polwrit(Lu, ' ', error, .false., NAX, ia, ia, &
                    CPOl(1, 1, Ip), IPOl, NIPol, REYnp1(Ip), MAChp1(Ip), ACRitp(1, Ip), XSTripp(1, Ip), &
                    & PTRatp(Ip), ETApp(Ip), &
                    NAMepol(Ip), IREtyp(Ip), IMAtyp(Ip), ISX, 1, CPOlsd(1, 1, 1, Ip), JPOl, NJPol, 'XFOIL', &
                    & VERsion, .false.)
            close (Lu)
            NIPol = NIPol0
            if (show_output) write (*, 99002) PFName(Ip)
            99002 format (' Point written to save file  ', a48)
        else
            if (show_output) write (*, 99003)
            99003 format (' Save file unspecified or not available')
        endif
        !
        !ccC---- sort polar in increasing alpha
        !cc      IDSORT = IAL
        !cc      CALL PLRSRT(IP,IDSORT)
        !
    end subroutine plradd
    !*==PLXADD.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PLRADD


    subroutine plxadd(Lu, Ip)
        use m_spline, only: seval
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ip, Lu
        intent (in) Ip
        !
        ! Local variables
        !
        real :: beta, bfac, cdtot, que, xt1, xt2
        real, dimension(IVX, 2) :: cf, cp, xx
        integer :: i, ibl, is
        integer, dimension(2) :: nside
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
        !
        if (.not.LPFilx) then
            if (show_output) write (*, 99001)
            99001 format (' Dump file unspecified or not available')
            return
        endif
        !
        beta = sqrt(1.0 - MINf**2)
        bfac = 0.5 * MINf**2 / (1.0 + beta)
        !
        open (Lu, file = PFNamx(Ip), status = 'OLD', form = 'UNFORMATTED')
        call bottomx(Lu)
        !
        !---- write integrated forces to unformatted dump file
        if (LVIsc) then
            cdtot = CD
            xt1 = XOCtr(1)
            xt2 = XOCtr(2)
        else
            cdtot = 0.
            xt1 = 0.
            xt2 = 0.
        endif
        write (Lu) ALFa / DTOr, CL, cdtot, 0.0, CM, xt1, xt2
        !
        nside(1) = IBLte(1) + (NBL(2) - IBLte(2))
        nside(2) = NBL(2)
        !
        nside(1) = max(nside(1), 2)
        nside(2) = max(nside(2), 2)
        !
        !---- write indexing info
        write (Lu) nside(1), nside(2), IBLte(1), IBLte(2)
        !
        que = 0.5 * QINf**2
        !
        !---- set stagnation point quantities
        ibl = 1
        xx(ibl, 1) = seval(SST, X, XP, S, N)
        cp(ibl, 1) = 1.0 / (beta + bfac)
        cf(ibl, 1) = 0.0
        THEt(ibl, 1) = 0.5 * (THEt(2, 1) + THEt(2, 2))
        DSTr(ibl, 1) = 0.5 * (DSTr(2, 1) + DSTr(2, 2))
        CTAu(ibl, 1) = 0.0
        !
        xx(ibl, 2) = xx(ibl, 1)
        cp(ibl, 2) = cp(ibl, 1)
        cf(ibl, 2) = cf(ibl, 1)
        THEt(ibl, 2) = THEt(ibl, 1)
        DSTr(ibl, 2) = DSTr(ibl, 1)
        CTAu(ibl, 2) = CTAu(ibl, 1)
        !
        !---- set BL and wake quantities
        do is = 1, 2
            do ibl = 2, nside(is)
                i = IPAn(ibl, is)
                xx(ibl, is) = X(i)
                cp(ibl, is) = CPV(i)
                cf(ibl, is) = TAU(ibl, is) / que
            enddo
        enddo
        !
        do is = 1, 2
            write (Lu) (xx(ibl, is), cp(ibl, is), THEt(ibl, is), &
                    DSTr(ibl, is), cf(ibl, is), CTAu(ibl, is), ibl = 1, nside(is))
        enddo
        !
        close (Lu)
        if (show_output) write (*, 99002) PFNamx(Ip)
        99002 format (' Point written to dump file ', a48)
        !
    end subroutine plxadd
    !*==PLRSRT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PLXADD



    subroutine plrsrt(Ip, Idsort)
        use m_sort, only: hsort, asort
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Idsort, Ip
        intent (in) Idsort, Ip
        !
        ! Local variables
        !
        real, dimension(NAX) :: atmp
        integer :: id, is
        integer, dimension(NAX) :: indx
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
        !---- sort polar in increasing variable IDSORT
        call hsort(NAPol(Ip), CPOl(1, Idsort, Ip), indx)
        !
        !---- do the actual reordering
        do id = 1, IPTOT
            call asort(NAPol(Ip), CPOl(1, id, Ip), indx, atmp)
        enddo
        do id = 1, JPTOT
            do is = 1, 2
                call asort(NAPol(Ip), CPOlsd(1, is, id, Ip), indx, atmp)
            enddo
        enddo
        !
    end subroutine plrsrt
    !*==PLRSUM.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PLRSRT



    subroutine plrsum(Ip1, Ip2, Ipactt)
        use m_userio, only: strip
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ip1, Ip2, Ipactt
        intent (in) Ip1, Ip2, Ipactt
        !
        ! Local variables
        !
        character(1) :: cacc, cfil
        character(5), dimension(3), save :: cltyp
        integer :: iexp, imat, ip, iret, npf
        real :: rman
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
        !---------------------------------------------
        !     Prints summary of polars IP1..IP2
        !---------------------------------------------
        !
        data cltyp/'     ', '/sqCL', '/CL  '/
        if (show_output) then
            write (*, *)
            write (*, 99002) '       airfoil                    Re           Mach     ', &
                &'  NcritT  NcritB  XtripT  XtripB       file'
            write (*, 99002) '      ------------------------  ------------  ----------', &
                &'  ------  ------  ------  ------    -------------------'

        endif
        !CC     >  10  NACA 0012 (mod)           1.232e6/sqCL  0.781/sqCL
        !CC         9.00    9.00   1.000   1.000
        !CC     1234567890123456789012345678901234567890123456789012345678901234567890
        !
        do ip = Ip1, Ip2
            if (ip==Ipactt) then
                cacc = '>'
                if (LPFile) then
                    cfil = '>'
                else
                    cfil = ' '
                endif
            else
                cacc = ' '
                cfil = ' '
            endif
            !
            iret = IREtyp(ip)
            imat = IMAtyp(ip)
            !
            if (REYnp1(ip)>0.0) then
                iexp = int(log10(REYnp1(ip)))
                iexp = max(min(iexp, 9), 0)
                rman = REYnp1(ip) / 10.0**iexp
            else
                rman = 0.0
            endif
            !
            call strip(PFName(ip), npf)
            if (show_output) then
                write (*, 99001) cacc, ip, NAMepol(ip), rman, iexp, cltyp(iret), MAChp1(ip), cltyp(imat), ACRitp(1, ip), &
                        & ACRitp(2, ip), XSTripp(1, ip), XSTripp(2, ip), cfil, PFName(ip)(1:npf)
                99001 format (1x, a1, i3, 2x, a24, f7.3, 'e', i1, a5, f7.3, a5, 2F8.2, 2F8.3, 2x, a1, 1x, a)
            end if
        enddo
        !
        99002 format (1x, a, a)
        !
    end subroutine plrsum
    !*==PRFSUM.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PLRSUM



    subroutine prfsum(Ir1, Ir2)
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ir1, Ir2
        intent (in) Ir1, Ir2
        !
        ! Local variables
        !
        integer :: ir
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
        !---------------------------------------------
        !     Prints summary of reference polars IR1..IR2
        !---------------------------------------------
        if (show_output) then
            write (*, *)
            write (*, 99002) '       reference polar                          '
            write (*, 99002) '      ------------------------------------------'
        endif
        !CC                  123456789012345678901234567890123456789012345678
        !
        do ir = Ir1, Ir2
            if (show_output) write (*, 99001) ir, NAMeref(ir)
            99001 format (1x, 1x, i3, 2x, a48)
        enddo
        !
        99002 format (1x, a, a)
        !
    end subroutine prfsum
    !*==PLRCOP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PRFSUM



    subroutine plrcop(Ip1, Ip2)
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ip1, Ip2
        intent (in) Ip1, Ip2
        !
        ! Local variables
        !
        integer :: i, ia, id
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
        !---------------------------------------------
        !     Copies polar in slot IP1 into slot IP2
        !---------------------------------------------
        !
        NAMepol(Ip2) = NAMepol(Ip1)
        CODepol(Ip2) = CODepol(Ip1)
        VERspol(Ip2) = VERspol(Ip1)
        PFName(Ip2) = PFName(Ip1)
        PFNamx(Ip2) = PFNamx(Ip1)
        !
        MAChp1(Ip2) = MAChp1(Ip1)
        REYnp1(Ip2) = REYnp1(Ip1)
        !
        IMAtyp(Ip2) = IMAtyp(Ip1)
        IREtyp(Ip2) = IREtyp(Ip1)

        ACRitp(1, Ip2) = ACRitp(1, Ip1)
        ACRitp(2, Ip2) = ACRitp(2, Ip1)
        !
        XSTripp(1, Ip2) = XSTripp(1, Ip1)
        XSTripp(2, Ip2) = XSTripp(2, Ip1)
        !
        NAPol(Ip2) = NAPol(Ip1)
        do ia = 1, NAPol(Ip2)
            do id = 1, IPTOT
                CPOl(ia, id, Ip2) = CPOl(ia, id, Ip1)
            enddo
            do id = 1, JPTOT
                CPOlsd(ia, 1, id, Ip2) = CPOlsd(ia, 1, id, Ip1)
                CPOlsd(ia, 2, id, Ip2) = CPOlsd(ia, 2, id, Ip1)
            enddo
        enddo
        !
        NXYpol(Ip2) = NXYpol(Ip1)
        do i = 1, NXYpol(Ip1)
            CPOlxy(i, 1, Ip2) = CPOlxy(i, 1, Ip1)
            CPOlxy(i, 2, Ip2) = CPOlxy(i, 2, Ip1)
        enddo
        !
    end subroutine plrcop
    !*==PRFCOP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PLRCOP




    subroutine prfcop(Ir1, Ir2)
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ir1, Ir2
        intent (in) Ir1, Ir2
        !
        ! Local variables
        !
        integer :: ia, is, k
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
        !---------------------------------------------
        !     Copies reference polar in slot IR1 into slot IR2
        !---------------------------------------------
        !
        NAMeref(Ir2) = NAMeref(Ir1)
        !
        do k = 1, 4
            NDRef(k, Ir2) = NDRef(k, Ir1)
        enddo
        !
        do is = 1, 2
            do k = 1, 4
                do ia = 1, NDRef(k, Ir2)
                    CPOlref(ia, is, k, Ir2) = CPOlref(ia, is, k, Ir1)
                enddo
            enddo
        enddo
        !
    end subroutine prfcop
    !*==POLAXI.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PRFCOP


    subroutine polaxi(Cpolplf, Xcdwid, Xalwid, Xocwid)
        use i_xfoil, only: show_output
        use m_userio, only: readr
        use i_pindex
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Xalwid, Xcdwid, Xocwid
        real, dimension(3, *) :: Cpolplf
        !
        ! Local variables
        !
        character(5), dimension(4), save :: cvar
        logical :: error
        integer :: j, kv
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
        !-------------------------------------------
        !     Gets polar plot axis limits from user
        !-------------------------------------------
        !
        data cvar/'Alpha', '  CL ', '  CD ', ' -CM '/
        !
        if (show_output) then
            write (*, *) 'Enter new axis annotations,', ' or <return> to leave unchanged...'
            write (*, *)
        endif
        !
        do kv = 1, 4
            do
                if (show_output) write (*, 99001) cvar(kv), (Cpolplf(j, kv), j = 1, 3)
                99001  format (3x, a, '  min, max, delta:', 3F11.5)
                call readr(3, Cpolplf(1, kv), error)
                if (error) then
                    if (show_output) write (*, *) 'READ error.  Enter again.'
                    cycle
                endif
                exit
            enddo
        enddo
        !
        !C---- widths of plot boxes in polar plot page
        !      XCDWID = 0.45
        !      XALWID = 0.25
        !      XOCWID = 0.20
        !
    end subroutine polaxi
    !*==BOTTOM.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! POLAXI



    subroutine bottom(Lu)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Lu
        intent (in) Lu
        !
        ! Local variables
        !
        character(1) :: dummy
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
        do
            !
            read (Lu, 99001, end = 99999, err = 99999) dummy
            99001 format (a)
        enddo
        !
    99999 end subroutine bottom
    !*==BOTTOMX.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine bottomx(Lu)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Lu
        intent (in) Lu
        !
        ! Local variables
        !
        character(1) :: dummy
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
        do
            !
            read (Lu, end = 99999, err = 99999) dummy
        enddo
        !
    99999 end subroutine bottomx

end module m_xpol