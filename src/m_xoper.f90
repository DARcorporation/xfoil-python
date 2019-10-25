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

!*==OPER.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! ZLEFIND
!
module m_xoper
    use s_xoper
contains
    subroutine oper
        use m_xpol, only: plxini, plradd, prfsum, plxadd, plrsrt, plrini, apcopy, plrsum, plrset, prfcop, plrcop
        use m_xpanel, only: psilin
        use m_userio, only: getflt, askc, askr, strip, aski, asks
        use m_iopol, only: polread, polref, polwrit
        use m_spline, only: seval, deval
        use i_xfoil
        use s_xfoil, only: mrcl, comset, cdcalc, cpcalc, clcalc
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        integer, parameter :: NPRX = 1001
        !
        ! Local variables
        !
        real :: aa1, aa2, adif, alast, cl1, cl2, clast, cpp, daa, dcl, doff, frac, psi, qqq, spr, uuu, vvv, &
                & xxx, yyy
        character(1) :: ans
        character(128), save :: argold
        character(4) :: comand
        character(128) :: comarg, line
        character(4), save :: comold
        logical :: error, lcpx, lrecalc
        integer :: i, ia, ia1, ia2, idsort, ip, ip1, ip2, ipoint, ipr, ir, irem, iseqex, itmaxs, ityp, ja, &
                & jp, jr, k, lu, luplr, luplx, nel, ninput, nnamep, nnp, nnref, npf, npoint, npr, nrem
        integer, dimension(20) :: iinput
        integer, dimension(NPX) :: nblp
        real, dimension(20) :: rinput
        real, dimension(NPRX) :: xpr, ypr
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !
        !
        !
        !
        !---- retain last-command info if OPER is exited and then re-entered
        !
        !---- logical units for  polar save file,  polar dump file
        luplr = 9
        luplx = 11
        !
        comand = '****'
        comarg = ' '
        lrecalc = .false.
        lcpx = .false.
        !
        if (N==0) then
            if (show_output) then
                write (*, *)
                write (*, *) '***  No airfoil available  ***'
            endif
            return
        endif
        !
        if (IPAct/=0) then
            if (show_output) write (*, 99001) IPAct
            99001 format (/'  Polar', i3, '  is active')
        endif
        !
        !cc 500  CONTINUE
        comold = comand
        argold = comarg
        do
            !
            !====================================================
            !---- start of menu loop
            !
            if (LVIsc) then
                if (LPAcc) then
                    call askc('.OPERva^', comand, comarg)
                else
                    call askc('.OPERv^', comand, comarg)
                endif
            elseif (LPAcc) then
                call askc('.OPERia^', comand, comarg)
            else
                call askc('.OPERi^', comand, comarg)
            endif
            !
            !---- process previous command ?
            if (comand(1:1)/='!') then
                lrecalc = .false.
            elseif (comold=='****') then
                if (show_output) write (*, *) 'Previous .OPER command not valid'
                cycle
            else
                comand = comold
                comarg = argold
                lrecalc = .true.
            endif
            !
            !---- return to top level
            !----- just <return> was typed... clean up plotting and exit OPER
            if (comand=='    ') return
            !
            !---- extract command line numeric arguments
            do i = 1, 20
                iinput(i) = 0
                rinput(i) = 0.0
            enddo
            ninput = 20
            call getflt(comarg, rinput, ninput, error)
            !
            !---- don't try to read integers, since might get integer overflow
            do i = 1, ninput
                if (abs(rinput(i))>2.1E9) then
                    iinput(i) = 2**30
                else
                    iinput(i) = int(rinput(i))
                endif
            enddo
            !
            !cc      NINPUT = 20
            !cc      CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
            !
            !--------------------------------------------------------
            if (comand=='?   ') then
                if (show_output) write (*, 99002)
                99002  format (&
                        /'   <cr>     Return to Top Level'&
                        /'   !        Redo last ALFA,CLI,CL,ASEQ,CSEQ,VELS'&
                        //'   Visc r   Toggle Inviscid/Viscous mode'&
                        /'  .VPAR     Change BL parameter(s)'&
                        /'   Re   r   Change Reynolds number'&
                        /'   Mach r   Change Mach number'&
                        /'   Type i   Change type of Mach,Re variation with CL'&
                        /'   ITER     Change viscous-solution iteration limit'&
                        /'   INIT     Toggle BL initialization flag'&
                        //'   Alfa r   Prescribe alpha'&
                        /'   CLI  r   Prescribe inviscid CL'&
                        /'   Cl   r   Prescribe CL'&
                        /'   ASeq rrr Prescribe a sequence of alphas'&
                        /'   CSeq rrr Prescribe a sequence of CLs'&
                        /'   CINC     Toggle  minimum Cp  inclusion in polar'&
                        /'   HINC     Toggle hinge moment inclusion in polar'&
                        /'   Pacc i   Toggle auto point accumulation to active polar'&
                        /'   PGET f   Read new polar from save file'&
                        /'   PWRT i   Write polar to save file'&
                        /'   PSUM     Show summary of stored polars'&
                        /'   PLIS i   List stored polar(s)'&
                        /'   PDEL i   Delete stored polar'&
                        /'   PSOR i   Sort stored polar'&
                        /'   ASET i   Copy stored airfoil into current airfoil'&
                        /'   PREM ir. Remove point(s) from stored polar'&
                        /'   PNAM i   Change airfoil name of stored polar'&
                        //'   RGET f   Read new reference polar from file'&
                        /'   RDEL i   Delete stored reference polar'&
                        //'   FMOM     Calculate flap hinge moment and forces'&
                        /'   FNEW rr  Set new flap hinge point'&
                        /'   VELS rr  Calculate velocity components at a point'&
                        /'   DUMP f   Output Ue,Dstar,Theta,Cf vs s,x,y to file'&
                        /'   CPWR f   Output x vs Cp to file'&
                        /'   CPMN     Report minimum surface Cp'&
                        /'   NAME s   Specify new airfoil name'&
                        /'   NINC     Increment name version number')
                !     &//'   IMAG    Toggle image-airfoil'
                !--------------------------------------------------------
            elseif (comand=='VISC' .or. comand=='V   ') then
                if (LPAcc) then
                    if (show_output) write (*, 99019)
                    cycle
                endif
                !
                LVIsc = .not.LVIsc
                !
                if (LVIsc) then
                    if (ninput>=1) then
                        REInf1 = rinput(1)
                    elseif (REInf1==0.0) then
                        call askr('Enter Reynolds number^', REInf1)
                    endif
                    !
                    call mrshow(.true., .true.)
                endif
                LVConv = .false.
                !
                !--------------------------------------------------------
            elseif (comand=='VPAR') then
                call vpar
                !
                !--------------------------------------------------------
            elseif (comand=='RE  ' .or. comand=='R   ') then
                if (LPAcc .and. LVIsc) then
                    if (show_output) write (*, 99019)
                    cycle
                endif
                !
                if (ninput>=1) then
                    REInf1 = rinput(1)
                else
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'Currently...'
                    endif
                    call mrshow(.false., .true.)
                    call askr('Enter new Reynolds number^', REInf1)
                endif
                !
                !cc    CALL MRSHOW(.FALSE.,.TRUE.)
                call mrcl(1.0, MINf_cl, REInf_cl)
                LVConv = .false.
                !
                !--------------------------------------------------------
            elseif (comand=='MACH' .or. comand=='M   ') then
                if (LPAcc) then
                    if (show_output) write (*, 99019)
                    cycle
                endif
                do
                    !
                    if (ninput>=1) then
                        MINf1 = rinput(1)
                    else
                        if (show_output) then
                            write (*, *)
                            write (*, *) 'Currently...'
                        endif
                        call mrshow(.true., .false.)
                        call askr('Enter Mach number^', MINf1)
                    endif
                    !
                    if (MINf1>=1.0) then
                        if (show_output) write (*, *) 'Supersonic freestream not allowed'
                        ninput = 0
                        cycle
                    endif
                    !cc    CALL MRSHOW(.TRUE.,.FALSE.)
                    call mrcl(1.0, MINf_cl, REInf_cl)
                    call comset
                    !
                    if (MINf>0.0 .and. show_output) write (*, 99003) CPStar, QSTar / QINf
                    99003      format (/' Sonic Cp =', f10.2, '      Sonic Q/Qinf =', f10.3/)
                    !
                    call cpcalc(N, QINv, QINf, MINf, CPI)
                    if (LVIsc) call cpcalc(N + NW, QVIs, QINf, MINf, CPV)
                    call clcalc(N, X, Y, GAM, GAM_a, ALFa, MINf, QINf, XCMref, YCMref, CL, CM, CDP, CL_alf, CL_msq)
                    call cdcalc
                    LVConv = .false.
                    exit
                enddo
                !
                !--------------------------------------------------------
            elseif (comand=='TYPE' .or. comand=='T') then
                if (LPAcc) then
                    if (show_output) write (*, 99019)
                    cycle
                endif
                do
                    !
                    if (ninput>=1) then
                        ityp = iinput(1)
                    else
                        if (show_output) write (*, 99004)
                        99004          format (&
                                /' Type   parameters held constant       varying      fixed   '&
                                /' ----   ------------------------       -------   -----------'&
                                /'   1    M          , Re            ..   lift     chord, vel.'&
                                /'   2    M sqrt(CL) , Re sqrt(CL)   ..   vel.     chord, lift'&
                                /'   3    M          , Re CL         ..   chord    lift , vel.')
                        call aski('Enter type of Mach,Re variation with CL^', ityp)
                    endif
                    !
                    if (ityp==1) then
                        MATyp = 1
                        RETyp = 1
                    elseif (ityp==2) then
                        MATyp = 2
                        RETyp = 2
                    elseif (ityp==3) then
                        MATyp = 1
                        RETyp = 3
                    endif
                    !
                    if (ityp<1 .or. ityp>3) then
                        ninput = 0
                        cycle
                    endif
                    !
                    call mrshow(.true., .true.)
                    call mrcl(1.0, MINf_cl, REInf_cl)
                    call comset
                    LVConv = .false.
                    exit
                enddo
                !
                !--------------------------------------------------------
            elseif (comand=='ITER') then
                do
                    if (ninput>=1) then
                        ITMax = iinput(1)
                    else
                        if (show_output) write (*, *) 'Current iteration limit:', ITMax
                        call aski('Enter new iteration limit^', ITMax)
                    endif
                    !
                    if (ITMax<1) then
                        ninput = 0
                        cycle
                    endif
                    exit
                enddo
                !
                !--------------------------------------------------------
            elseif (comand=='INIT') then
                LBLini = .not.LBLini
                if (LBLini) then
                    if (show_output) write (*, *) 'BLs are assumed to be initialized'
                else
                    if (show_output) write (*, *) 'BLs will be initialized on next point'
                    LIPan = .false.
                endif
                !
                !--------------------------------------------------------
                !      ELSEIF(COMAND.EQ.'IMAG') THEN
                !       LIMAGE = .NOT.LIMAGE
                !       IF(LIMAGE) THEN
                !        CALL ASKR('Enter y-position of image plane^',YIMAGE)
                !        CALL ASKI('Specify image type (1=wall -1=free jet)^',KIMAGE)
                !       ELSE
                !        WRITE(*,*) 'Image airfoil removed'
                !       ENDIF
                !       LGAMU = .FALSE.
                !       LQAIJ = .FALSE.
                !
                !--------------------------------------------------------
            elseif (comand=='ALFA' .or. comand=='A   ') then
                if (.not.lrecalc) then
                    !------- set inviscid solution only if point is not being recalculated
                    if (ninput>=1) then
                        ADEg = rinput(1)
                    else
                        ADEg = ALFa / DTOr
                        call askr('Enter angle of attack (deg)^', ADEg)
                    endif
                    LALfa = .true.
                    ALFa = DTOr * ADEg
                    QINf = 1.0
                    call specal
                    if (abs(ALFa - AWAke)>1.0E-5) LWAke = .false.
                    if (abs(ALFa - AVIsc)>1.0E-5) LVConv = .false.
                    if (abs(MINf - MVIsc)>1.0E-5) LVConv = .false.
                endif
                !
                if (LVIsc) then
                    LVConv = viscal(ITMax)
                end if
                call fcpmin
                !
                !cc    IF( LVISC .AND. LPACC .AND. LVCONV ) THEN
                if (LPAcc .and. (LVConv .or. .not.LVIsc)) then
                    call plradd(luplr, IPAct)
                    call plxadd(luplx, IPAct)
                endif
                !
                if (LVIsc .and. .not.LPAcc .and. .not.LVConv .and. show_output) write (*, *) 'Type "!" to continue iterating'
                !
                !
                comold = comand
                argold = comarg
                !
                !--------------------------------------------------------
            elseif (comand=='CLI ') then
                if (.not.lrecalc) then
                    if (ninput>=1) then
                        CLSpec = rinput(1)
                    else
                        call askr('Enter inviscid lift coefficient^', CLSpec)
                    endif
                    LALfa = .true.
                    ALFa = 0.0
                    QINf = 1.0
                    call speccl
                    ADEg = ALFa / DTOr
                    if (abs(ALFa - AWAke)>1.0E-5) LWAke = .false.
                    if (abs(ALFa - AVIsc)>1.0E-5) LVConv = .false.
                    if (abs(MINf - MVIsc)>1.0E-5) LVConv = .false.
                endif
                !
                if (LVIsc) then
                    LVConv = viscal(ITMax)
                end if
                call fcpmin
                !
                !cc    IF( LVISC .AND. LPACC .AND. LVCONV ) THEN
                if (LPAcc .and. (LVConv .or. .not.LVIsc)) then
                    call plradd(luplr, IPAct)
                    call plxadd(luplx, IPAct)
                endif
                !
                if (LVIsc .and. .not.LPAcc .and. .not.LVConv .and. show_output) write (*, *) 'Type "!" to continue iterating'
                !
                comold = comand
                argold = comarg
                !
                !--------------------------------------------------------
            elseif (comand=='CL  ' .or. comand=='C   ') then
                if (.not.lrecalc) then
                    if (ninput>=1) then
                        CLSpec = rinput(1)
                    else
                        call askr('Enter lift coefficient^', CLSpec)
                    endif
                    LALfa = .false.
                    ALFa = 0.0
                    QINf = 1.0
                    call speccl
                    ADEg = ALFa / DTOr
                    if (abs(ALFa - AWAke)>1.0E-5) LWAke = .false.
                    if (abs(ALFa - AVIsc)>1.0E-5) LVConv = .false.
                    if (abs(MINf - MVIsc)>1.0E-5) LVConv = .false.
                endif
                if (LVIsc) then
                    LVConv = viscal(ITMax)
                end if
                call fcpmin
                !
                !cc    IF( LVISC .AND. LPACC .AND. LVCONV ) THEN
                if (LPAcc .and. (LVConv .or. .not.LVIsc)) then
                    call plradd(luplr, IPAct)
                    call plxadd(luplx, IPAct)
                endif
                !
                if (LVIsc .and. .not.LPAcc .and. .not.LVConv .and. show_output) write (*, *) 'Type "!" to continue iterating'
                !
                comold = comand
                argold = comarg
                !
                !--------------------------------------------------------
            elseif (comand=='ASEQ' .or. comand=='AS  ' .or. comand=='CSEQ' .or. comand=='CS  ') then
                LALfa = comand=='ASEQ' .or. comand=='AS  '
                !
                if (LALfa) then
                    if (ninput>=3) then
                        aa1 = rinput(1)
                        aa2 = rinput(2)
                        daa = rinput(3)
                    elseif (ninput>=2) then
                        aa1 = rinput(1)
                        aa2 = rinput(2)
                        daa = daa / DTOr
                        call askr('Enter alfa increment   (deg)^', daa)
                    elseif (ninput>=1) then
                        aa1 = rinput(1)
                        aa2 = aa2 / DTOr
                        call askr('Enter last  alfa value (deg)^', aa2)
                        daa = daa / DTOr
                        call askr('Enter alfa increment   (deg)^', daa)
                    else
                        aa1 = aa1 / DTOr
                        call askr('Enter first alfa value (deg)^', aa1)
                        aa2 = aa2 / DTOr
                        call askr('Enter last  alfa value (deg)^', aa2)
                        daa = daa / DTOr
                        call askr('Enter alfa increment   (deg)^', daa)
                    endif
                    if (aa2<aa1) then
                        daa = -abs(daa)
                    else
                        daa = abs(daa)
                    endif
                    aa1 = aa1 * DTOr
                    aa2 = aa2 * DTOr
                    daa = daa * DTOr
                    npoint = 1
                    if (daa/=0.0) npoint = int((aa2 - aa1) / daa + 0.5) + 1
                    !
                else
                    if (ninput>=3) then
                        cl1 = rinput(1)
                        cl2 = rinput(2)
                        dcl = rinput(3)
                    elseif (ninput>=2) then
                        cl1 = rinput(1)
                        cl2 = rinput(2)
                        call askr('Enter CL increment  ^', dcl)
                    elseif (ninput>=1) then
                        cl1 = rinput(1)
                        call askr('Enter last  CL value^', cl2)
                        call askr('Enter CL increment  ^', dcl)
                    else
                        call askr('Enter first CL value^', cl1)
                        call askr('Enter last  CL value^', cl2)
                        call askr('Enter CL increment  ^', dcl)
                    endif
                    if (cl2<cl1) then
                        dcl = -abs(dcl)
                    else
                        dcl = abs(dcl)
                    endif
                    npoint = 1
                    if (dcl/=0.0) npoint = int((cl2 - cl1) / dcl + 0.5) + 1
                endif
                !
                !- - - - - - - - - - - - - - - - - -
                !
                !----- initialize unconverged-point counter
                iseqex = 0
                alast = ADEg
                clast = CL
                !
                !----- calculate each point, add Cp distribution to plot, and save to polar
                do ipoint = 1, npoint
                    !
                    !------- set proper alpha for this point
                    if (LALfa) then
                        ALFa = aa1 + daa * float(ipoint - 1)
                    else
                        CLSpec = cl1 + dcl * float(ipoint - 1)
                        call speccl
                    endif
                    !
                    if (abs(ALFa - AWAke)>1.0E-5) LWAke = .false.
                    if (abs(ALFa - AVIsc)>1.0E-5) LVConv = .false.
                    if (abs(MINf - MVIsc)>1.0E-5) LVConv = .false.
                    call specal
                    itmaxs = ITMax + 5
                    if (LVIsc) then
                        LVConv = viscal(itmaxs)
                    end if
                    !
                    ADEg = ALFa / DTOr
                    !
                    call fcpmin
                    !
                    !------- add point to buffer polar and/or disk files
                    !cc      IF( LVISC .AND. LPACC .AND. LVCONV ) THEN
                    if (LPAcc .and. (LVConv .or. .not.LVIsc)) then
                        call plradd(luplr, IPAct)
                        call plxadd(luplx, IPAct)
                    endif
                    !###
                    !cc    call dcpout
                    !
                    if (LVIsc .and. .not.LVConv) then
                        !-------- increment unconverged-point counter
                        iseqex = iseqex + 1
                        if (iseqex>=NSEqex) then
                            if (show_output) write (*, 99005) iseqex, alast, clast
                            99005 format (/' Sequence halted since previous', i3, ' points did not converge'/&
                                    ' Last-converged  alpha =', f8.3, '    CL =', f10.5)
                            exit
                        endif
                    else
                        !-------- converged OK... reset unconverged-point counter
                        iseqex = 0
                        alast = ADEg
                        clast = CL
                    endif
                    !
                enddo
                !cc      CALL ASKC('hit <cr>^',DUMMY,COMARG)
                !
                comold = comand
                argold = comarg
                !
                !--------------------------------------------------------
            elseif (comand=='PACC' .or. comand=='P   ') then
                LPAcc = .not.LPAcc
                !
                if (LPAcc) then
                    if (ninput>=1) then
                        !------- slot into which accumulated polar will go
                        ip = min(max(iinput(1), 0), NPOl + 1)
                    else
                        !------- no command argument was given... just use next available slot
                        ip = NPOl + 1
                        PFName(ip) = ' '
                        PFNamx(ip) = ' '
                    endif
                    !
                    if (ip<=NPOl) then
                        IPAct = ip
                    elseif (NPOl==NPX) then
                        if (show_output) then
                            write (*, *)
                            write (*, *) 'Number of polars is at array limit'
                            write (*, *) 'New polar will not be stored'
                        endif
                        IPAct = 0
                    else
                        IPAct = NPOl + 1
                        PFName(IPAct) = ' '
                        PFNamx(IPAct) = ' '
                        !
                        !
                    endif
                    !
                    !------ set up for appending to new or existing polar (if IPACT > 0)
                    call plrset(IPAct)
                    !
                    !------ jump out if decision was made to abort polar accumulation
                    if (IPAct<=0) then
                        LPAcc = .false.
                        cycle
                    endif
                    !
                    call plrini(luplr, IPAct)
                    call plxini(luplx, IPAct)
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'Polar accumulation enabled'
                    endif
                    !
                else
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'Polar accumulation disabled'
                    endif
                    IPAct = 0
                    !
                endif
                !
                !--------------------------------------------------------
            elseif (comand=='PGET') then
                if (NPOl>=NPX) then
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'Number of polars is at array limit'
                        write (*, *) 'Delete with PDEL if necessary'
                    endif
                    cycle
                endif
                !
                ip = NPOl + 1
                !
                if (comarg==' ') then
                    call asks('Enter polar filename^', FNAme)
                else
                    FNAme = comarg
                endif
                !
                lu = 17
                call polread(lu, FNAme, error, NAX, NAPol(ip), CPOl(1, 1, ip), &
                        REYnp1(ip), MAChp1(ip), ACRitp(1, ip), XSTripp(1, ip), &
                        & PTRatp(ip), ETApp(ip), NAMepol(ip), IREtyp(ip), IMAtyp(ip), &
                        ISX, nblp(ip), CPOlsd(1, 1, 1, ip), CODepol(ip), &
                        & VERspol(ip))
                if (error) then
                    if (show_output) write (*, *) 'Polar file READ error'
                else
                    NPOl = ip
                    NXYpol(ip) = 0
                    call strip(NAMepol(ip), nnamep)
                    nel = 1
                    call polwrit(6, ' ', error, .true., NAX, 1, &
                            NAPol(ip), CPOl(1, 1, ip), IPOl, NIPol, REYnp1(ip), MAChp1(ip), ACRitp(1, ip), &
                            & XSTripp(1, ip), PTRatp(ip), ETApp(ip), NAMepol(ip), IREtyp(ip), &
                            IMAtyp(ip), ISX, nel, CPOlsd(1, 1, 1, ip), &
                            & JPOl, NJPol, CODepol(ip), VERspol(ip), .false.)
                    PFName(ip) = FNAme
                    if (show_output) write (*, 99006) ip
                    99006      format (/' Stored as  Polar', i4)
                endif
                !
                !--------------------------------------------------------
            elseif (comand=='PWRT') then
                do
                    if (NPOl==1) then
                        ip = 1
                    elseif (ninput==0) then
                        call plrsum(1, NPOl, IPAct)
                        call aski('Enter index of polar to write (0=all, -1=abort)^', ip)
                        if (ip==-1) exit
                    else
                        ip = iinput(1)
                    endif
                    !
                    if (ip==0) then
                        ip1 = 1
                        ip2 = NPOl
                    elseif (ip>=1 .and. ip<=NPOl) then
                        ip1 = ip
                        ip2 = ip
                    else
                        ninput = 0
                        cycle
                    endif
                    !
                    nel = 1
                    do ip = ip1, ip2
                        lu = 19
                        call plrsum(ip, ip, IPAct)
                        call strip(PFName(ip), npf)
                        if (npf==0) then
                            line = 'Enter polar output filename^'
                        else
                            line = 'Enter polar output filename [' // PFName(ip)(1:npf) // ']^'
                        endif
                        call asks(line, FNAme)
                        if (npf/=0 .and. FNAme==' ') FNAme = PFName(ip)
                        !
                        NIPol = NIPol0
                        if (LCMinp) then
                            NIPol = NIPol + 1
                            IPOl(IMC) = NIPol
                        endif
                        if (LHMomp) then
                            NIPol = NIPol + 1
                            IPOl(ICH) = NIPol
                        endif
                        !
                        call polwrit(lu, FNAme, error, .true., NAX, 1, &
                                NAPol(ip), CPOl(1, 1, ip), IPOl, NIPol, REYnp1(ip), MAChp1(ip), &
                                & ACRitp(1, ip), XSTripp(1, ip), PTRatp(ip), ETApp(ip), &
                                NAMepol(ip), IREtyp(ip), IMAtyp(ip), ISX, nel, &
                                & CPOlsd(1, 1, 1, ip), JPOl, NJPol, 'XFOIL', VERsion, .true.)
                        if (error) then
                            if (show_output) write (*, 99007) ip
                            99007              format (' Polar', i3, '  not written')
                        else
                            PFName(ip) = FNAme
                        endif
                    enddo
                    exit
                enddo
                !
                !--------------------------------------------------------
            elseif (comand=='RGET') then
                if (NPOlref>=NPX) then
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'Number of reference polars is at array limit'
                        write (*, *) 'Delete with RDEL if necessary'
                    endif
                    cycle
                endif
                !
                ir = NPOlref + 1
                !
                if (comarg==' ') then
                    call asks('Enter reference polar filename^', FNAme)
                else
                    FNAme = comarg
                endif
                !
                lu = 9
                open (lu, file = FNAme, status = 'OLD', err = 20)
                call polref(lu, FNAme, error, NFX, NDRef(1, ir), CPOlref(1, 1, 1, ir), NAMeref(ir))
                close (lu)
                if (.not.(error)) then
                    !
                    NPOlref = ir
                    !
                    call strip(NAMeref(ir), nnref)
                    if (nnref==0) then
                        call asks('Enter label for reference polar^', NAMeref(ir))
                        call strip(NAMeref(ir), nnref)
                    else
                        if (show_output) then
                            write (*, *)
                            write (*, *) NAMeref(ir)
                        endif
                    endif
                    !
                    !cc     ICOLR(IR) = NCOLOR - IR + 1
                    ICOlr(ir) = 2 + ir
                    ISYmr(ir) = mod(ir, 10)
                    cycle
                endif
                !
                20    if (show_output) write (*, *) 'File OPEN error'
                !
                !--------------------------------------------------------
            elseif (comand=='RDEL') then
                if (NPOlref==0) then
                    if (show_output) write (*, *) 'No reference polars are stored'
                    cycle
                endif
                !
                if (ninput>=1) then
                    ir = iinput(1)
                else
                    ir = NPOlref + 1
                endif
                do
                    !
                    !
                    if (ir==0) then
                        !------- delete all polars
                        NPOlref = 0
                        !
                        !------- abort
                    elseif (ir==-1) then
                        !
                    elseif (ir<-1 .or. ir>NPOlref) then
                        call prfsum(1, NPOlref)
                        call aski('Specify ref. polar to delete (0 = all, -1 = abort)^', ir)
                        cycle
                        !
                    else
                        !------- delete ref. polar IR
                        do jr = ir + 1, NPOlref
                            call prfcop(jr, jr - 1)
                            if (show_output) write (*, 99016) jr, jr - 1
                        enddo
                        NPOlref = NPOlref - 1
                    endif
                    exit
                enddo
                !
                !--------------------------------------------------------
            elseif (comand=='PSUM') then
                if (NPOl==0) then
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'No polars are stored'
                    endif
                    cycle
                endif
                !
                call plrsum(1, NPOl, IPAct)
                !
                !--------------------------------------------------------
            elseif (comand=='PLIS') then
                if (NPOl==0) then
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'No polars are stored'
                    endif
                    cycle
                endif
                !
                if (ninput==0) then
                    ip1 = 1
                    ip2 = NPOl
                else
                    ip = iinput(1)
                    if (ip==0) then
                        ip1 = 1
                        ip2 = NPOl
                    elseif (ip>=1 .and. ip<=NPOl) then
                        ip1 = ip
                        ip2 = ip
                    else
                        if (show_output) then
                            write (*, *)
                            write (*, *) 'Specified stored polar does not exist'
                        endif
                        cycle
                    endif
                endif
                !
                NIPol = NIPol0
                if (LCMinp) then
                    NIPol = NIPol + 1
                    IPOl(IMC) = NIPol
                endif
                if (LHMomp) then
                    NIPol = NIPol + 1
                    IPOl(ICH) = NIPol
                endif
                !
                nel = 1
                do ip = ip1, ip2
                    if (show_output) write (*, 99015) ip
                    ia1 = 1
                    ia2 = NAPol(ip)
                    call polwrit(6, ' ', error, .true., NAX, ia1, ia2, &
                            CPOl(1, 1, ip), IPOl, NIPol, REYnp1(ip), MAChp1(ip), ACRitp(1, ip), &
                            & XSTripp(1, ip), PTRatp(ip), ETApp(ip), &
                            NAMepol(ip), IREtyp(ip), IMAtyp(ip), ISX, nel, CPOlsd(1, 1, 1, ip), &
                            & JPOl, NJPol, 'XFOIL', VERsion, .false.)
                enddo
                NIPol = NIPol0
                !
                !--------------------------------------------------------
            elseif (comand=='PDEL') then
                if (NPOl==0) then
                    if (show_output) write (*, *) 'No polars are stored'
                    cycle
                endif
                !
                if (ninput>=1) then
                    !------- use command argument
                    ip = iinput(1)
                else
                    !------- no argument given... set up for user query test below
                    ip = NPOl + 1
                endif
                do
                    !
                    if (ip==0) then
                        !------- delete all polars
                        NPOl = 0
                        IPAct = 0
                        LPAcc = .false.
                        !
                    elseif (ip==-1) then
                        !------- abort
                        exit
                        !
                    elseif (ip<-1 .or. ip>NPOl) then
                        call plrsum(1, NPOl, IPAct)
                        call aski('Specify polar to delete (0 = all, -1 = abort)^', ip)
                        cycle
                        !
                    else
                        !------- delete polar IP
                        if (IPAct==ip) then
                            if (show_output) write (*, *) 'Active polar deleted.  Accumulation turned off'
                            IPAct = 0
                            LPAcc = .false.
                        endif
                        !
                        do jp = ip + 1, NPOl
                            call plrcop(jp, jp - 1)
                            if (show_output) write (*, 99016) jp, jp - 1
                            if (IPAct==jp) IPAct = jp - 1
                        enddo
                        NPOl = NPOl - 1
                        !
                    endif
                    !
                    if (IPAct>0 .and. show_output) write (*, 99017) IPAct
                    exit
                enddo
                !
                !--------------------------------------------------------
            elseif (comand=='PSOR') then
                if (NPOl==0) then
                    if (show_output) write (*, *) 'No polars are stored'
                    cycle
                endif
                !
                if (ninput>=1) then
                    !------- use command argument
                    ip = iinput(1)
                else
                    !------- no argument given... set up for user query test below
                    ip = NPOl + 1
                endif
                !
                !------ sort polars in increasing alpha
                idsort = IAL
                !------- abort
                do
                    !
                    if (ip==-1) then
                        !
                    elseif (ip<-1 .or. ip>NPOl) then
                        call plrsum(1, NPOl, IPAct)
                        call aski('Specify polar to sort (0 = all, -1 = abort)^', ip)
                        cycle
                        !
                    else
                        !------- sort polar(s)
                        if (ip==0) then
                            ip1 = 1
                            ip2 = NPOl
                        else
                            ip1 = ip
                            ip2 = ip
                        endif
                        do jp = ip1, ip2
                            call plrsrt(jp, idsort)
                        enddo
                    endif
                    exit
                enddo
                !
                !--------------------------------------------------------
            elseif (comand=='ASET') then
                if (NPOl==0) then
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'No polar airfoils are stored'
                    endif
                    cycle
                endif
                do
                    !
                    if (ninput/=0) then
                        ip = iinput(1)
                    elseif (NPOl==1) then
                        ip = 1
                    else
                        call plrsum(1, NPOl, IPAct)
                        call aski('Enter index of polar airfoil to set^', ip)
                    endif
                    !
                    if (ip==0) exit
                    if (ip<1 .or. ip>NPOl) then
                        if (show_output) then
                            write (*, *)
                            write (*, *) 'Specified polar airfoil does not exist'
                        endif
                        ninput = 0
                        cycle
                    endif
                    !
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'Current airfoil will be overwritten.  Proceed?  Y'
                    endif
                    read (*, 99018) ans
                    !
                    if (index('Nn', ans)/=0) then
                        if (show_output) write (*, *) 'No action taken'
                    else
                        call apcopy(ip)
                    endif
                    exit
                enddo
                !
                !--------------------------------------------------------
            elseif (comand=='PREM') then
                if (NPOl==0) then
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'No polars are stored'
                    endif
                    cycle
                endif
                do
                    !
                    if (ninput/=0) then
                        ip = iinput(1)
                    elseif (NPOl==1) then
                        ip = 1
                    else
                        call plrsum(1, NPOl, IPAct)
                        call aski('Enter index of polar to modify^', ip)
                    endif
                    !
                    if (ip==0) goto 100
                    if (ip<1 .or. ip>NPOl) then
                        if (show_output) then
                            write (*, *)
                            write (*, *) 'Specified polar airfoil does not exist'
                        endif
                        ninput = 0
                        cycle
                    endif
                    !
                    if (ninput>=2) then
                        nrem = ninput - 1
                    else
                        NIPol = NIPol0
                        if (LCMinp) then
                            NIPol = NIPol + 1
                            IPOl(IMC) = NIPol
                        endif
                        if (LHMomp) then
                            NIPol = NIPol + 1
                            IPOl(ICH) = NIPol
                        endif
                        !
                        if (show_output) write (*, 99015) ip
                        ia1 = 1
                        ia2 = NAPol(ip)
                        call polwrit(6, ' ', error, .true., NAX, ia1, ia2, &
                                CPOl(1, 1, ip), IPOl, NIPol, REYnp1(ip), MAChp1(ip), ACRitp(1, ip), &
                                & XSTripp(1, ip), PTRatp(ip), ETApp(ip), &
                                NAMepol(ip), IREtyp(ip), IMAtyp(ip), ISX, 1, CPOlsd(1, 1, 1, ip), &
                                & JPOl, NJPol, 'XFOIL', VERsion, .false.)
                        do
                            if (show_output) write (*, 99008)
                            99008              format (/' Enter alpha(s) of points to be removed:  ', $)
                            read (*, 99018) line
                            nrem = 19
                            call getflt(line, rinput(2), nrem, error)
                            if (.not.(error)) exit
                        enddo
                    endif
                    exit
                enddo
                !
                !----- go over specified alphas to be removed
                do irem = 1, nrem
                    !------- check all alpha points in polar IP
                    do ia = 1, NAPol(ip)
                        adif = CPOl(ia, IAL, ip) - rinput(irem + 1)
                        if (abs(adif)<0.0005) then
                            !---------- alphas match within 3-digit print tolerance...
                            !-             remove point by pulling down all points above it
                            do ja = ia, NAPol(ip) - 1
                                do k = 1, IPTOT
                                    CPOl(ja, k, ip) = CPOl(ja + 1, k, ip)
                                enddo
                                do k = 1, JPTOT
                                    CPOlsd(ja, 1, k, ip) = CPOlsd(ja + 1, 1, k, ip)
                                    CPOlsd(ja, 2, k, ip) = CPOlsd(ja + 1, 2, k, ip)
                                enddo
                            enddo
                            !---------- shrink polar by 1
                            NAPol(ip) = NAPol(ip) - 1
                            !
                            if (NAPol(ip)<=0) then
                                !----------- last point has been removed... eliminate this polar IP
                                do jp = ip + 1, NPOl
                                    call plrcop(jp, jp - 1)
                                    if (IPAct==jp) IPAct = jp - 1
                                    if (show_output) write (*, 99016) jp, jp - 1
                                enddo
                                NPOl = NPOl - 1
                                !
                                if (IPAct>0 .and. show_output) write (*, 99017) IPAct
                                !
                                goto 100
                            endif
                            !
                            !---------- go to next specified alpha to be removed
                            exit
                        endif
                    enddo
                enddo
                !
                !--------------------------------------------------------
            elseif (comand=='PNAM') then
                if (NPOl==0) then
                    if (show_output) then
                        write (*, *)
                        write (*, *) 'No polars are stored'
                    endif
                    cycle
                endif
                do
                    !
                    if (ninput/=0) then
                        ip = iinput(1)
                    elseif (NPOl==1) then
                        ip = 1
                    else
                        call plrsum(1, NPOl, IPAct)
                        call aski('Enter index of polar to modify^', ip)
                    endif
                    !
                    if (ip==0) exit
                    if (ip<1 .or. ip>NPOl) then
                        if (show_output) then
                            write (*, *)
                            write (*, *) 'Specified polar airfoil does not exist'
                        endif
                        ninput = 0
                        cycle
                    endif
                    !
                    NIPol = NIPol0
                    if (LCMinp) then
                        NIPol = NIPol + 1
                        IPOl(IMC) = NIPol
                    endif
                    if (LHMomp) then
                        NIPol = NIPol + 1
                        IPOl(ICH) = NIPol
                    endif
                    !
                    if (show_output) write (*, 99015) ip
                    ia1 = 0
                    ia2 = -1
                    call polwrit(6, ' ', error, .true., NAX, ia1, ia2, &
                            CPOl(1, 1, ip), IPOl, NIPol, REYnp1(ip), MAChp1(ip), ACRitp(1, ip), &
                            & XSTripp(1, ip), PTRatp(ip), ETApp(ip), &
                            NAMepol(ip), IREtyp(ip), IMAtyp(ip), ISX, 1, CPOlsd(1, 1, 1, ip), &
                            & JPOl, NJPol, 'XFOIL', VERsion, .false.)
                    NIPol = NIPol0
                    if (show_output) write (*, 99009)
                    99009      format (/' Enter new airfoil name of polar:  ', $)
                    read (*, 99018) NAMepol(ip)
                    call strip(NAMepol(ip), nnp)
                    exit
                enddo
                !
                !--------------------------------------------------------
            elseif (comand=='BL  ') then
                if (.not.LVConv) then
                    if (show_output) write (*, *) 'Compute valid viscous solution first'
                    cycle
                endif
                !
                if (ninput>=1) then
                    npr = min(iinput(1), NPRX)
                else
                    npr = 21
                    if (show_output) write (*, *) 'Using default number of profiles:', npr
                endif
                !
                if (npr>1) then
                    !------ set NPR points along surface, offset slightly for the locating logic
                    doff = 0.00001 * (S(N) - S(1))
                    do ipr = 1, npr
                        frac = float(ipr - 1) / float(npr - 1)
                        spr = S(1) + (S(N) - S(1)) * frac
                        xpr(ipr) = seval(spr, X, XP, S, N) + doff * deval(spr, Y, YP, S, N)
                        ypr(ipr) = seval(spr, Y, YP, S, N) - doff * deval(spr, X, XP, S, N)
                    enddo
                endif
                !
                !--------------------------------------------------------
            elseif (comand=='FMOM') then
                call mhinge
                if (show_output) write (*, 99010) XOF, YOF, HMOm, HFX, HFY
                99010  format (/' Flap hinge x,y :', 2F8.4/&
                        '                                           2  2'/&
                        ' Hinge moment/span = ', f8.6, '  x  1/2 rho V  c '/&
                        '                                           2   '/&
                        ' x-Force     /span = ', f8.6, '  x  1/2 rho V  c '/&
                        '                                           2   '/&
                        ' y-Force     /span = ', f8.6, '  x  1/2 rho V  c '/)
                !
                !--------------------------------------------------------
            elseif (comand=='FNEW') then
                if (ninput>=2) then
                    XOF = rinput(1)
                    YOF = rinput(2)
                elseif (ninput>=1) then
                    XOF = rinput(1)
                    YOF = -999.0
                else
                    XOF = -999.0
                    YOF = -999.0
                endif
                LFLap = .false.
                !
                !--------------------------------------------------------
            elseif (comand=='VELS') then
                if (ninput>=2) then
                    xxx = rinput(1)
                    yyy = rinput(2)
                elseif (ninput>=1) then
                    xxx = rinput(1)
                    call askr('Enter y^', yyy)
                else
                    call askr('Enter x^', xxx)
                    call askr('Enter y^', yyy)
                endif
                call psilin(0, xxx, yyy, -1.0, 0.0, psi, vvv, .false., .true.)
                call psilin(0, xxx, yyy, 0.0, 1.0, psi, uuu, .false., .true.)
                qqq = sqrt(uuu**2 + vvv**2)
                cpp = 1.0 - (uuu**2 + vvv**2)
                if (show_output) write (*, 99011) uuu, vvv, qqq, cpp
                99011  format (/' u/Uinf = ', f8.4, '   v/Uinf = ', f8.4/' q/Uinf = ', f8.4, '   Cp     = ', f8.4/)
                !
                comold = comand
                argold = comarg
                !
                !--------------------------------------------------------
            elseif (comand=='DUMP') then
                call bldump(comarg)

                !--------------------------------------------------------
            elseif (comand=='DMP ') then
                call bldump2(comarg)
                !
                !--------------------------------------------------------
            elseif (comand=='CPWR') then
                call cpdump(comarg)
                !
                !--------------------------------------------------------
            elseif (comand=='CPMN') then
                if (LVIsc) then
                    if (show_output) write (*, 99012) CPMni, XCPmni, CPMnv, XCPmnv
                    99012      format ('  Minimum Inviscid Cp =', f8.4, '   at x =', f8.4/&
                            '  Minimum Viscous  Cp =', f8.4, '   at x =', f8.4)
                else
                    if (show_output) write (*, 99013) CPMni, XCPmni
                    99013      format ('  Minimum Inviscid Cp =', f8.4, '   at x =', f8.4)
                endif
                !
                !--------------------------------------------------------
            elseif (comand=='CINC') then
                LCMinp = .not.LCMinp
                if (LCMinp) then
                    if (show_output) write (*, *) 'Min Cp will be written to polar save file'
                else
                    if (show_output) write (*, *) 'Min Cp won''t be written to polar save file'
                endif
                !
                !--------------------------------------------------------
            elseif (comand=='HINC') then
                LHMomp = .not.LHMomp
                if (show_output) then
                    if (LHMomp) then
                        write (*, *) 'Hinge moment will be written to polar save file'
                        if (.not.LFLap) then
                            write (*, *)
                            write (*, *) 'Note: Flap hinge location not defined'
                            write (*, *) '      Set it with FNEW,FMOM commands'
                        endif
                    else
                        write (*, *) 'Hinge moment won''t be written to polar save file'
                    endif
                end if
                !
                !--------------------------------------------------------
            elseif (comand=='NAME') then
                if (comarg==' ') then
                    call nammod(NAMe, 0, -1)
                else
                    NAMe = comarg
                endif
                call strip(NAMe, NNAme)
                !
                !--------------------------------------------------------
            elseif (comand=='NINC') then
                call nammod(NAMe, 1, 1)
                call strip(NAMe, NNAme)
                !
                !--------------------------------------------------------
            elseif (comand=='NDEC') then
                call nammod(NAMe, -1, 1)
                call strip(NAMe, NNAme)
                !
                !--------------------------------------------------------
            elseif (comand=='DAMP') then
                if (IDAmp==0) then
                    IDAmp = 1
                    if (show_output) write (*, *) 'Modified amplification used'
                else
                    IDAmp = 0
                    if (show_output) write (*, *) 'Original amplification used'
                endif
                !--------------------------------------------------------
            else
                if (show_output) write (*, 99014) comand
                99014  format (1x, a4, ' command not recognized.  Type a "?" for list')

                !
                !---- go back to top of menu loop
            endif
        100  enddo
        99015 format (/' =============================================================='/' Polar', i3)
        99016 format (' Polar', i3, '  moved into polar', i3)
        99017 format (' Polar', i3, '  is now active')
        99018 format (a)
        !
        !--------------------------------------------
        99019 format (/' * Polar is being accumulated.'/' * Cannot change its parameters in midstream.')
    end subroutine oper
    !*==FCPMIN.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! OPER


    subroutine fcpmin
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
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
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !------------------------------------------------
        !     Finds minimum Cp on dist for cavitation work
        !------------------------------------------------
        !
        XCPmni = X(1)
        XCPmnv = X(1)
        CPMni = CPI(1)
        CPMnv = CPV(1)
        !
        do i = 2, N + NW
            if (CPI(i)<CPMni) then
                XCPmni = X(i)
                CPMni = CPI(i)
            endif
            if (CPV(i)<CPMnv) then
                XCPmnv = X(i)
                CPMnv = CPV(i)
            endif
        enddo
        !

        if (LVIsc) then
            CPMn = CPMnv
        else
            CPMn = CPMni
            !
            CPMnv = CPMni
            XCPmnv = XCPmni
        endif
        !
    end subroutine fcpmin
    !*==MRSHOW.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! FCPMIN



    subroutine mrshow(Lm, Lr)
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        logical :: Lm, Lr
        intent (in) Lm, Lr
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        if (Lm .or. Lr .and. show_output) write (*, *)
        !
        if (Lm) then
            if (show_output) then
                write (*, 99001) MINf1
                write (*, 99001) MINf1, ' / sqrt(CL)'
                if (MATyp==3) write (*, 99001) MINf1, ' / CL'
            endif
        endif
        !
        if (Lr) then
            if (show_output) then
                write (*, 99002) REInf1
                write (*, 99002) REInf1, ' / sqrt(CL)'
                if (RETyp==3) write (*, 99002) REInf1, ' / CL'
            endif
        endif
        !
        return
        !
        99001 format (1x, 'M  =', f10.4, a)
        99002 format (1x, 'Re =', g12.4, a)
    end subroutine mrshow
    !*==NAMMOD.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! MRSHOW



    subroutine nammod(Name, Kdel, Kmod0)
        use i_xfoil, only: show_output
        use m_userio, only: strip
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Kdel, Kmod0
        character(*) :: Name
        intent (in) Kdel, Kmod0
        intent (inout) Name
        !
        ! Local variables
        !
        integer :: kbrack1, kbrack2, kmod, kmodp, nname
        character(48) :: namdef
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
        !     Requests new modified NAME with
        !     version number in brackets, e.g.
        !            NACA 0012  [5]
        !
        !     If bracketed index exists in NAME,
        !        it is incremented by KDEL.
        !     If no bracketed index exists, it
        !        is added with initial value KMOD0,
        !        unless KMOD0 is negative in which
        !        case nothing is added.
        !-------------------------------------------
        !
        call strip(Name, nname)
        kbrack1 = index(Name, '[')
        kbrack2 = index(Name, ']')
        !
        namdef = Name(1:nname)
        !
        if (kbrack1/=0 .and. kbrack2/=0 .and. kbrack2 - kbrack1>1) then
            !----- brackets exist... get number, (go get user's input on READ error)
            read (Name(kbrack1 + 1:kbrack2 - 1), *, err = 100) kmod
            kmod = iabs(kmod)
            kmodp = mod(kmod + Kdel, 100)
            if (kbrack1>=2) then
                Name = Name(1:kbrack1 - 1)
            else
                Name = ' '
            endif
            call strip(Name, nname)
        elseif (Kmod0>0) then
            kmodp = mod(Kmod0, 100)
        else
            kmodp = 0
        endif
        !
        if (kmodp>=10) then
            namdef = Name(1:nname) // ' [  ]'
            write (namdef(nname + 3:nname + 4), 99001) kmodp
            99001 format (i2)
        elseif (kmodp>=1) then
            namdef = Name(1:nname) // ' [ ]'
            write (namdef(nname + 3:nname + 3), 99002) kmodp
            99002 format (i1)
        endif
        !
        100  if (show_output) write (*, 99003) namdef
        99003 format (/' Enter airfoil name or <return> for default:  ', a)
        read (*, 99004) Name
        99004 format (a)
        if (Name==' ') Name = namdef
        !
    end subroutine nammod
    !*==BLDUMP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! NAMMOD



    subroutine bldump(Fname1)
        use s_xfoil, only: comset
        use m_xblsys, only: hkin
        use m_userio, only: bstrip, asks, strip
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Fname1
        intent (in) Fname1
        !
        ! Local variables
        !
        real :: amsq, cdis, cf, ds, dummy, h, hk, hs, hstinv, th, ts, ue, ui
        character(1) :: delim
        character(80) :: fildef
        integer :: i, ibl, is, lu, nfn, nline
        character(256) :: line
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
        !
        if (KDElim==0) then
            delim = ' '
        elseif (KDElim==1) then
            delim = ','
        elseif (KDElim==2) then
            delim = char(9)
        else
            if (show_output) write (*, *) '? Illegal delimiter.  Using blank.'
            delim = ' '
        endif
        !
        if (Fname1(1:1)/=' ') then
            FNAme = Fname1
            !----- no argument... get it somehow
        elseif (NPRefix>0) then
            !------ offer default using existing prefix
            fildef = PREfix(1:NPRefix) // '.bl'
            if (show_output) write (*, 99001) fildef
            99001 format (/' Enter filename:  ', a)
            read (*, 99002) FNAme
            call strip(FNAme, nfn)
            if (nfn==0) FNAme = fildef
        else
            !------ nothing available... just ask for filename
            call asks('Enter filename^', FNAme)
        endif
        !
        lu = 19
        open (lu, file = FNAme, status = 'UNKNOWN')
        rewind (lu)
        !
        if (KDElim==0) then
            write (lu, 99002) '#    s        x        y     Ue/Vinf    Dstar     Theta ', '     Cf       H', &
                    &'       H*        P         m          K          tau         Di'
            !         1.23456  0.23451  0.23451  0.23451  0.012345  0.001234  0.004123  10.512
        else
            write (lu, 99002) '#s', delim, 'x', delim, 'y', delim, 'Ue/Vinf', delim, 'Dstar', delim, 'Theta', delim, &
                    &'Cf', delim, 'H'
        endif
        !
        call comset
        hstinv = GAMm1 * (MINf / QINf)**2 / (1.0 + 0.5 * GAMm1 * MINf**2)
        !
        do i = 1, N
            is = 1
            if (GAM(i)<0.0) is = 2
            !
            if (LIPan .and. LVIsc) then
                if (is==1) then
                    ibl = IBLte(is) - i + 1
                else
                    ibl = IBLte(is) + i - N
                endif
                ds = DSTr(ibl, is)
                th = THEt(ibl, is)
                ts = TSTr(ibl, is)
                cf = TAU(ibl, is) / (0.5 * QINf**2)
                cdis = DIS(ibl, is) / (QINf**3)
                if (th==0.0) then
                    h = 1.0
                    hs = 1.0
                else
                    h = ds / th
                    hs = ts / th
                endif
            else
                ds = 0.
                th = 0.
                ts = 0.
                cf = 0.
                h = 1.0
                hs = 2.0
            endif
            ue = (GAM(i) / QINf) * (1.0 - TKLam) / (1.0 - TKLam * (GAM(i) / QINf)**2)
            amsq = ue * ue * hstinv / (GAMm1 * (1.0 - 0.5 * ue * ue * hstinv))
            call hkin(h, amsq, hk, dummy, dummy)
            !
            if (KDElim==0) then
                write (lu, 99003) S(i), X(i), Y(i), ue, ds, th, cf, hk, hs, th * ue**2, ds * ue, ts * ue**3
                !
            else
                write (line, 99004) S(i), delim, X(i), delim, Y(i), delim, ue, delim, ds, delim, th, delim, cf, &
                        & delim, hk
                call bstrip(line, nline)
                write (lu, 99002) line(1:nline)
            endif
            !
        enddo
        !
        if (LWAke) then
            is = 2
            do i = N + 1, N + NW
                ibl = IBLte(is) + i - N
                ds = DSTr(ibl, is)
                th = THEt(ibl, is)
                h = ds / th
                cf = 0.
                ui = UEDg(ibl, is)
                ue = (ui / QINf) * (1.0 - TKLam) / (1.0 - TKLam * (ui / QINf)**2)
                amsq = ue * ue * hstinv / (GAMm1 * (1.0 - 0.5 * ue * ue * hstinv))
                call hkin(h, amsq, hk, dummy, dummy)
                !
                if (KDElim==0) then
                    write (lu, 99003) S(i), X(i), Y(i), ue, ds, th, cf, hk
                    !
                else
                    write (line, 99004) S(i), delim, X(i), delim, Y(i), delim, ue, delim, ds, delim, th, delim, cf, &
                            & delim, hk
                    call bstrip(line, nline)
                    write (lu, 99002) line(1:nline)
                endif
            enddo
        endif
        !
        close (lu)
        !
        99002 format (20(a))
        !    &     TAU(IBL,IS), DIS(IBL,IS), cdis
        99003 format (1x, 4F9.5, 3F10.6, f10.4, f10.4, 3F9.5, 2F10.6, f10.6)
        99004 format (1x, 4(f9.5, a), 3(f10.6, a), f10.4)
    end subroutine bldump
    !*==BLDUMP2.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! BLDUMP



    subroutine bldump2(Fname1)
        use s_xfoil, only: comset
        use m_xblsys, only: hkin
        use m_userio, only: asks, strip
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Fname1
        intent (in) Fname1
        !
        ! Local variables
        !
        real :: amsq, cdis, cf, ct, disa, dk, dmds, dprei, ds, duds, dummy, dxssi, edisi, h, havg, hk, hs, &
                & hstinv, pprei, ptaui, rt, sqfac, taua, th, ts, ue, xs
        character(1) :: delim
        real, dimension(IVX, ISX) :: edis, ptau
        character(80) :: fildef
        integer :: i, ibl, iblm, iblp, is, lu, nfn
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
        !
        !
        if (KDElim==0) then
            delim = ' '
        elseif (KDElim==1) then
            delim = ','
        elseif (KDElim==2) then
            delim = char(9)
        else
            if (show_output) write (*, *) '? Illegal delimiter.  Using blank.'
            delim = ' '
        endif
        !
        if (Fname1(1:1)/=' ') then
            FNAme = Fname1
            !----- no argument... get it somehow
        elseif (NPRefix>0) then
            !------ offer default using existing prefix
            fildef = PREfix(1:NPRefix) // '.bl'
            if (show_output) write (*, 99001) fildef
            99001 format (/' Enter filename:  ', a)
            read (*, 99003) FNAme
            call strip(FNAme, nfn)
            if (nfn==0) FNAme = fildef
        else
            !------ nothing available... just ask for filename
            call asks('Enter filename^', FNAme)
        endif
        !
        lu = 19
        open (lu, file = FNAme, status = 'UNKNOWN')
        rewind (lu)
        !
        write (lu, 99003) '#    s        x        y      Ue/Vinf ', '   Dstar      Theta      Tstar', &
                &'       Cf         CD         H         H*  ', &
                &'       m          P         Pf        Pp         K/2  ', &
                &'       tau       Diss    -m du/ds    dP/ds      DV    ', '     dm/ds     N     Rtheta'
        write (lu, 99003) '#    1        2        3        4     ', '     5          6          7  ', &
                &'        8         9          10        11  ', &
                &'       12         13        14        15         16   ', &
                &'       17         18        19        20        21    ', '      22      23      24   '
        !         1.23456  0.23451  0.23451  0.23451  0.012345  0.001234  0.004123  10.512
        !
        call comset
        hstinv = GAMm1 * (MINf / QINf)**2 / (1.0 + 0.5 * GAMm1 * MINf**2)
        !
        do is = 1, 2
            ibl = 1
            ptau(ibl, is) = 0.
            edis(ibl, is) = 0.
            do ibl = 2, IBLte(is)
                dxssi = XSSi(ibl, is) - XSSi(ibl - 1, is)
                taua = (TAU(ibl, is) + TAU(ibl - 1, is)) * 0.5
                disa = (DIS(ibl, is) + DIS(ibl - 1, is)) * 0.5
                ptau(ibl, is) = ptau(ibl - 1, is) + taua * dxssi
                edis(ibl, is) = edis(ibl - 1, is) + disa * dxssi
            enddo
        enddo
        is = 2
        ibl = IBLte(is) + 1
        ptau(ibl, is) = ptau(IBLte(1), 1) + ptau(IBLte(2), 2)
        edis(ibl, is) = edis(IBLte(1), 1) + edis(IBLte(2), 2)
        do ibl = IBLte(is) + 2, NBL(is)
            dxssi = XSSi(ibl, is) - XSSi(ibl - 1, is)
            taua = (TAU(ibl, is) + TAU(ibl - 1, is)) * 0.5
            disa = (DIS(ibl, is) + DIS(ibl - 1, is)) * 0.5
            ptau(ibl, is) = ptau(ibl - 1, is) + taua * dxssi
            edis(ibl, is) = edis(ibl - 1, is) + disa * dxssi
        enddo
        !
        !
        do is = 1, 2
            do ibl = 2, NBL(is)
                i = IPAn(ibl, is)

                if (ibl==2) then
                    iblm = ibl
                else
                    iblm = ibl - 1
                endif

                if (ibl==IBLte(is)) then
                    iblp = ibl
                else
                    iblp = ibl + 1
                endif

                if (is==2) then
                    if (ibl==IBLte(is) + 1) iblm = ibl
                    if (ibl==NBL(is)) iblp = ibl
                endif

                xs = XSSi(ibl, is)
                if (is==2 .and. ibl>IBLte(2)) xs = xs - XSSi(IBLte(2), 2) + XSSi(IBLte(1), 1)

                if (LIPan .and. LVIsc) then
                    ue = UEDg(ibl, is)
                    ds = DSTr(ibl, is)
                    th = THEt(ibl, is)
                    ts = TSTr(ibl, is)
                    cf = TAU(ibl, is) / (0.5 * QINf**2)
                    cdis = DIS(ibl, is) / (QINf**3)
                    ptaui = ptau(ibl, is)
                    pprei = th * ue**2 - ptaui
                    edisi = edis(ibl, is)
                    dprei = 0.5 * CPV(i)
                    dprei = (1.0 - ue**2) * 0.5
                    havg = 0.5 * (ds / th + 1.0)
                    sqfac = ue**havg
                    if (th==0.0) then
                        h = 1.0
                        hs = 1.0
                    else
                        h = ds / th
                        hs = ts / th
                    endif
                    amsq = ue * ue * hstinv / (GAMm1 * (1.0 - 0.5 * ue * ue * hstinv))
                    call hkin(h, amsq, hk, dummy, dummy)
                    duds = (UEDg(iblp, is) - UEDg(iblm, is)) / (XSSi(iblp, is) - XSSi(iblm, is))
                    dmds = (UEDg(iblp, is) * DSTr(iblp, is) &
                            - UEDg(iblm, is) * DSTr(iblm, is)) / (XSSi(iblp, is) - XSSi(iblm, is))
                    dk = hk * THEt(ibl, is)
                    ct = CTAu(ibl, is)
                    rt = ue * th * REInf
                else
                    ue = (GAM(i) / QINf) * (1.0 - TKLam) / (1.0 - TKLam * (GAM(i) / QINf)**2)
                    ds = 0.
                    th = 0.
                    ts = 0.
                    cf = 0.
                    cdis = 0.
                    ptaui = 0.
                    pprei = 0.
                    edisi = 0.
                    dprei = 0.
                    sqfac = 1.0
                    h = 1.0
                    hs = 1.0
                    hk = 1.0
                    duds = 0.
                    dmds = 0.
                    dk = 0.
                    ct = 0.
                    rt = 0.
                endif
                !
                write (lu, 99002) xs, X(i), Y(i), ue, ds, th, ts, cf, cdis, hk, hs, ds * ue, th * ue**2, ptaui, pprei, &
                        & 0.5 * ts * ue**3, TAU(ibl, is), DIS(ibl, is), -ue * ds * duds, TAU(ibl, is) - ue * ds * duds, &
                        & th * ue**2 - dprei * dk, dmds, ct, rt
                99002  format (1x, 4F9.5, 3F11.7, 2F11.7, 2F10.4, 5F11.7, 4F11.7, f11.7, f11.7, f11.7, f11.3)
                !
                if (is==2 .and. ibl==IBLte(2)) write (lu, *)
            enddo
            if (is==1) write (lu, *)

        enddo
        !
        close (lu)
        !
        99003 format (20(a))
    end subroutine bldump2
    !*==CPDUMP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! BLDUMP2



    subroutine cpdump(Fname1)
        use s_xfoil, only: comset
        use m_userio, only: bstrip, asks, strip
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Fname1
        intent (in) Fname1
        !
        ! Local variables
        !
        real :: beta, bfac, cpcom, cpinc, den
        character(1) :: delim
        character(80) :: fildef
        integer :: i, lu, nfn, nline
        character(128) :: line
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
        !
        if (KDElim==0) then
            delim = ' '
        elseif (KDElim==1) then
            delim = ','
        elseif (KDElim==2) then
            delim = char(9)
        else
            if (show_output) write (*, *) '? Illegal delimiter.  Using blank.'
            delim = ' '
        endif
        !
        if (Fname1(1:1)/=' ') then
            FNAme = Fname1
            !----- no argument... get it somehow
        elseif (NPRefix>0) then
            !------ offer default using existing prefix
            fildef = PREfix(1:NPRefix) // '.cp'
            if (show_output) write (*, 99001) fildef
            99001 format (/' Enter filename:  ', a)
            read (*, 99004) FNAme
            call strip(FNAme, nfn)
            if (nfn==0) FNAme = fildef
        else
            !------ nothing available... just ask for filename
            call asks('Enter filename^', FNAme)
        endif
        !
        !
        lu = 19
        open (lu, file = FNAme, status = 'UNKNOWN')
        rewind (lu)
        !
        if (KDElim==0) then
            write (lu, 99004) '#      x          Cp  '
            !            0.23451    0.23451
        else
            write (lu, 99004) '#x', delim, 'Cp'
            !
        endif
        !
        call comset
        !
        beta = sqrt(1.0 - MINf**2)
        bfac = 0.5 * MINf**2 / (1.0 + beta)
        !
        do i = 1, N
            cpinc = 1.0 - (GAM(i) / QINf)**2
            den = beta + bfac * cpinc
            cpcom = cpinc / den
            !
            if (KDElim==0) then
                write (lu, 99002) X(i), cpcom
                99002  format (1x, 2F11.5)
            else
                write (line, 99003) X(i), delim, cpcom
                99003  format (1x, 2(f11.5, a))
                call bstrip(line, nline)
                write (lu, 99004) line(1:nline)
            endif
        enddo
        !
        close (lu)
        !
        99004 format (8a)
    end subroutine cpdump
    !*==MHINGE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! CPDUMP



    subroutine vpar
        use m_xbl, only: blpini
        use m_userio, only: getflt, askc, askr, getint
        use i_xfoil
        use i_blpar
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        character(4) :: comand
        character(128) :: comarg
        logical :: error
        integer :: i, ninput
        integer, dimension(20) :: iinput
        real, dimension(20) :: rinput
        real, dimension(ISX) :: turb
        real :: waklen0
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        do
            !
            !*** End of declarations rewritten by SPAG
            !
            !---------------------------------------------
            !     Viscous parameter change menu routine.
            !---------------------------------------------
            !
            !
            !
            turb(1) = 100.0 * exp(-(ACRit(1) + 8.43) / 2.4)
            turb(2) = 100.0 * exp(-(ACRit(2) + 8.43) / 2.4)
            if (show_output) then
                write (*, 99001) XSTrip(1), XSTrip(2), ACRit(1), turb(1), ACRit(2), turb(2), VACcel, WAKlen, SCCon, DUXcon, &
                        & DLCon, GACon, GBCon, CTCon, CTRcon, CTRcex
                99001 format (&
                        /' Xtr/c     =', F8.4, '    top    side'&
                        /' Xtr/c     =', F8.4, '    bottom side'&
                        /' NcritT    =', F8.2, '   (', F6.3, ' % turb. level )'&
                        /' NcritB    =', F8.2, '   (', F6.3, ' % turb. level )'&
                        /' Vacc      =', F8.4, &
                        /' WakeL/c   =', F8.3, &
                        //' Klag  =', F8.4, '     Uxwt  =', F8.2, '       Kdl =', F8.4&
                        /' A     =', F8.4, '     B     =', F8.4, '       KCt =', F8.5&
                        /' CtiniK=', F8.4, '     CtiniX=', F8.4)
            end if
            do
                !
                !======================================================================
                !---- start of user interaction loop
                call askc('..VPAR^', comand, comarg)
                !
                do i = 1, 20
                    iinput(i) = 0
                    rinput(i) = 0.0
                enddo
                ninput = 20
                call getint(comarg, iinput, ninput, error)
                ninput = 20
                call getflt(comarg, rinput, ninput, error)
                !
                !--------------------------------------------------------------
                if (comand=='    ') then
                    return
                    !
                    !--------------------------------------------------------------
                elseif (comand=='?   ') then
                    if (show_output) write (*, 99002)
                    99002      format (&
                            /'   <cr>    Return to OPER menu'&
                            /'   SHOW    Display viscous parameters'&
                            /'   Xtr  rr Change trip positions Xtr/c'&
                            /'   N    r  Change critical amplification exponent Ncrit'&
                            /'   NT   r  Change Ncrit on Top'&
                            /'   NB   r  Change Ncrit on Bot'&
                            /'   Vacc r  Change Newton solution acceleration parameter'&
                            /'   Wake r  Change wake length/chord'&
                            /'   INIT    BL initialization flag toggle'&
                            //'   LAG     change lag equation constants'&
                            /'   GB      change G-beta constants A,B'&
                            /'   CTR     change initial transition-Ctau constants'&
                            /'   REST    restore BL calibration to baseline')
                    !
                    !--------------------------------------------------------------
                elseif (comand=='SHOW') then
                    goto 100
                    !
                    !--------------------------------------------------------------
                elseif (comand=='XTR ' .or. comand=='X   ') then
                    if (LPAcc .and. LVIsc) then
                        if (show_output) write (*, 99004)
                        cycle
                    endif
                    if (ninput>=2) then
                        XSTrip(1) = rinput(1)
                        XSTrip(2) = rinput(2)
                    else
                        call askr('Enter top    side Xtrip/c^', XSTrip(1))
                        call askr('Enter bottom side Xtrip/c^', XSTrip(2))
                    endif
                    LVConv = .false.
                    !
                    !--------------------------------------------------------------
                elseif (comand=='N   ') then
                    if (LPAcc .and. LVIsc) then
                        if (show_output) write (*, 99004)
                        cycle
                    endif
                    if (ninput>=1) then
                        ACRit(1) = rinput(1)
                        ACRit(2) = rinput(1)
                    else
                        call askr('Enter critical amplification ratio^', ACRit(1))
                        ACRit(2) = ACRit(1)
                    endif
                    LVConv = .false.
                    !
                    !--------------------------------------------------------------
                elseif (comand=='NT  ') then
                    if (LPAcc .and. LVIsc) then
                        if (show_output) write (*, 99004)
                        cycle
                    endif
                    if (ninput>=1) then
                        ACRit(1) = rinput(1)
                    else
                        call askr('Enter top-surface critical amplification^', ACRit(1))
                    endif
                    LVConv = .false.
                    !
                    !--------------------------------------------------------------
                elseif (comand=='NB  ') then
                    if (LPAcc .and. LVIsc) then
                        if (show_output) write (*, 99004)
                        cycle
                    endif
                    if (ninput>=1) then
                        ACRit(2) = rinput(1)
                    else
                        call askr('Enter bot-surface critical amplification^', ACRit(2))
                    endif
                    LVConv = .false.
                    !
                    !--------------------------------------------------------------
                elseif (comand=='VACC' .or. comand=='V   ') then
                    if (ninput>=1) then
                        VACcel = rinput(1)
                    else
                        call askr('Enter viscous acceleration parameter^', VACcel)
                    endif
                    !
                    !--------------------------------------------------------------
                elseif (comand=='WAKE' .or. comand=='W   ') then
                    waklen0 = WAKlen
                    if (ninput>=1) then
                        WAKlen = rinput(1)
                    else
                        call askr('Enter wake length/chord^', WAKlen)
                    endif
                    LWAke = .false.
                    LBLini = .false.
                    LVConv = .false.
                    LIPan = .false.
                    !
                    !--------------------------------------------------------------
                elseif (comand=='INIT') then
                    LBLini = .not.LBLini
                    if (show_output) then
                        write (*, *) 'BLs will be initialized on next point'
                        write (*, *) 'BLs are assumed to be initialized'
                    endif
                    if (.not.LBLini) LIPan = .false.
                    !
                    !--------------------------------------------------------------
                elseif (comand=='LAG ') then
                    if (LPAcc .and. LVIsc) then
                        if (show_output) write (*, 99004)
                        cycle
                    endif
                    if (ninput>=3) then
                        SCCon = rinput(1)
                        DUXcon = rinput(2)
                        DLCon = rinput(3)
                    else
                        call askr('Enter shear lag constant^', SCCon)
                        call askr('Enter shear lag UxEQ weight^', DUXcon)
                        call askr('Enter wake 1/dissipation-length factor^', DLCon)
                    endif
                    !
                    !--------------------------------------------------------------
                elseif (comand=='GB  ') then
                    if (LPAcc .and. LVIsc) then
                        if (show_output) write (*, 99004)
                        cycle
                    endif
                    if (ninput>=2) then
                        GACon = rinput(1)
                        GBCon = rinput(2)
                    else
                        call askr('Enter G-beta constant A^', GACon)
                        call askr('Enter G-beta constant B^', GBCon)
                    endif
                    CTCon = 0.5 / (GACon**2 * GBCon)
                    !
                    !--------------------------------------------------------------
                elseif (comand=='CTR ') then
                    if (LPAcc .and. LVIsc) then
                        if (show_output) write (*, 99004)
                        cycle
                    endif
                    if (ninput>=2) then
                        CTRcon = rinput(1)
                        CTRcex = rinput(2)
                    else
                        call askr('Enter initial-Ctau constant^', CTRcon)
                        call askr('Enter initial-Ctau exponent^', CTRcex)
                    endif
                    !
                    !--------------------------------------------------------------
                elseif (comand=='CFAC') then
                    if (ninput>=1) then
                        CFFac = rinput(1)
                    else
                        call askr('Enter Cf scaling factor^', CFFac)
                    endif
                    !
                    !--------------------------------------------------------------
                elseif (comand=='REST') then
                    if (LPAcc .and. LVIsc) then
                        if (show_output) write (*, 99004)
                        cycle
                    endif
                    call blpini
                    !
                    !--------------------------------------------------------------
                else
                    if (show_output) write (*, 99003) comand
                    99003      format (1x, a4, ' command not recognized.  Type a "?" for list')
                    !
                    !
                endif
            enddo
            exit
        100  enddo
        !--------------------------------------------
        99004 format (/' * Polar is being accumulated.'/' * Cannot change its parameters in midstream.')
    end subroutine vpar
    !*==SPECAL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! VPAR




    subroutine specal
        use m_xpanel, only: qiset, ggcalc
        use i_xfoil
        use s_xfoil, only: mrcl, comset, cpcalc, clcalc, tecalc
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: clm, clm1, dclm, minf_clm, msq_clm, reinf_clm
        integer :: i, irlx, itcl
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-----------------------------------
        !     Converges to specified alpha.
        !-----------------------------------
        !
        !---- calculate surface vorticity distributions for alpha = 0, 90 degrees
        if (.not.LGAmu .or. .not.LQAij) call ggcalc
        !
        COSa = cos(ALFa)
        SINa = sin(ALFa)
        !
        !---- superimpose suitably weighted  alpha = 0, 90  distributions
        do i = 1, N
            GAM(i) = COSa * GAMu(i, 1) + SINa * GAMu(i, 2)
            GAM_a(i) = -SINa * GAMu(i, 1) + COSa * GAMu(i, 2)
        enddo
        PSIo = COSa * GAMu(N + 1, 1) + SINa * GAMu(N + 1, 2)
        !
        call tecalc
        call qiset
        !
        !---- set initial guess for the Newton variable CLM
        clm = 1.0
        !
        !---- set corresponding  M(CLM), Re(CLM)
        call mrcl(clm, minf_clm, reinf_clm)
        call comset
        !
        !---- set corresponding CL(M)
        call clcalc(N, X, Y, GAM, GAM_a, ALFa, MINf, QINf, XCMref, YCMref, CL, CM, CDP, CL_alf, CL_msq)
        !
        !---- iterate on CLM
        do itcl = 1, 20
            !
            msq_clm = 2.0 * MINf * minf_clm
            dclm = (CL - clm) / (1.0 - CL_msq * msq_clm)
            !
            clm1 = clm
            RLX = 1.0
            !
            !------ under-relaxation loop to avoid driving M(CL) above 1
            do irlx = 1, 12
                !
                clm = clm1 + RLX * dclm
                !
                !-------- set new freestream Mach M(CLM)
                call mrcl(clm, minf_clm, reinf_clm)
                !
                !-------- if Mach is OK, go do next Newton iteration
                if (MATyp==1 .or. MINf==0.0 .or. minf_clm/=0.0) exit
                !
                RLX = 0.5 * RLX
            enddo
            !
            !------ set new CL(M)
            call comset
            call clcalc(N, X, Y, GAM, GAM_a, ALFa, MINf, QINf, XCMref, YCMref, CL, CM, CDP, CL_alf, CL_msq)
            !
            if (abs(dclm)<=1.0E-6) goto 100
            !
        enddo
        if (show_output) write (*, *) 'SPECAL:  Minf convergence failed'
        !
        !---- set final Mach, CL, Cp distributions, and hinge moment
        100  call mrcl(CL, MINf_cl, REInf_cl)
        call comset
        call clcalc(N, X, Y, GAM, GAM_a, ALFa, MINf, QINf, XCMref, YCMref, CL, CM, CDP, CL_alf, CL_msq)
        call cpcalc(N, QINv, QINf, MINf, CPI)
        if (LVIsc) then
            call cpcalc(N + NW, QVIs, QINf, MINf, CPV)
            call cpcalc(N + NW, QINv, QINf, MINf, CPI)
        else
            call cpcalc(N, QINv, QINf, MINf, CPI)
        endif
        if (LFLap) call mhinge
        !
    end subroutine specal
    !*==SPECCL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! SPECAL


    subroutine speccl
        use m_xpanel, only: qiset, ggcalc
        use i_xfoil
        use s_xfoil, only: mrcl, comset, clcalc, cpcalc, tecalc
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: dalfa
        integer :: i, ital
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !-----------------------------------------
        !     Converges to specified inviscid CL.
        !-----------------------------------------
        !
        !---- calculate surface vorticity distributions for alpha = 0, 90 degrees
        if (.not.LGAmu .or. .not.LQAij) call ggcalc
        !
        !---- set freestream Mach from specified CL -- Mach will be held fixed
        call mrcl(CLSpec, MINf_cl, REInf_cl)
        call comset
        !
        !---- current alpha is the initial guess for Newton variable ALFA
        COSa = cos(ALFa)
        SINa = sin(ALFa)
        do i = 1, N
            GAM(i) = COSa * GAMu(i, 1) + SINa * GAMu(i, 2)
            GAM_a(i) = -SINa * GAMu(i, 1) + COSa * GAMu(i, 2)
        enddo
        PSIo = COSa * GAMu(N + 1, 1) + SINa * GAMu(N + 1, 2)
        !
        !---- get corresponding CL, CL_alpha, CL_Mach
        call clcalc(N, X, Y, GAM, GAM_a, ALFa, MINf, QINf, XCMref, YCMref, CL, CM, CDP, CL_alf, CL_msq)
        !
        !---- Newton loop for alpha to get specified inviscid CL
        do ital = 1, 20
            !
            dalfa = (CLSpec - CL) / CL_alf
            RLX = 1.0
            !
            ALFa = ALFa + RLX * dalfa
            !
            !------ set new surface speed distribution
            COSa = cos(ALFa)
            SINa = sin(ALFa)
            do i = 1, N
                GAM(i) = COSa * GAMu(i, 1) + SINa * GAMu(i, 2)
                GAM_a(i) = -SINa * GAMu(i, 1) + COSa * GAMu(i, 2)
            enddo
            PSIo = COSa * GAMu(N + 1, 1) + SINa * GAMu(N + 1, 2)
            !
            !------ set new CL(alpha)
            call clcalc(N, X, Y, GAM, GAM_a, ALFa, MINf, QINf, XCMref, YCMref, CL, CM, CDP, CL_alf, CL_msq)
            !
            if (abs(dalfa)<=1.0E-6) goto 100
        enddo
        if (show_output) write (*, *) 'SPECCL:  CL convergence failed'
        !
        !---- set final surface speed and Cp distributions
        100  call tecalc
        call qiset
        if (LVIsc) then
            call cpcalc(N + NW, QVIs, QINf, MINf, CPV)
            call cpcalc(N + NW, QINv, QINf, MINf, CPI)
        else
            call cpcalc(N, QINv, QINf, MINf, CPI)
        endif
        if (LFLap) call mhinge
        !
    end subroutine speccl
    !*==VISCAL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! SPECCL


    function viscal(Niter1)
        use m_xpanel, only: qiset, qvfue, iblpan, qwcalc, uicalc, xicalc, gamqv, stfind, stmove, xywake, qdcalc
        use m_xbl, only: update, setbl
        use m_xsolve, only: blsolv
        use m_userio, only: aski
        use i_xfoil
        use s_xbl, only: iblsys
        use s_xfoil, only: mrcl, comset, cdcalc, cpcalc, clcalc
        implicit none

        logical :: viscal
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Niter1
        intent (in) Niter1
        !
        ! Local variables
        !
        real :: cdpdif, delp, hfrac, hki, hkm, hkmax, patt, pdefi, pdefm, psep
        real, save :: eps1
        integer :: i, ibl, is, iter, niter

! These are used by the weird writing to a file code
!        real :: ddef, dpds, duds, edef, fnum, hk, pdef
!        integer :: i, iblm, iblp, idec, ione, iten, izero, lu
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
        !     Converges viscous operating point
        !----------------------------------------
        !
        !---- convergence tolerance
        data eps1/1.0E-4/
        viscal = .true.
        !
        niter = Niter1
        !
        !---- calculate wake trajectory from current inviscid solution if necessary
        if (.not.LWAke) call xywake
        !
        !---- set velocities on wake from airfoil vorticity for alpha=0, 90
        call qwcalc
        !
        !---- set velocities on airfoil and wake for initial alpha
        call qiset
        !
        if (.not.LIPan) then
            !
            if (LBLini) call gamqv
            !
            !----- locate stagnation point arc length position and panel index
            call stfind
            !
            !----- set  BL position -> panel position  pointers
            call iblpan
            !
            !----- calculate surface arc length array for current stagnation point location
            call xicalc
            !
            !----- set  BL position -> system line  pointers
            call iblsys
            !
        endif
        !
        !---- set inviscid BL edge velocity UINV from QINV
        call uicalc
        !
        if (.not.LBLini) then
            !
            !----- set initial Ue from inviscid Ue
            do ibl = 1, NBL(1)
                UEDg(ibl, 1) = UINv(ibl, 1)
            enddo
            !
            do ibl = 1, NBL(2)
                UEDg(ibl, 2) = UINv(ibl, 2)
            enddo
            !
        endif
        !
        if (LVConv) then
            !----- set correct CL if converged point exists
            call qvfue
            if (LVIsc) then
                call cpcalc(N + NW, QVIs, QINf, MINf, CPV)
                call cpcalc(N + NW, QINv, QINf, MINf, CPI)
            else
                call cpcalc(N, QINv, QINf, MINf, CPI)
            endif
            call gamqv
            call clcalc(N, X, Y, GAM, GAM_a, ALFa, MINf, QINf, XCMref, YCMref, CL, CM, CDP, CL_alf, CL_msq)
            call cdcalc
        endif
        !
        !---- set up source influence matrix if it doesn't exist
        if (.not.LWDij .or. .not.LADij) call qdcalc
        !
        !---- Newton iteration for entire BL solution
        if (niter==0) call aski('Enter number of iterations^', niter)
        if (show_output) then
            write (*, *)
            write (*, *) 'Solving BL system ...'
        endif
        do iter = 1, niter
            !
            !------ fill Newton system for BL variables
            viscal = setbl()
            if (abort_on_nan .and. .not. viscal) return
            !
            !------ solve Newton system with custom solver
            call blsolv
            !
            !------ update BL variables
            call update
            !
            if (LALfa) then
                !------- set new freestream Mach, Re from new CL
                call mrcl(CL, MINf_cl, REInf_cl)
                call comset
            else
                !------- set new inviscid speeds QINV and UINV for new alpha
                call qiset
                call uicalc
            endif
            !
            !------ calculate edge velocities QVIS(.) from UEDG(..)
            call qvfue
            !
            !------ set GAM distribution from QVIS
            call gamqv
            !
            !------ relocate stagnation point
            call stmove
            !
            !------ set updated CL,CD
            call clcalc(N, X, Y, GAM, GAM_a, ALFa, MINf, QINf, XCMref, YCMref, CL, CM, CDP, CL_alf, CL_msq)
            call cdcalc
            !
            !------ display changes and test for convergence
            if (RLX<1.0 .and. show_output) write (*, 99001) iter, RMSbl, RMXbl, VMXbl, IMXbl, ISMxbl, RLX
            !....................................................................
            99001 format (/1x, i3, '   rms: ', e10.4, '   max: ', e10.4, 3x, a1, ' at ', i4, i3, '   RLX:', f6.3)
            if (RLX==1.0 .and. show_output) write (*, 99002) iter, RMSbl, RMXbl, VMXbl, IMXbl, ISMxbl
            99002 format (/1x, i3, '   rms: ', e10.4, '   max: ', e10.4, 3x, a1, ' at ', i4, i3)
            cdpdif = CD - CDF
            if (show_output) write (*, 99003) ALFa / DTOr, CL, CM, CD, CDF, cdpdif
            99003 format (1x, 3x, '   a =', f7.3, '      CL =', f8.4/1x, 3x, '  Cm =', f8.4, &
                    '     CD =', f9.5, '   =>   CDf =', f9.5, &
                    &'    CDp =', f9.5)
            !         CDSURF = CDP + CDF
            !         WRITE(*,2025) CDSURF, CDF, CDP

            if (RMSbl<eps1) then
                LVConv = .true.
                AVIsc = ALFa
                MVIsc = MINf
                goto 100
            endif
            !
        enddo
        if (show_output) write (*, *) 'VISCAL:  Convergence failed'
        !
        100  call cpcalc(N + NW, QINv, QINf, MINf, CPI)
        call cpcalc(N + NW, QVIs, QINf, MINf, CPV)
        if (LFLap) call mhinge

        is = 1
        hkmax = 0.
        hkm = 0.0
        psep = 0.
        patt = 0.
        do ibl = 2, IBLte(is)
            hki = DSTr(ibl, is) / THEt(ibl, is)
            hkmax = max(hki, hkmax)
            if (hkm<4.0 .and. hki>=4.0) then
                hfrac = (4.0 - hkm) / (hki - hkm)
                pdefm = UEDg(ibl - 1, is)**2 * THEt(ibl - 1, is)
                pdefi = UEDg(ibl, is)**2 * THEt(ibl, is)
                psep = pdefm * (1.0 - hfrac) + pdefi * hfrac
            endif
            if (hkm>4.0 .and. hki<4.0) then
                hfrac = (4.0 - hkm) / (hki - hkm)
                pdefm = UEDg(ibl - 1, is)**2 * THEt(ibl - 1, is)
                pdefi = UEDg(ibl, is)**2 * THEt(ibl, is)
                patt = pdefm * (1.0 - hfrac) + pdefi * hfrac
            endif
            hkm = hki
        enddo
        delp = patt - psep

        if (show_output) write (*, 99004) ACRit(is), hkmax, CD, 2.0 * psep, 2.0 * patt, 2.0 * delp, XOCtr(is)
        99004 format (1x, f10.3, f10.4, f11.6, 3F11.6, f10.4, '     #')

! This stuff seems to only write stuff to a file, which seems unecessary to do each time viscal is called...
!        izero = ichar('0')
!
!        !c      fnum = acrit(is)
!        fnum = XSTrip(is) * 100.0
!
!        iten = int(fnum / 9.99999)
!        ione = int((fnum - float(10 * iten)) / 0.99999)
!        idec = int((fnum - float(10 * iten) - float(ione)) / 0.09999)
!
!        FNAme = char(iten + izero) // char(ione + izero) // char(idec + izero) // '.bl'
!        lu = 44
!        open (lu, file = FNAme, status = 'unknown')
!        rewind (lu)
!        write (lu, '(a,a)') '#       s         ue          H          P         K ', '        x    -m du/dx'
!        !       1234567890 1234567890 1234567890 1234567890 1234567890 1234567890
!        do ibl = 2, IBLte(is)
!            iblm = max(ibl - 1, 2)
!            iblp = min(ibl + 1, IBLte(is))
!            i = IPAn(ibl, is)
!            hk = DSTr(ibl, is) / THEt(ibl, is)
!            ddef = DSTr(ibl, is) * UEDg(ibl, is)
!            pdef = THEt(ibl, is) * UEDg(ibl, is)**2
!            edef = TSTr(ibl, is) * UEDg(ibl, is)**3 * 0.5
!            duds = (UEDg(iblp, is) - UEDg(iblm, is)) / (XSSi(iblp, is) - XSSi(iblm, is))
!            dpds = -ddef * duds
!            write (lu, 99005) XSSi(ibl, is), UEDg(ibl, is), hk, pdef, edef, X(i), dpds
!            99005 format (1x, 3F11.4, 2F11.6, f11.3, e14.6)
!        enddo
!        close (lu)

    end function viscal
    !*==DCPOUT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! VISCAL


    subroutine dcpout
        use m_spline, only: seval, sinvrt, spline
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: cpl1, cpl2, cpu1, cpu2, sl1, sl2, su1, su2, x1, x2
        integer :: lu
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !     Computes and writes upper and lower-surface
        !     Cp values at two specified x locations
        !
        !
        x1 = 0.05
        x2 = 0.15
        !
        lu = 60
        open (lu, file = 'dcp.out', status = 'old', access = 'append', err = 100)
        goto 200
        !
        100  open (lu, file = 'dcp.out', status = 'new')
        write (lu, *) '#  ', NAMe
        write (lu, *) '# alpha   CL       ', ' Cpl05     Cpu05     dCp05    ', ' Cpl15     Cpu15     dCp15    '
        !
        200  call spline(CPV, W1, S, N)
        !
        su1 = SLE + x1 * (S(1) - SLE)
        sl1 = SLE + x1 * (S(N) - SLE)
        su2 = SLE + x2 * (S(1) - SLE)
        sl2 = SLE + x2 * (S(N) - SLE)
        !
        call sinvrt(sl1, x1, X, XP, S, N)
        call sinvrt(su1, x1, X, XP, S, N)
        call sinvrt(sl2, x2, X, XP, S, N)
        call sinvrt(su2, x2, X, XP, S, N)
        !
        cpl1 = seval(sl1, CPV, W1, S, N)
        cpu1 = seval(su1, CPV, W1, S, N)
        cpl2 = seval(sl2, CPV, W1, S, N)
        cpu2 = seval(su2, CPV, W1, S, N)
        !
        write (lu, 99001) ALFa / DTOr, CL, cpl1, cpu1, cpl1 - cpu1, cpl2, cpu2, cpl2 - cpu2

        99001 format (1x, f7.3, f9.4, 8F10.5)
        !
        close (lu)
        !
    end subroutine dcpout

end module m_xoper