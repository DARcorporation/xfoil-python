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

!*==MDES.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! HALF
!
module m_xmdes
contains
    subroutine mdes
        use m_xgeom, only: geopar
        use m_userio, only: getflt, askc, asks, getint
        use m_xqdes, only: smooq
        use m_spline, only: splind, scalc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        character(128), save :: argold
        real :: cfilt, clq, g
        character(4) :: comand
        character(128) :: comarg
        character(4), save :: comold
        logical :: error, lcnpl, lrecalc
        integer :: i, k, kqsp, lu, ninput, ntmp, ntqspl
        integer, dimension(20) :: iinput
        character(80) :: line
        real :: qcomp
        real, dimension(20) :: rinput
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
        !------------------------------------
        !     Full-Inverse design routine.
        !     Based on circle plane mapping.
        !------------------------------------
        !
        !
        !
        !
        !
        !
        !---- statement function for compressible Karman-Tsien velocity
        qcomp(g) = g * (1.0 - TKLam) / (1.0 - TKLam * (g / QINf)**2)
        !
        comand = '****'
        comarg = ' '
        lrecalc = .false.
        !
        if (N==0) then
            if (show_output) then
                write (*, *)
                write (*, *) '***  No airfoil available  ***'
            endif
            return
        endif
        !
        lcnpl = .false.
        LSYm = .true.
        !
        ntqspl = 1
        if (LQSlop) ntqspl = 4
        !
        !
        !---- see if current Qspec, if any, didn't come from Mixed-Inverse
        100  if (NSP/=NC1) then
            LQSpec = .false.
            IQ1 = 1
            IQ2 = NC1
        endif
        !
        !---- initialize Fourier transform arrays if it hasn't been done
        if (.not.LEIw) call eiwset(NC1)
        LEIw = .true.
        !
        !---- if Qspec alpha has never been set, set it to current alpha
        if (NQSp==0) then
            IACqsp = 1
            ALQsp(1) = ALFa
            NQSp = 1
        endif
        !
        if (.not.LSCini) then
            !------ initialize s(w) for current airfoil, generating its Cn coefficients
            call scinit(N, X, XP, Y, YP, S, SLE)
            LSCini = .true.
            !
            !------ set up to initialize Qspec to current conditions
            LQSpec = .false.
        endif
        !
        !---- set initial Q for current alpha
        ALGam = ALFa
        call mapgam(1, ALGam, CLGam, CMGam)
        if (show_output) write (*, 99009) ALGam / DTOr, CLGam
        !
        if (.not.LQSpec) then
            !------ set Cn coefficients from current Q
            call cncalc(QGAmm, .false.)
            !
            !------ set Qspec from Cn coefficients
            call qspcir
            if (show_output) write (*, 99001)
            99001 format (/' Qspec initialized to current Q')
        endif
        !
        !====================================================
        !---- start of menu loop
        200  comold = comand
        argold = comarg
        do
            !
            if (LQSym) then
                call askc('.MDESs^', comand, comarg)
            else
                call askc('.MDES^', comand, comarg)
            endif
            do
                !
                !
                !---- process previous command ?
                if (comand(1:1)/='!') then
                    lrecalc = .false.
                elseif (comold=='****') then
                    if (show_output) write (*, *) 'Previous .MDES command not valid'
                    goto 300
                else
                    comand = comold
                    comarg = argold
                    lrecalc = .true.
                endif
                !
                !----- just <return> was typed... clean up plotting and exit OPER
                if (comand=='    ') return
                !
                !---- extract command line numeric arguments
                do i = 1, 20
                    iinput(i) = 0
                    rinput(i) = 0.0
                enddo
                ninput = 0
                call getint(comarg, iinput, ninput, error)
                ninput = 0
                call getflt(comarg, rinput, ninput, error)
                !
                !--------------------------------------------------------
                if (comand=='?   ') then
                    if (show_output) write (*, 99002)
                    99002      format (&
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
                elseif (comand=='INIT') then
                    LQSpec = .false.
                    LSCini = .false.
                    goto 100
                    !
                    !--------------------------------------------------------
                elseif (comand=='QSET') then
                    call cncalc(QGAmm, .false.)
                    if (LQSym) call cnsymm
                    call qspcir
                    !
                    lcnpl = .false.
                    !
                    !--------------------------------------------------------
                elseif (comand=='AQ  ') then
                    !----- set Qspec(s) for specified alphas
                    if (ninput>=1) then
                        NQSp = min(ninput, IPX)
                        do k = 1, NQSp
                            ALQsp(k) = rinput(k) * DTOr
                        enddo
                    else
                        if (show_output) then
                            write (*, 99009) ALGam / DTOr, CLGam
                            write (*, 99003) (ALQsp(k) / DTOr, k=1, NQSp)
                            99003          format (/' Current Qspec alphas  =', 20F9.3)
                        endif
                        do
                            if (show_output) write (*, 99004)
                            99004              format (' New alphas or <return>:  ', $)
                            read (*, 99012) line
                            ntmp = IPX
                            call getflt(line, W1, ntmp, error)
                            if (.not.(error)) then
                                ntmp = min(ntmp, IPX)
                                !
                                !------ if just <return> was hit, don't do anything
                                if (ntmp==0) goto 200
                                !
                                NQSp = ntmp
                                do k = 1, NQSp
                                    ALQsp(k) = W1(k) * DTOr
                                enddo
                                exit
                            endif
                        enddo
                    endif
                    !
                    IACqsp = 1
                    call qspcir
                    !
                    lcnpl = .false.
                    !
                    !--------------------------------------------------------
                elseif (comand=='CQ  ') then
                    !----- set Qspec(s) for specified CLs
                    if (ninput>=1) then
                        NQSp = min(ninput, IPX)
                        do k = 1, NQSp
                            CLQsp(k) = rinput(k)
                        enddo
                    else
                        if (show_output) then
                            write (*, 99009) ALGam / DTOr, CLGam
                            write (*, 99005) (CLQsp(k), k = 1, NQSp)
                            99005          format (/' Current Qspec CLs  =', 20F8.4)
                        endif
                        do
                            if (show_output) write (*, 99006)
                            99006              format (' New CLs or <return>:  ', $)
                            read (*, 99012) line
                            ntmp = IPX
                            call getflt(line, W1, ntmp, error)
                            if (.not.(error)) then
                                ntmp = min(ntmp, IPX)
                                !
                                !------ if just <return> was hit, don't do anything
                                if (ntmp==0) goto 200
                                !
                                NQSp = ntmp
                                do k = 1, NQSp
                                    CLQsp(k) = W1(k)
                                enddo
                                exit
                            endif
                        enddo
                    endif
                    !
                    IACqsp = 2
                    call qspcir
                    !
                    lcnpl = .false.
                    !
                    !--------------------------------------------------------
                elseif (comand=='SYMM' .or. comand=='S   ') then
                    LQSym = .not.LQSym
                    if (LQSym) then
                        if (show_output) write (*, *) 'Qspec symmetry forcing enabled.'
                        !cc       KQSP = 1
                        !cc       CALL SYMQSP(KQSP)
                        !cc       CALL CNCALC(QSPEC(1,KQSP),.FALSE.)
                        call cnsymm
                        call qspcir
                        !
                        lcnpl = .false.
                    else
                        if (show_output) write (*, *) 'Qspec symmetry forcing disabled.'
                    endif
                    !
                    !--------------------------------------------------------
                elseif (comand=='TGAP') then
                    call dztset(rinput, ninput)
                    !
                    !--------------------------------------------------------
                elseif (comand=='TANG') then
                    call agtset(rinput, ninput)
                    !
                    !--------------------------------------------------------
                elseif (comand=='READ') then
                    !----- read in Qspec
                    kqsp = 1
                    call getvov(kqsp)
                    call cncalc(QSPec(1, kqsp), .false.)
                    if (LQSym) call cnsymm
                    lcnpl = .false.
                    !
                    kqsp = 1
                    call qspint(ALQsp(kqsp), QSPec(1, kqsp), QINf, MINf, CLQsp(kqsp), CMQsp(kqsp))
                    if (show_output) then
                        write (*, 99010) ALGam / DTOr, CLGam, CMGam
                        write (*, 99011) kqsp, ALQsp(kqsp) / DTOr, CLQsp(kqsp), CMQsp(kqsp)
                    endif
                    !
                    !--------------------------------------------------------
                elseif (comand=='SMOO') then
                    !----- smooth Qspec within target segment
                    kqsp = KQTarg
                    call smooq(IQ1, IQ2, kqsp)
                    call cncalc(QSPec(1, kqsp), LQSym)
                    call qspcir
                    !
                    if (show_output) write (*, 99010) ALGam / DTOr, CLGam, CMGam
                    !
                    do kqsp = 1, NQSp
                        call qspint(ALQsp(kqsp), QSPec(1, kqsp), QINf, MINf, clq, CMQsp(kqsp))
                        !
                        !------- set new CL only if alpha is prescribed
                        if (IACqsp==1) CLQsp(kqsp) = clq
                        !
                        if (show_output) write (*, 99011) kqsp, ALQsp(kqsp) / DTOr, CLQsp(kqsp), CMQsp(kqsp)
                    enddo
                    LQSppl = .false.
                    !
                    !--------------------------------------------------------
                elseif (comand=='FILT' .or. comand=='F   ') then
                    !----- apply modified Hanning filter to Cn coefficients
                    cfilt = 0.2
                    call cnfilt(cfilt)
                    call piqsum
                    call qspcir
                    !
                    if (show_output) write (*, 99010) ALGam / DTOr, CLGam, CMGam
                    !
                    do kqsp = 1, NQSp
                        call qspint(ALQsp(kqsp), QSPec(1, kqsp), QINf, MINf, clq, CMQsp(kqsp))
                        !
                        !------- set new CL only if alpha is prescribed
                        if (IACqsp==1) CLQsp(kqsp) = clq
                        !
                        if (show_output) write (*, 99011) kqsp, ALQsp(kqsp) / DTOr, CLQsp(kqsp), CMQsp(kqsp)
                    enddo
                    LQSppl = .false.
                    !
                    !--------------------------------------------------------
                elseif (comand=='DUMP') then
                    FNAme = comarg
                    if (FNAme(1:1)==' ') call asks('Enter Cn output filename^', FNAme)
                    !
                    lu = 19
                    open (lu, file = FNAme, status = 'UNKNOWN')
                    call cndump(lu)
                    close (lu)
                    !
                    !--------------------------------------------------------
                elseif (comand=='EXEC' .or. comand=='X   ') then
                    !----- execute full-inverse calculation
                    call mapgen(FFIlt, NB, XB, YB)
                    !
                    !----- spline new buffer airfoil
                    call scalc(XB, YB, SB, NB)
                    call splind(XB, XBP, SB, NB, -999.0, -999.0)
                    call splind(YB, YBP, SB, NB, -999.0, -999.0)
                    !
                    call geopar(XB, XBP, YB, YBP, SB, NB, W1, &
                            SBLe, CHOrdb, AREab, RADble, ANGbte, EI11ba, EI22ba, APX1ba, APX2ba, EI11bt, EI22bt, &
                            & APX1bt, APX2bt, THIckb, CAMbrb)
                    !
                    !
                    LQSppl = .false.
                    LGSame = .false.
                    lcnpl = .false.
                    !
                    if (show_output) write (*, 99007)
                    99007      format (//' New buffer airfoil generated'/&
                            ' Execute PANE at Top Level to set new current airfoil'/)
                    !
                    !--------------------------------------------------------
                elseif (comand=='PERT') then
                    call pert(QSPec(1, 1))
                    !----- set Q(s) for changed Cn
                    call qspcir
                    !----- go generate perturbed geometry
                    comand = 'EXEC'
                    comarg = ' '
                    cycle
                    !
                    !--------------------------------------------------------
                else
                    if (show_output) write (*, 99008) comand
                    99008      format (' Command ', a4, ' not recognized.  Type a " ? " for list.')
                    comand = '****'
                    !
                endif
                !
                goto 200
            enddo
            exit
        300  enddo
        !
        !....................................................
        !
        99009 format (/' Current Q operating condition:', '   alpha = ', f7.3, '     CL = ', f8.4)
        99010 format (/' Current :  alpha =', f9.4, '    CL =', f11.6, '    CM =', f11.6)
        99011 format (' Qspec', i2, ' :  alpha =', f9.4, '    CL =', f11.6, '    CM =', f11.6)
        99012 format (a)
    end subroutine mdes
    !*==DZTSET.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! MDES


    subroutine dztset(Rinput, Ninput)
        use i_xfoil, only: show_output
        use m_userio, only: askr
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ninput
        real, dimension(*) :: Rinput
        intent (in) Ninput, Rinput
        !
        ! Local variables
        !
        real :: dxnew, dynew
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
        if (Ninput>=2) then
            dxnew = Rinput(1)
            dynew = Rinput(2)
        else
            if (show_output) write (*, 99001) real(DZTe), imag(DZTe)
            99001 format (/' Current TE gap  dx/c dy/c =', 2F7.4)
            call askr('Enter new TE gap dx/c^', dxnew)
            call askr('Enter new TE gap dy/c^', dynew)
        endif
        !
        DZTe = cmplx(dxnew, dynew)
    end subroutine dztset
    !*==AGTSET.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine agtset(Rinput, Ninput)
        use i_xfoil, only: show_output
        use m_userio, only: askr
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ninput
        real, dimension(*) :: Rinput
        intent (in) Ninput, Rinput
        !
        ! Local variables
        !
        real :: agted
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
        if (Ninput>=2) then
            agted = Rinput(1)
        else
            if (show_output) write (*, 99001) AGTe * 180.0
            99001 format (/' Current TE angle =', f7.3, ' deg.')
            call askr('Enter new TE angle (deg)^', agted)
        endif
        !
        AGTe = agted / 180.0
    end subroutine agtset
    !*==MAPGAM.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine mapgam(Iac, Alg, Clg, Cmg)
        use m_spline, only: seval
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Alg, Clg, Cmg
        integer :: Iac
        !
        ! Local variables
        !
        real :: chsq, chx, chy, xic, yic
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
        !--------------------------------------------
        !     Sets mapped Q for current airfoil
        !     for angle of attack or CL.
        !
        !       IAC=1: specified ALGAM
        !       IAC=2: specified CLGAM
        !--------------------------------------------
        !
        !---- calculate q(w), set number of circle points NSP
        call qccalc(Iac, Alg, Clg, Cmg, MINf, QINf, NSP, W1, W2, W5, W6)
        !
        !---- store q(w), s(w), x(w), y(w)
        chx = XTE - XLE
        chy = YTE - YLE
        chsq = chx**2 + chy**2
        do i = 1, NSP
            QGAmm(i) = W6(i)
            SSPec(i) = W5(i)
            xic = seval(S(N) * SSPec(i), X, XP, S, N)
            yic = seval(S(N) * SSPec(i), Y, YP, S, N)
            XSPoc(i) = ((xic - XLE) * chx + (yic - YLE) * chy) / chsq
            YSPoc(i) = ((yic - YLE) * chx - (xic - XLE) * chy) / chsq
        enddo
        SSPle = SLE / S(N)
        !
    end subroutine mapgam
    !*==QSPCIR.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! MAPGAM


    subroutine qspcir
        use m_xqdes, only: splqsp
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: kqsp
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
        !----------------------------------------------------
        !     Sets Qspec arrays for all design alphas or CLs
        !----------------------------------------------------
        !
        do kqsp = 1, NQSp
            call qccalc(IACqsp, ALQsp(kqsp), CLQsp(kqsp), CMQsp(kqsp), MINf, QINf, NSP, W1, W2, W5, QSPec(1, kqsp))
            call splqsp(kqsp)
        enddo
        LQSpec = .true.
        !
    end subroutine qspcir
    !*==MAPGEN.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine mapgen(Ffilt, N, X, Y)
        use i_xfoil, only: show_output
        use m_xsolve, only: cgauss
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Ffilt
        integer :: N
        real, dimension(NC) :: X, Y
        intent (out) N, X, Y
        !
        ! Local variables
        !
        complex, dimension(IMX / 4) :: dcn
        real :: dcnmax, dx, dy, qimoff
        integer :: i, itercn, l, m, ncn
        complex, dimension(IMX / 4, IMX / 4) :: qq
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
        !--------------------------------------------------------
        !     Calculates the geometry from the speed function
        !     Fourier coefficients Cn, modifying them as needed
        !     to achieve specified constraints.
        !--------------------------------------------------------
        !
        !
        !---- preset rotation offset of airfoil so that initial angle is close
        !-    to the old airfoil's angle
        dx = XCOld(2) - XCOld(1)
        dy = YCOld(2) - YCOld(1)
        QIM0 = atan2(dx, -dy) + 0.5 * PI * (1.0 + AGTe)
        qimoff = QIM0 - imag(CN(0))
        CN(0) = CN(0) + cmplx(0.0, qimoff)
        !
        !---- inverse-transform and calculate geometry ZC = z(w)
        !cc   CALL CNFILT(FFILT)
        call piqsum
        call zccalc(MCT)
        !
        !---- scale,rotate z(w) to get previous chord and orientation
        call zcnorm(MCT)
        !
        !CCC---- put back rotation offset so speed routine QCCALC gets the right alpha
        !CC      CN(0) = CN(0) - CMPLX( 0.0 , QIMOFF )
        !
        !---- enforce Lighthill's first constraint
        CN(0) = cmplx(0.0, imag(CN(0)))
        !
        !---- number of free coefficients
        ncn = 1
        !
        !---- Newton iteration loop for modified Cn's
        do itercn = 1, 10
            do m = 1, ncn
                do l = 1, ncn
                    qq(m, l) = 0.
                enddo
                dcn(m) = 0.
                qq(m, m) = 1.0
            enddo
            !
            !------ fix TE gap
            m = 1
            dcn(m) = ZC(1) - ZC(NC) - DZTe
            do l = 1, ncn
                qq(m, l) = ZC_cn(1, l) - ZC_cn(NC, l)
            enddo
            !
            call cgauss(IMX / 4, ncn, qq, dcn, 1)
            !
            dcnmax = 0.
            do m = 1, ncn
                CN(m) = CN(m) - dcn(m)
                dcnmax = max(abs(dcn(m)), dcnmax)
            enddo
            !
            !cc     CALL CNFILT(FFILT)
            call piqsum
            !
            call zccalc(MCT)
            call zcnorm(MCT)
            !
            if (show_output) write (*, *) itercn, dcnmax
            if (dcnmax<=5.0E-5) goto 100
        enddo
        if (show_output) then
            write (*, *)
            write (*, *) 'MAPGEN: Geometric constraints not fully converged'
        endif
        !
        !
        !---- return new airfoil coordinates
        100  N = NC
        do i = 1, NC
            X(i) = real(ZC(i))
            Y(i) = imag(ZC(i))
        enddo
        !
    end subroutine mapgen
    !*==SCINIT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! MAPGEN


    subroutine scinit(N, X, Xp, Y, Yp, S, Sle)
        use i_xfoil, only: show_output
        use m_spline, only: curv, seval, deval
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real :: Sle
        real, dimension(N) :: S, X, Xp, Y, Yp
        intent (in) N, S, Sle, X, Xp, Y, Yp
        !
        ! Local variables
        !
        real :: bots, chordx, chordy, cvabs, cvle, dscmax, dsdwle, dxds, dxte, dyds, dyte, dzwt, hwc, qim, sic, &
                & sinw, sinwe, tops, wwt, xle, yle
        real, save :: ceps, seps
        complex :: dcn, zle, zte
        integer :: ic, ipass, itgap
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
        !----------------------------------------------------------
        !     Calculates the circle-plane coordinate s(w) = SC
        !     at each point of the current geometry.
        !     A by-product is the complex-mapping coefficients Cn.
        !     (see CNCALC header for more info).
        !----------------------------------------------------------
        !
        !c      DATA CEPS, SEPS / 1.0E-5, 5.0E-5 /
        data ceps, seps/1.0E-7, 5.0E-7/
        !
        !---- set TE angle parameter
        AGTe = (atan2(Xp(N), -Yp(N)) - atan2(Xp(1), -Yp(1))) / PI - 1.0
        !
        !---- set surface angle at first point
        AG0 = atan2(Xp(1), -Yp(1))
        !
        !---- temporary offset Qo to make  Q(w)-Qo = 0  at  w = 0 , 2 pi
        !-     --- avoids Gibbs problems with Q(w)'s Fourier sine transform
        QIM0 = AG0 + 0.5 * PI * (1.0 + AGTe)
        !
        xle = seval(Sle, X, Xp, S, N)
        yle = seval(Sle, Y, Yp, S, N)
        !
        !---- save TE gap and airfoil chord
        dxte = X(1) - X(N)
        dyte = Y(1) - Y(N)
        DZTe = cmplx(dxte, dyte)
        !
        chordx = 0.5 * (X(1) + X(N)) - xle
        chordy = 0.5 * (Y(1) + Y(N)) - yle
        CHOrdz = cmplx(chordx, chordy)
        ZLEold = cmplx(xle, yle)
        !
        if (show_output) write (*, 99001) real(DZTe), imag(DZTe), AGTe * 180.0
        99001 format (/' Current TE gap  dx dy =', 2F7.4, '    TE angle =', f7.3, ' deg.'/)
        if (show_output) write (*, *) 'Initializing mapping coordinate ...'
        !
        !---- set approximate slope ds/dw at airfoil nose
        cvle = curv(Sle, X, Xp, Y, Yp, S, N) * S(N)
        cvabs = abs(cvle)
        dsdwle = max(1.0E-3, 0.5 / cvabs)
        !
        tops = Sle / S(N)
        bots = (S(N) - Sle) / S(N)
        !
        !---- set initial top surface s(w)
        wwt = 1.0 - 2.0 * dsdwle / tops
        do ic = 1, (NC - 1) / 2 + 1
            SC(ic) = tops * (1.0 - cos(wwt * WC(ic))) / (1.0 - cos(wwt * PI))
        enddo
        !
        !---- set initial bottom surface s(w)
        wwt = 1.0 - 2.0 * dsdwle / bots
        do ic = (NC - 1) / 2 + 2, NC
            SC(ic) = 1.0 - bots * (1.0 - cos(wwt * (WC(NC) - WC(ic)))) / (1.0 - cos(wwt * PI))
        enddo
        !
        !---- iteration loop for s(w) array
        do ipass = 1, 30
            !
            !---- calculate imaginary part of harmonic function  P(w) + iQ(w)
            do ic = 1, NC
                !
                sic = S(1) + (S(N) - S(1)) * SC(ic)
                dxds = deval(sic, X, Xp, S, N)
                dyds = deval(sic, Y, Yp, S, N)
                !
                !------ set Q(w) - Qo   (Qo defined so that Q(w)-Qo = 0  at  w = 0 , 2 pi)
                qim = atan2(dxds, -dyds) - 0.5 * (WC(ic) - PI) * (1.0 + AGTe) - QIM0
                !
                PIQ(ic) = cmplx(0.0, qim)
                !
            enddo
            !
            !---- Fourier-decompose Q(w)
            call ftp
            !
            !---- zero out average real part and add on Qo we took out above
            CN(0) = cmplx(0.0, imag(CN(0)) + QIM0)
            !
            !---- transform back to get entire  PIQ = P(w) + iQ(w)
            call piqsum
            !
            !---- save s(w) for monitoring of changes in s(w) by ZCCALC
            do ic = 1, NC
                SCOld(ic) = SC(ic)
            enddo
            !
            !---- correct n=1 complex coefficient Cn for proper TE gap
            do itgap = 1, 5
                call zccalc(1)
                !
                !------ set current LE,TE locations
                call zlefind(zle, ZC, WC, NC, PIQ, AGTe)
                zte = 0.5 * (ZC(1) + ZC(NC))
                !
                dzwt = abs(zte - zle) / abs(CHOrdz)
                dcn = -(ZC(1) - ZC(NC) - dzwt * DZTe) / (ZC_cn(1, 1) - ZC_cn(NC, 1))
                CN(1) = CN(1) + dcn
                !
                call piqsum
                if (abs(dcn)<ceps) exit
            enddo
            !
            dscmax = 0.
            do ic = 1, NC
                dscmax = max(dscmax, abs(SC(ic) - SCOld(ic)))
            enddo
            !
            if (show_output) write (*, *) ipass, '     max(dw) =', dscmax
            if (dscmax<seps) exit
            !
        enddo
        !
        !---- normalize final geometry
        call zcnorm(1)
        !
        !---- set final  s(w), x(w), y(w)  arrays for old airfoil
        do ic = 1, NC
            SCOld(ic) = SC(ic)
            XCOld(ic) = real(ZC(ic))
            YCOld(ic) = imag(ZC(ic))
        enddo
        !
        do ic = 1, NC
            sinw = 2.0 * sin(0.5 * WC(ic))
            sinwe = 0.
            if (sinw>0.0) sinwe = sinw**(1.0 - AGTe)
            !
            hwc = 0.5 * (WC(ic) - PI) * (1.0 + AGTe) - 0.5 * PI
            ZCOldw(ic) = sinwe * exp(PIQ(ic) + cmplx(0.0, hwc))
        enddo
        !
        QIMold = imag(CN(0))
        !
        !C---- print out Fourier coefficients
        !      write(*,*) ' '
        !      do 700 m=0, mc
        !        write(*,*) m, real(cn(m)), IMAG(cn(m))
        !        write(1,*) m, real(cn(m)), IMAG(cn(m))
        !cc 7000   format(1x,i3,2f10.6)
        !  700 continue
        !
    end subroutine scinit
    !*==CNCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! SCINIT



    subroutine cncalc(Qc, Lsymm)
        use m_spline, only: splind, sinvrt
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        logical :: Lsymm
        real, dimension(NC) :: Qc
        intent (in) Lsymm
        !
        ! Local variables
        !
        real :: alfcir, cnr, cosw, pfun, sinw, sinwe, wcle
        complex, dimension(0:IMX) :: cnsav
        integer :: ic, m
        real, dimension(ICX) :: qcw
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
        !
        !
        !
        !c      REAL WCJ(2)
        !
        if (NC>ICX) stop 'CNCALC: Array overflow.'
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
        call splind(Qc, qcw, WC, NC, -999.0, -999.0)
        !
        !---- get approximate w value at stagnation point
        do ic = 2, NC
            if (Qc(ic)<0.0) exit
        enddo
        wcle = WC(ic)
        !
        !---- set exact numerical w value at stagnation point from splined q(w)
        call sinvrt(wcle, 0.0, Qc, qcw, WC, NC)
        !
        !---- set corresponding circle plane alpha
        alfcir = 0.5 * (wcle - PI)
        !
        !---- calculate real part of harmonic function  P(w) + iQ(w)
        do ic = 2, NC - 1
            !
            cosw = 2.0 * cos(0.5 * WC(ic) - alfcir)
            sinw = 2.0 * sin(0.5 * WC(ic))
            sinwe = sinw**AGTe
            !
            !c        IF(WC(IC).GE.WCJ(1) .AND. WC(IC).LE.WCJ(2)) THEN
            !
            !------- set P(w) from q(w)
            if (abs(cosw)<1.0E-4) then
                !-------- use asymptotic form near stagnation point
                pfun = abs(sinwe / qcw(ic))
            else
                !-------- use actual expression
                pfun = abs(cosw * sinwe / Qc(ic))
            endif
            !
            !c        ELSE
            !cC
            !cC------- set P(w) from old geometry derivative z'(w)
            !c         PFUN = ABS( ZCOLDW(IC)*SINWE/SINW )
            !cC
            !c        ENDIF
            !
            PIQ(ic) = cmplx(log(pfun), 0.0)
            !
        enddo
        !
        !---- extrapolate P(w) to TE
        PIQ(1) = 3.0 * PIQ(2) - 3.0 * PIQ(3) + PIQ(4)
        PIQ(NC) = 3.0 * PIQ(NC - 1) - 3.0 * PIQ(NC - 2) + PIQ(NC - 3)
        !
        do m = 0, MC
            cnsav(m) = CN(m)
        enddo
        !
        !---- Fourier-transform P(w) to get new Cn coefficients
        call ftp
        CN(0) = cmplx(0.0, QIMold)
        !
        if (Lsymm) then
            do m = 1, MC
                cnr = 2.0 * real(CN(m)) - real(cnsav(m))
                CN(m) = cmplx(cnr, 0.0)
            enddo
        endif
        !
        call piqsum
        !
    end subroutine cncalc
    !*==CNSYMM.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! CNCALC


    subroutine cnsymm
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: m
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
        !---- eliminate imaginary (camber) parts of mapping coefficients
        do m = 1, MC
            CN(m) = cmplx(real(CN(m)), 0.0)
        enddo
        !
        call piqsum
    end subroutine cnsymm
    !*==PIQSUM.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! CNSYMM


    subroutine piqsum
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: ic, m
        complex :: zsum
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
        !---------------------------------------------
        !     Inverse-transform to get back modified
        !     speed function and its conjugate.
        !---------------------------------------------
        !
        do ic = 1, NC
            zsum = (0.0, 0.0)
            do m = 0, MC
                zsum = zsum + CN(m) * conjg(EIW(ic, m))
            enddo
            PIQ(ic) = zsum
        enddo
        !
    end subroutine piqsum
    !*==CNFILT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PIQSUM


    subroutine cnfilt(Ffilt)
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Ffilt
        intent (in) Ffilt
        !
        ! Local variables
        !
        real :: cwt, cwtx, freq
        integer :: m
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
        !-------------------------------------
        !     Filters out upper harmonics
        !     with modified Hanning filter.
        !-------------------------------------
        !
        if (Ffilt==0.0) return
        !
        do m = 0, MC
            freq = float(m) / float(MC)
            cwt = 0.5 * (1.0 + cos(PI * freq))
            cwtx = cwt
            if (Ffilt>0.0) cwtx = cwt**Ffilt
            CN(m) = CN(m) * cwtx
        enddo
        !
    end subroutine cnfilt
    !*==ZCCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! CNFILT


    subroutine zccalc(Mtest)
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Mtest
        intent (in) Mtest
        !
        ! Local variables
        !
        complex :: dzdw1, dzdw2, dz_piq1, dz_piq2
        real :: hwc, sinw, sinwe
        integer :: ic, m
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
        !--------------------------------------------------------
        !     Calculates the airfoil geometry z(w) from the
        !     harmonic function P(w) + iQ(w).  Also normalizes
        !     the coordinates to the old chord and calculates
        !     the geometry sensitivities dz/dCn  (1 < n < MTEST)
        !     for each point.
        !--------------------------------------------------------
        !
        !---- integrate upper airfoil surface coordinates from x,y = 4,0
        ic = 1
        ZC(ic) = (4.0, 0.0)
        do m = 1, Mtest
            ZC_cn(ic, m) = (0.0, 0.0)
        enddo
        !
        sinw = 2.0 * sin(0.5 * WC(ic))
        sinwe = 0.
        if (sinw>0.0) sinwe = sinw**(1.0 - AGTe)
        !
        hwc = 0.5 * (WC(ic) - PI) * (1.0 + AGTe) - 0.5 * PI
        dzdw1 = sinwe * exp(PIQ(ic) + cmplx(0.0, hwc))
        do ic = 2, NC
            !
            sinw = 2.0 * sin(0.5 * WC(ic))
            sinwe = 0.
            if (sinw>0.0) sinwe = sinw**(1.0 - AGTe)
            !
            hwc = 0.5 * (WC(ic) - PI) * (1.0 + AGTe) - 0.5 * PI
            dzdw2 = sinwe * exp(PIQ(ic) + cmplx(0.0, hwc))
            !
            ZC(ic) = 0.5 * (dzdw1 + dzdw2) * DWC + ZC(ic - 1)
            dz_piq1 = 0.5 * (dzdw1) * DWC
            dz_piq2 = 0.5 * (dzdw2) * DWC
            !
            do m = 1, Mtest
                ZC_cn(ic, m) = dz_piq1 * conjg(EIW(ic - 1, m)) + dz_piq2 * conjg(EIW(ic, m)) + ZC_cn(ic - 1, m)
            enddo
            !
            dzdw1 = dzdw2
        enddo
        !
        !---- set arc length array s(w)
        SC(1) = 0.
        do ic = 2, NC
            SC(ic) = SC(ic - 1) + abs(ZC(ic) - ZC(ic - 1))
        enddo
        !
        !---- normalize arc length
        do ic = 1, NC
            SC(ic) = SC(ic) / SC(NC)
        enddo
        !
    end subroutine zccalc
    !*==ZCNORM.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! ZCCALC


    subroutine zcnorm(Mtest)
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Mtest
        intent (in) Mtest
        !
        ! Local variables
        !
        integer :: ic, m
        real :: qimoff
        complex :: zcnew, zc_zte, zle, zte
        complex, dimension(IMX / 4) :: zte_cn
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
        !     Normalizes the complex airfoil z(w) to
        !     the old chord and angle, and resets the
        !     influence coefficients  dz/dCn .
        !-----------------------------------------------
        !
        !---- find current LE location
        call zlefind(zle, ZC, WC, NC, PIQ, AGTe)
        !
        !---- place leading edge at origin
        do ic = 1, NC
            ZC(ic) = ZC(ic) - zle
        enddo
        !
        !---- set normalizing quantities and sensitivities
        zte = 0.5 * (ZC(1) + ZC(NC))
        do m = 1, Mtest
            zte_cn(m) = 0.5 * (ZC_cn(1, m) + ZC_cn(NC, m))
        enddo
        !
        !---- normalize airfoil to proper chord, put LE at old position,
        !-    and set sensitivities dz/dCn for the rescaled coordinates
        do ic = 1, NC
            zcnew = CHOrdz * ZC(ic) / zte
            zc_zte = -zcnew / zte
            ZC(ic) = zcnew
            do m = 1, Mtest
                ZC_cn(ic, m) = CHOrdz * ZC_cn(ic, m) / zte + zc_zte * zte_cn(m)
            enddo
        enddo
        !
        !---- add on rotation to mapping coefficient so QCCALC gets the right alpha
        qimoff = -imag(log(CHOrdz / zte))
        CN(0) = CN(0) - cmplx(0.0, qimoff)
        !
        !---- shift airfoil to put LE at old location
        do ic = 1, NC
            ZC(ic) = ZC(ic) + ZLEold
        enddo
        !
    end subroutine zcnorm
    !*==QCCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! ZCNORM


    subroutine qccalc(Ispec, Alfa, Cl, Cm, Minf, Qinf, Ncir, Xcir, Ycir, Scir, Qcir)
        use i_xfoil, only: show_output
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Alfa, Cl, Cm, Minf, Qinf
        integer :: Ispec, Ncir
        real, dimension(NC) :: Qcir, Scir, Xcir, Ycir
        intent (in) Ispec, Minf, Qinf
        intent (out) Cm, Ncir, Scir, Xcir, Ycir
        intent (inout) Alfa, Cl, Qcir
        !
        ! Local variables
        !
        real, save :: aeps
        real :: alfcir, beta, bfac, clt, clt_a, cpcom1, cpcom2, cpc_a1, cpc_a2, cpc_q1, cpc_q2, cpinc1, cpinc2, &
                & cpi_q1, cpi_q2, dalfa, eppp, ppp, sinw, sinwe
        complex :: cft, cft_a, cmt, dz, eia, za
        integer :: ic, icp, ipass
        real, dimension(ICX) :: qc_a
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
        !---------------------------------------------------
        !     Calculates the surface speed from the complex
        !     speed function so that either a prescribed
        !     ALFA or CL is achieved, depending on whether
        !     ISPEC=1 or 2.  The CL calculation uses the
        !     transformed Karman-Tsien Cp.
        !---------------------------------------------------
        data aeps/5.0E-7/
        !
        !---- Karman-Tsien quantities
        beta = sqrt(1.0 - Minf**2)
        bfac = 0.5 * Minf**2 / (1.0 + beta)
        !
        Ncir = NC
        !
        !---- Newton iteration loop (executed only once if alpha specified)
        do ipass = 1, 10
            !
            !------ set alpha in the circle plane
            alfcir = Alfa - imag(CN(0))
            !
            cmt = (0.0, 0.0)
            cft = (0.0, 0.0)
            cft_a = (0.0, 0.0)
            !
            !------ set surface speed for current circle plane alpha
            do ic = 1, NC
                ppp = real(PIQ(ic))
                eppp = exp(-ppp)
                sinw = 2.0 * sin(0.5 * WC(ic))
                !
                if (AGTe==0.0) then
                    sinwe = 1.0
                elseif (sinw>0.0) then
                    sinwe = sinw**AGTe
                else
                    sinwe = 0.0
                endif
                !
                Qcir(ic) = 2.0 * cos(0.5 * WC(ic) - alfcir) * sinwe * eppp
                qc_a(ic) = 2.0 * sin(0.5 * WC(ic) - alfcir) * sinwe * eppp
                !
                Xcir(ic) = real(ZC(ic))
                Ycir(ic) = imag(ZC(ic))
                Scir(ic) = SC(ic)
            enddo
            !
            !------ integrate compressible  Cp dz  to get complex force  CL + iCD
            ic = 1
            cpinc1 = 1.0 - (Qcir(ic) / Qinf)**2
            cpi_q1 = -2.0 * Qcir(ic) / Qinf**2
            cpcom1 = cpinc1 / (beta + bfac * cpinc1)
            cpc_q1 = (1.0 - bfac * cpcom1) / (beta + bfac * cpinc1) * cpi_q1
            cpc_a1 = cpc_q1 * qc_a(ic)
            do ic = 1, NC
                icp = ic + 1
                if (ic==NC) icp = 1
                !
                cpinc2 = 1.0 - (Qcir(icp) / Qinf)**2
                cpi_q2 = -2.0 * Qcir(icp) / Qinf**2
                cpcom2 = cpinc2 / (beta + bfac * cpinc2)
                cpc_q2 = (1.0 - bfac * cpcom2) / (beta + bfac * cpinc2) * cpi_q2
                cpc_a2 = cpc_q2 * qc_a(icp)
                !
                za = (ZC(icp) + ZC(ic)) * 0.5 - (0.25, 0.0)
                dz = ZC(icp) - ZC(ic)
                !
                cmt = cmt - 0.5 * (cpcom1 + cpcom2) * dz * conjg(za) + (cpcom1 - cpcom2) * dz * conjg(dz) / 12.0
                cft = cft + 0.5 * (cpcom1 + cpcom2) * dz
                cft_a = cft_a + 0.5 * (cpc_a1 + cpc_a2) * dz
                !
                cpcom1 = cpcom2
                cpc_a1 = cpc_a2
            enddo
            !
            !------ rotate force vector into freestream coordinates
            eia = exp(cmplx(0.0, -Alfa))
            cft = cft * eia
            cft_a = cft_a * eia + cft * (0.0, -1.0)
            !
            !------ lift is real part of complex force vector
            clt = real(cft)
            clt_a = real(cft_a)
            !
            !------ moment is real part of complex moment
            Cm = real(cmt)
            !
            if (Ispec==1) then
                !------- if alpha is prescribed, we're done
                Cl = clt
                return
            else
                !------- adjust alpha with Newton-Raphson to get specified CL
                dalfa = (Cl - clt) / clt_a
                Alfa = Alfa + dalfa
                if (abs(dalfa)<aeps) return
            endif
            !
        enddo
        if (show_output) write (*, *) 'QCCALC: CL convergence failed.  dAlpha =', dalfa
        !
    end subroutine qccalc
    !*==QSPINT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! QCCALC



    subroutine qspint(Alqsp, Qspec, Qinf, Minf, Clqsp, Cmqsp)
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Alqsp, Clqsp, Cmqsp, Minf, Qinf
        real, dimension(NC) :: Qspec
        intent (in) Alqsp, Minf, Qinf, Qspec
        intent (inout) Clqsp, Cmqsp
        !
        ! Local variables
        !
        real :: aq, ax, ay, beta, bfac, ca, cpq1, cpq2, cqinc, du, dx, dy, sa
        integer :: i, ip
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
        !--------------------------------------------
        !     Integrates circle-plane array surface
        !     pressures to get CL and CM
        !--------------------------------------------
        !
        sa = sin(Alqsp)
        ca = cos(Alqsp)
        !
        beta = sqrt(1.0 - Minf**2)
        bfac = 0.5 * Minf**2 / (1.0 + beta)
        !
        Clqsp = 0.0
        Cmqsp = 0.0
        !
        i = 1
        cqinc = 1.0 - (Qspec(i) / Qinf)**2
        cpq1 = cqinc / (beta + bfac * cqinc)
        !
        do i = 1, NC
            ip = i + 1
            if (i==NC) ip = 1
            !
            cqinc = 1.0 - (Qspec(ip) / Qinf)**2
            cpq2 = cqinc / (beta + bfac * cqinc)
            !
            dx = (XCOld(ip) - XCOld(i)) * ca + (YCOld(ip) - YCOld(i)) * sa
            dy = (YCOld(ip) - YCOld(i)) * ca - (XCOld(ip) - XCOld(i)) * sa
            du = cpq2 - cpq1
            !
            ax = 0.5 * (XCOld(ip) + XCOld(i)) * ca + 0.5 * (YCOld(ip) + YCOld(i)) * sa
            ay = 0.5 * (YCOld(ip) + YCOld(i)) * ca - 0.5 * (XCOld(ip) + XCOld(i)) * sa
            aq = 0.5 * (cpq2 + cpq1)
            !
            Clqsp = Clqsp + dx * aq
            Cmqsp = Cmqsp - dx * (aq * (ax - 0.25) + du * dx / 12.0) - dy * (aq * ay + du * dy / 12.0)
            !
            cpq1 = cpq2
        enddo
        !
    end subroutine qspint
    !*==FTP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! QSPINT


    subroutine ftp
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: ic, m
        complex :: zsum
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
        !----------------------------------------------------------------
        !     Slow-Fourier-Transform P(w) using Trapezoidal integration.
        !----------------------------------------------------------------
        !
        do m = 0, MC
            zsum = (0.0, 0.0)
            do ic = 2, NC - 1
                zsum = zsum + PIQ(ic) * EIW(ic, m)
            enddo
            CN(m) = (0.5 * (PIQ(1) * EIW(1, m) + PIQ(NC) * EIW(NC, m)) + zsum) * DWC / PI
        enddo
        CN(0) = 0.5 * CN(0)
        !
    end subroutine ftp
    !*==EIWSET.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! FTP


    subroutine eiwset(Nc1)
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Nc1
        intent (in) Nc1
        !
        ! Local variables
        !
        integer :: ic, ic1, m
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
        !----------------------------------------------------
        !     Calculates the uniformly-spaced circle-plane
        !     coordinate array WC (omega), and the
        !     corresponding complex unit numbers exp(inw)
        !     for Slow Fourier Transform operations.
        !----------------------------------------------------
        !
        PI = 4.0 * atan(1.0)
        !
        !---- set requested number of points in circle plane
        NC = Nc1
        MC = Nc1 / 4
        MCT = Nc1 / 16
        !
        if (NC>ICX) stop 'EIWSET: Array overflow. Increase ICX.'
        !
        DWC = 2.0 * PI / float(NC - 1)
        !
        do ic = 1, NC
            WC(ic) = DWC * float(ic - 1)
        enddo
        !
        !---- set  m = 0  numbers
        do ic = 1, NC
            EIW(ic, 0) = (1.0, 0.0)
        enddo
        !
        !---- set  m = 1  numbers
        do ic = 1, NC
            EIW(ic, 1) = exp(cmplx(0.0, WC(ic)))
        enddo
        !
        !---- set  m > 1  numbers by indexing appropriately from  m = 1  numbers
        do m = 2, MC
            do ic = 1, NC
                ic1 = m * (ic - 1)
                ic1 = mod(ic1, (NC - 1)) + 1
                EIW(ic, m) = EIW(ic1, 1)
            enddo
        enddo
        !
    end subroutine eiwset
    !*==PERT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! EIWSET



    subroutine pert(Qspec)
        use i_xfoil, only: show_output
        use m_xsolve, only: cgauss
        use i_circle
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real, dimension(ICX) :: Qspec
        !
        ! Local variables
        !
        complex, dimension(IMX / 4) :: dcn
        real :: dcni, dcnmax, dcnr, dx, dy, qimoff
        integer :: itercn, l, m, ncn
        complex, dimension(IMX / 4, IMX / 4) :: qq
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
        !--------------------------------------------------------
        !     Calculates the perturbed geometry resulting from
        !     one Cn mapping coefficient being perturbed by user.
        !--------------------------------------------------------
        !
        !
        !---- calculate mapping coefficients for initial airfoil shape
        call cncalc(Qspec, .false.)
        !
        !---- preset rotation offset of airfoil so that initial angle is close
        !-    to the old airfoil's angle
        dx = XCOld(2) - XCOld(1)
        dy = YCOld(2) - YCOld(1)
        QIM0 = atan2(dx, -dy) + 0.5 * PI * (1.0 + AGTe)
        qimoff = QIM0 - imag(CN(0))
        CN(0) = CN(0) + cmplx(0.0, qimoff)
        !
        if (show_output) then
            write (*, *)
            write (*, *) 'Current mapping coefficients...'
            write (*, *) '      n    Re(Cn)      Im(Cn)'
            !cc   DO M = 1, NC
            do m = 1, min(NC, 32)
                write (*, 99001) m, real(CN(m)), imag(CN(m))
                99001 format (4x, i4, 2F12.6)
            enddo
        endif
        100  do
            !
            if (show_output) write (*, 99002)
            99002 format (/4x, 'Enter  n, delta(Cnr), delta(Cni):  ', $)
            read (*, *, err = 100) m, dcnr, dcni
            if (m<=0) cycle
            if (m>NC) then
                if (show_output) write (*, *) 'Max number of modes is', NC
                cycle
            endif
            CN(m) = CN(m) + cmplx(dcnr, dcni)
            !
            !---- inverse-transform and calculate geometry
            !cc   CALL CNFILT(FFILT)
            call piqsum
            call zccalc(MCT)
            !
            !---- normalize chord and set exact previous alpha
            call zcnorm(MCT)
            !
            !CC---- put back rotation offset so speed routine QCCALC gets the right alpha
            !CC      CN(0) = CN(0) - CMPLX( 0.0 , QIMOFF )

            !---- enforce Lighthill's first constraint
            CN(0) = cmplx(0.0, imag(CN(0)))

            !---- number of free coefficients
            ncn = 1

            !---- Newton iteration loop for modified Cn's
            do itercn = 1, 10

                !------ fix TE gap
                m = 1
                dcn(m) = ZC(1) - ZC(NC) - DZTe
                do l = 1, ncn
                    qq(m, l) = ZC_cn(1, l) - ZC_cn(NC, l)
                enddo
                !
                call cgauss(IMX / 4, ncn, qq, dcn, 1)
                !
                dcnmax = 0.
                do m = 1, ncn
                    CN(m) = CN(m) - dcn(m)
                    dcnmax = max(abs(dcn(m)), dcnmax)
                enddo
                !
                !cc     CALL CNFILT(FFILT)
                call piqsum
                !
                call zccalc(MCT)
                call zcnorm(MCT)
                !
                if (show_output) write (*, *) itercn, dcnmax
                if (dcnmax<=5.0E-5) goto 99999
            enddo
            if (show_output) write (*, *) 'TE gap,chord did not converge'
            exit
        enddo
    99999 end subroutine pert
    !*==CNDUMP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! PERT



    subroutine cndump(Lu)
        use i_circle
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
        integer :: m
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
        !--------------------------------------------------------
        !     Writes out the Fourier coefficients Cn
        !--------------------------------------------------------
        !
        do m = 0, MC
            write (Lu, 99001) m, real(CN(m)), imag(CN(m)), real(PIQ(m + 1)), imag(PIQ(m + 1))
        enddo
        !
        do m = MC + 1, NC - 1
            write (Lu, 99001) m, 0.0, 0.0, real(PIQ(m + 1)), imag(PIQ(m + 1))
        enddo
        !
        99001 format (1x, i3, 4F11.6)
        !
    end subroutine cndump
    !*==GETVOV.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine getvov(Kqsp)
        use m_userio, only: asks
        use m_sort, only: sort
        use m_xqdes, only: splqsp, qincom
        use m_spline, only: seval, splind
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Kqsp
        !
        ! Local variables
        !
        integer :: i, k, kk, lu
        real :: qsnew, ss, sspan, sstart
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
        !LED ENTIRE ROUTINE
        !
        kk = 0
        do i = 1, IQX
            W1(i) = 0.
            W2(i) = 0.
            W3(i) = 0.
        enddo
        !
        lu = 2
        !
        call asks('Enter V/Vinf vs s data filename^', FNAme)
        open (lu, file = FNAme, status = 'OLD', err = 200)
        !
        !---- read the Qspec file
        do k = 1, IQX
            read (lu, *, end = 100, err = 300) W1(k), W2(k)
        enddo
        100  kk = k - 1
        close (lu)
        !
        !---- nondimensionalize S distances
        sspan = W1(kk) - W1(1)
        sstart = W1(1)
        do k = 1, kk
            W1(k) = 1. - (W1(k) - sstart) / sspan
        enddo
        !
        !---- sort input points then, removing identical pairs
        call sort(kk, W1, W2)
        !
        !---- spline input points
        call splind(W2, W3, W1, kk, -999.0, -999.0)
        !
        !---- set Qspec array
        do i = 1, NSP
            ss = SSPec(i)
            !
            !------ evaluate spline at SSPEC positions
            qsnew = seval(ss, W2, W3, W1, kk)
            !
            !------ set incompressible speed from new compressible speed
            QSPec(i, Kqsp) = qincom(qsnew, QINf, TKLam)
            !
        enddo
        !
        !---- spline new Qspec array
        call splqsp(Kqsp)
        !
        return
        !
        200  if (show_output) write (*, *) 'GETVOV: File OPEN error.'
        return
        !
        300  if (show_output) write (*, *) 'GETVOV: File READ error.'
        close (lu)
        !
    end subroutine getvov
    !*==ZLEFIND.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! GETVOV


    subroutine zlefind(Zle, Zc, Wc, Nc, Piq, Agte)
        use i_xfoil, only: show_output
        use m_spline, only: d2val, seval, splind, deval
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        integer, parameter :: NTX = 33
        !
        ! Dummy arguments
        !
        real :: Agte
        integer :: Nc
        complex :: Zle
        complex, dimension(*) :: Piq, Zc
        real, dimension(*) :: Wc
        intent (in) Agte, Nc, Piq, Zc
        intent (out) Zle
        !
        ! Local variables
        !
        real :: dist, dmax, dwcle, dxdd, dxdw, dydd, dydw, hwc, res, resw, sinw, sinwe, wcle, xchord, xcle, &
                & xcte, ychord, ycle, ycte
        complex :: dzdw1, dzdw2, zte
        integer :: i, ic, ic1, ic2, icle, itcle, nic
        real, save :: pi
        real, dimension(NTX) :: xc, xcw, yc, ycw
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
        !
        !---- temporary work arrays for splining near leading edge
        !
        data pi/3.1415926535897932384/
        !
        zte = 0.5 * (Zc(1) + Zc(Nc))
        !
        !---- find point farthest from TE
        dmax = 0.0
        do ic = 1, Nc
            dist = abs(Zc(ic) - zte)
            !
            if (dist>dmax) then
                dmax = dist
                icle = ic
            endif
        enddo
        !
        !---- set restricted spline limits around leading edge
        ic1 = max(icle - (NTX - 1) / 2, 1)
        ic2 = min(icle + (NTX - 1) / 2, Nc)
        !
        !---- set up derivatives at spline endpoints
        sinw = 2.0 * sin(0.5 * Wc(ic1))
        sinwe = sinw**(1.0 - Agte)
        hwc = 0.5 * (Wc(ic1) - pi) * (1.0 + Agte) - 0.5 * pi
        dzdw1 = sinwe * exp(Piq(ic1) + cmplx(0.0, hwc))
        !
        sinw = 2.0 * sin(0.5 * Wc(ic2))
        sinwe = sinw**(1.0 - Agte)
        hwc = 0.5 * (Wc(ic2) - pi) * (1.0 + Agte) - 0.5 * pi
        dzdw2 = sinwe * exp(Piq(ic2) + cmplx(0.0, hwc))
        !
        !---- fill temporary x,y coordinate arrays
        do ic = ic1, ic2
            i = ic - ic1 + 1
            xc(i) = real(Zc(ic))
            yc(i) = imag(Zc(ic))
        enddo
        !
        !---- calculate spline near leading edge with derivative end conditions
        nic = ic2 - ic1 + 1
        call splind(xc, xcw, Wc(ic1), nic, real(dzdw1), real(dzdw2))
        call splind(yc, ycw, Wc(ic1), nic, imag(dzdw1), imag(dzdw2))
        !
        xcte = 0.5 * real(Zc(1) + Zc(Nc))
        ycte = 0.5 * imag(Zc(1) + Zc(Nc))
        !
        !---- initial guess for leading edge coordinate
        wcle = Wc(icle)
        !
        !---- Newton loop for improved leading edge coordinate
        do itcle = 1, 10
            xcle = seval(wcle, xc, xcw, Wc(ic1), nic)
            ycle = seval(wcle, yc, ycw, Wc(ic1), nic)
            dxdw = deval(wcle, xc, xcw, Wc(ic1), nic)
            dydw = deval(wcle, yc, ycw, Wc(ic1), nic)
            dxdd = d2val(wcle, xc, xcw, Wc(ic1), nic)
            dydd = d2val(wcle, yc, ycw, Wc(ic1), nic)
            !
            xchord = xcle - xcte
            ychord = ycle - ycte
            !
            !------ drive dot product between chord line and LE tangent to zero
            res = xchord * dxdw + ychord * dydw
            resw = dxdw * dxdw + dydw * dydw + xchord * dxdd + ychord * dydd
            !
            dwcle = -res / resw
            wcle = wcle + dwcle
            !
            if (abs(dwcle)<1.0E-5) goto 100
        enddo
        if (show_output) write (*, *) 'ZLEFIND: LE location failed.'
        wcle = Wc(icle)
        !
        !---- set final leading edge point complex coordinate
        100  xcle = seval(wcle, xc, xcw, Wc(ic1), nic)
        ycle = seval(wcle, yc, ycw, Wc(ic1), nic)
        Zle = cmplx(xcle, ycle)
        !
    end subroutine zlefind

end module m_xmdes