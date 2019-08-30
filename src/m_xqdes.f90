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

!*==M_XQDES.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
!
module m_xqdes
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    !*** End of declarations rewritten by SPAG
    !
contains
    subroutine qdes
        use s_xfoil, only: clcalc, tecalc
        use m_xpanel, only: apcalc, ncalc
        use m_xoper, only: specal
        use m_xgeom, only: lefind
        use m_userio, only: getflt, askc, getint
        use m_spline, only: seval, splind, scalc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        character(128), save :: argold
        real :: cdpq, chsq, chx, chy, clq_alf, clq_msq, g
        character(4) :: comand
        character(128) :: comarg
        character(4), save :: comold
        logical :: error, lrecalc
        integer :: i, kqsp, ninput, ntqspl
        integer, dimension(20) :: iinput
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
        !------------------------------------------------------
        !     Mixed-Inverse design routine. Based on the
        !     same panel formulation as basic analysis method.
        !------------------------------------------------------
        !
        !
        !
        !
        !
        !---- statement function for compressible Karman-Tsien velocity
        qcomp(g) = g * (1.0 - TKLam) / (1.0 - TKLam * (g / QINf)**2)
        !
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
        LSYm = .true.
        !
        !---- number of sub-intervals for Qspec(s) plotting
        ntqspl = 1
        if (LQSlop) ntqspl = 8
        !
        !---- make sure a current solution exists
        call specal
        !
        !---- see if current Qspec, if any, didn't come from Full-Inverse
        if (NSP/=N) then
            LQSpec = .false.
            LIQset = .false.
        endif
        !
        !---- set alpha, etc corresponding to Q
        ALGam = ALFa
        CLGam = CL
        CMGam = CM
        !
        !---- set "old" speed distribution Q, arc length, and x/c,y/c arrays
        chx = XTE - XLE
        chy = YTE - YLE
        chsq = chx**2 + chy**2
        NSP = N
        do i = 1, NSP
            QGAmm(i) = GAM(i)
            SSPec(i) = S(i) / S(N)
            XSPoc(i) = ((X(i) - XLE) * chx + (Y(i) - YLE) * chy) / chsq
            YSPoc(i) = ((Y(i) - YLE) * chx - (X(i) - XLE) * chy) / chsq
        enddo
        SSPle = SLE / S(N)
        !
        if (show_output) write (*, 99001) ALGam / DTOr, CLGam
        99001 format (/' Current Q operating condition:'/' alpha = ', f8.3, ' deg.      CL = ', f8.4/)
        !
        if (.not.LQSpec) then
            !----- initialize Qspec to "old" solution and notify user
            NQSp = 1
            KQTarg = 1
            call gamqsp(1)
            if (show_output) write (*, 99002)
            !
            !....................................................
            !
            99002 format (/' Qspec initialized to current Q.'/)
            LQSpec = .true.
        endif
        do
            !
            !
            !====================================================
            !---- start of menu loop
            comold = comand
            argold = comarg
            do
                !
                call askc('.QDES^', comand, comarg)
                !
                !--------------------------------------------------------
                !---- process previous command ?
                if (comand(1:1)/='!') then
                    lrecalc = .false.
                elseif (comold=='****') then
                    if (show_output) write (*, *) 'Previous .QDES command not valid'
                    cycle
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
                    if (show_output) write (*, 99003)
                    99003      format (&
                            /'   <cr>   Return to Top Level'&
                            //'   QSET   Reset Qspec <== Q'&
                            /'   SMOO   Smooth Qspec inside target segment'&
                            /'   SLOP   Toggle modified-Qspec slope matching flag'&
                            /'   REST   Restore geometry from buffer airfoil'&
                            /'   CPXX   CPxx endpoint constraint toggle')
                    !
                    !--------------------------------------------------------
                    !---- re-initialize Qspec to Q
                elseif (comand=='QSET') then
                    call gamqsp(1)
                    !
                    !--------------------------------------------------------
                    !---- smooth Qspec within target segment, or entire Qspec if not marked off
                elseif (comand=='SMOO') then
                    !
                    kqsp = 1
                    call smooq(IQ1, IQ2, kqsp)
                    call splqsp(kqsp)
                    !
                    call clcalc(N, X, Y, QSPec(1, kqsp), W1, ALFa, MINf, QINf, &
                            XCMref, YCMref, CLQsp(kqsp), CMQsp(kqsp), cdpq, clq_alf, clq_msq)
                    if (show_output) write (*, 99004) CL, CM, CLQsp(kqsp), CMQsp(kqsp)
                    99004      format (/' Q    :   CL =', f11.6, '    CM =', f11.6/&
                            ' Qspec:   CL =', f11.6, '    CM =', f11.6)
                    !
                    !--------------------------------------------------------
                    !---- toggle Qspec endpoint slope matching
                elseif (comand=='SLOP') then
                    LQSlop = .not.LQSlop
                    if (LQSlop) then
                        if (show_output) write (*, *) 'Modified Qspec piece will be made tangent at endpoints'
                    else
                        if (show_output) write (*, *) 'Modified Qspec piece will not be made tangent at endpoints'
                    endif
                    !
                    !--------------------------------------------------------
                    !---- toggle CPxx preservation constraints
                elseif (comand=='CPXX') then
                    LCPxx = .not.LCPxx
                    if (LCPxx) then
                        if (show_output) write (*, *) 'CPxx will be constrained'
                    else
                        if (show_output) write (*, *) 'CPxx will not be constrained'
                    endif
                    !
                    !--------------------------------------------------------
                    !---- restore and spline old airfoil
                elseif (comand=='REST') then
                    do i = 1, N
                        X(i) = XB(i)
                        Y(i) = YB(i)
                    enddo
                    call scalc(X, Y, S, N)
                    call splind(X, XP, S, N, -999.0, -999.0)
                    call splind(Y, YP, S, N, -999.0, -999.0)
                    call ncalc(X, Y, S, N, NX, NY)
                    call lefind(SLE, X, XP, Y, YP, S, N)
                    XLE = seval(SLE, X, XP, S, N)
                    YLE = seval(SLE, Y, YP, S, N)
                    CHOrd = sqrt((0.5 * (X(1) + X(N)) - XLE)**2 + (0.5 * (Y(1) + Y(N)) - YLE)**2)
                    call tecalc
                    call apcalc
                    LGAmu = .false.
                    LQInu = .false.
                    LGSame = .true.
                    !
                    !c       CALL NAMMOD(NAME,-1,1)
                    !c       CALL STRIP(NAME,NNAME)
                    !
                    !--------------------------------------------------------
                else
                    if (show_output) write (*, 99005) comand
                    99005      format (' Command ', a4, ' not recognized.  Type a " ? " for list.')
                    !
                    comand = '****'
                endif
                !
                goto 100
            enddo
            exit
        100  enddo
    end subroutine qdes


    subroutine splqsp(Kqsp)
        use m_spline, only: splind
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Kqsp
        intent (in) Kqsp
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
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !------------------------------------------------------
        !     Splines Qspec(s).  The end intervals are treated
        !     specially to avoid Gibbs-type problems from
        !     blindly splining to the stagnation point.
        !------------------------------------------------------
        !
        !---- usual spline with natural end BCs
        call splind(QSPec(2, Kqsp), QSPecp(2, Kqsp), SSPec(2), NSP - 2, -999.0, -999.0)
        !
        !cC---- pseudo-monotonic spline with simple secant slope calculation
        !c      CALL SPLINA(QSPEC(2,KQSP),QSPECP(2,KQSP),SSPEC(2),NSP-2)
        !
        !---- end intervals are splined separately with natural BCs at
        !     the trailing edge and matching slopes at the interior points
        !
        i = 1
        call splind(QSPec(i, Kqsp), QSPecp(i, Kqsp), SSPec(i), 2, -999.0, QSPecp(i + 1, Kqsp))
        !
        i = NSP - 1
        call splind(QSPec(i, Kqsp), QSPecp(i, Kqsp), SSPec(i), 2, QSPecp(i, Kqsp), -999.0)
        !
    end subroutine splqsp


    subroutine smooq(Kq1, Kq2, Kqsp)
        use m_spline, only: trisol
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Kq1, Kq2, Kqsp
        intent (in) Kq1, Kq2, Kqsp
        !
        ! Local variables
        !
        real :: ds, dsm, dso, dsp, qspp1, qspp2, smool, smoosq
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
        !     Smooths Qspec(s) inside target segment
        !--------------------------------------------
        !
        !C---- calculate smoothing coordinate
        !cc      IF(NSP.EQ.NC1) THEN
        !C
        !C------ mapping inverse: use circle plane coordinate
        !        I = 1
        !        W8(I) = 0.0
        !        DO 10 I=2, NSP
        !          SINW = 2.0*SIN( 0.25*(WC(I)+WC(I-1)) )
        !          SINWE = SINW**(1.0-AGTE)
        !C
        !          DSDW = SINWE * EXP( REAL(0.5*(PIQ(I)+PIQ(I-1)) ))
        !          W8(I) = W8(I-1) + (WC(I)-WC(I-1))/DSDW
        !   10   CONTINUE
        !        DO 11 I=1, NSP
        !          W8(I) = W8(I)/W8(NSP)
        ! 11     CONTINUE
        !C
        !C------ do not smooth first and last intervals in circle plane
        !        KQ1 = MAX(IQ1,2)
        !        KQ2 = MIN(IQ2,NSP-1)
        !C
        !cc      ELSE
        !
        !------ mixed inverse: use arc length coordinate
        do i = 1, NSP
            W8(i) = SSPec(i)
        enddo
        !
        !cc      ENDIF
        !
        !
        if (Kq2 - Kq1<2) then
            if (show_output) write (*, *) 'Segment is too short.  No smoothing possible.'
            return
        endif
        !
        !---- set smoothing length ( ~ distance over which data is smeared )
        smool = 0.002 * (W8(NSP) - W8(1))
        !CC   CALL ASKR('Enter Qspec smoothing length^',SMOOL)
        !
        !---- set up tri-diagonal system for smoothed Qspec
        smoosq = smool**2
        do i = Kq1 + 1, Kq2 - 1
            dsm = W8(i) - W8(i - 1)
            dsp = W8(i + 1) - W8(i)
            dso = 0.5 * (W8(i + 1) - W8(i - 1))
            !
            W1(i) = smoosq * (-1.0 / dsm) / dso
            W2(i) = smoosq * (1.0 / dsp + 1.0 / dsm) / dso + 1.0
            W3(i) = smoosq * (-1.0 / dsp) / dso
        enddo
        !
        !---- set fixed-Qspec end conditions
        W2(Kq1) = 1.0
        W3(Kq1) = 0.0
        !
        W1(Kq2) = 0.0
        W2(Kq2) = 1.0
        !
        if (LQSlop) then
            !----- also enforce slope matching at endpoints
            i = Kq1 + 1
            dsm = W8(i) - W8(i - 1)
            dsp = W8(i + 1) - W8(i)
            ds = W8(i + 1) - W8(i - 1)
            W1(i) = -1.0 / dsm - (dsm / ds) / dsm
            W2(i) = 1.0 / dsm + (dsm / ds) / dsm + (dsm / ds) / dsp
            W3(i) = -(dsm / ds) / dsp
            qspp1 = W1(i) * QSPec(i - 1, Kqsp) + W2(i) * QSPec(i, Kqsp) + W3(i) * QSPec(i + 1, Kqsp)
            !
            i = Kq2 - 1
            dsm = W8(i) - W8(i - 1)
            dsp = W8(i + 1) - W8(i)
            ds = W8(i + 1) - W8(i - 1)
            W1(i) = (dsp / ds) / dsm
            W2(i) = -1.0 / dsp - (dsp / ds) / dsp - (dsp / ds) / dsm
            W3(i) = 1.0 / dsp + (dsp / ds) / dsp
            qspp2 = W1(i) * QSPec(i - 1, Kqsp) + W2(i) * QSPec(i, Kqsp) + W3(i) * QSPec(i + 1, Kqsp)
            !
            QSPec(Kq1 + 1, Kqsp) = qspp1
            QSPec(Kq2 - 1, Kqsp) = qspp2
        endif
        !
        !
        !---- solve for smoothed Qspec array
        call trisol(W2(Kq1), W1(Kq1), W3(Kq1), QSPec(Kq1, Kqsp), (Kq2 - Kq1 + 1))
        !
        !
        !c      IF(LQSYM) THEN
        !c        DO 40 I=KQ1+1, KQ2-1
        !c          QSPEC(NSP-I+1,KQSP) = -QSPEC(I,KQSP)
        !c 40     CONTINUE
        !c      ENDIF
        !
    end subroutine smooq


    function qincom(Qc, Qinf, Tklam)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Qc, Qinf, Tklam
        real :: qincom
        intent (in) Qc, Qinf, Tklam
        !
        ! Local variables
        !
        real :: tmp
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
        !     Sets incompressible speed from
        !     Karman-Tsien compressible speed
        !-------------------------------------
        !
        if (Tklam<1.0E-4 .or. abs(Qc)<1.0E-4) then
            !----- for nearly incompressible case or very small speed, use asymptotic
            !      expansion of singular quadratic formula to avoid numerical problems
            qincom = Qc / (1.0 - Tklam)
        else
            !----- use quadratic formula for typical case
            tmp = 0.5 * (1.0 - Tklam) * Qinf / (Qc * Tklam)
            qincom = Qinf * tmp * (sqrt(1.0 + 1.0 / (Tklam * tmp**2)) - 1.0)
        endif
    end function qincom


    subroutine gamqsp(Kqsp)
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
        !------------------------------------------------
        !     Sets Qspec(s,k) from current speed Q(s).
        !------------------------------------------------
        !
        ALQsp(Kqsp) = ALGam
        CLQsp(Kqsp) = CLGam
        CMQsp(Kqsp) = CMGam
        !
        do i = 1, NSP
            QSPec(i, Kqsp) = QGAmm(i)
        enddo
        !
        !---- zero out Qspec DOFs
        QDOf0 = 0.0
        QDOf1 = 0.0
        QDOf2 = 0.0
        QDOf3 = 0.0
        !
        call splqsp(Kqsp)
        !
        !---- reset target segment endpoints
        if (.not.LIQset) then
            IQ1 = 1
            IQ2 = NSP
        endif
        !
    end subroutine gamqsp

end module m_xqdes
