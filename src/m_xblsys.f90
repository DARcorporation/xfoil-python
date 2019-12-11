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

!*==TRCHEK.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module m_xblsys
contains
    function trchek()
        implicit none
        logical :: trchek
        !
        !*** Start of declarations rewritten by SPAG
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !---- 1st-order amplification equation
        !c      CALL TRCHEK1
        !
        !---- 2nd-order amplification equation
        trchek = trchek2()
        !
    end function trchek
    !*==AXSET.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine axset(Hk1, T1, Rt1, A1, Hk2, T2, Rt2, A2, Acrit, Idampv, Ax, Ax_hk1, Ax_t1, Ax_rt1, Ax_a1, Ax_hk2, Ax_t2, &
            Ax_rt2, Ax_a2)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: A1, A2, Acrit, Ax, Ax_a1, Ax_a2, Ax_hk1, Ax_hk2, Ax_rt1, Ax_rt2, Ax_t1, Ax_t2, Hk1, Hk2, Rt1, &
                & Rt2, T1, T2
        integer :: Idampv
        intent (in) A1, A2, Acrit, Idampv
        intent (out) Ax, Ax_a1, Ax_a2, Ax_hk1, Ax_hk2, Ax_rt1, Ax_rt2, Ax_t1, Ax_t2
        !
        ! Local variables
        !
        real :: arg, ax1, ax1_hk1, ax1_rt1, ax1_t1, ax2, ax2_hk2, ax2_rt2, ax2_t2, axa, axa_ax1, axa_ax2, axsq, &
                & dax, dax_a1, dax_a2, dax_t1, dax_t2, exn, exn_a1, exn_a2
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
        !     Returns average amplification AX over interval 1..2
        !----------------------------------------------------------
        !
        !C==========================
        !C---- 1st-order -- based on "1" quantities only
        !      CALL DAMPL( HK1, T1, RT1, AX1, AX1_HK1, AX1_T1, AX1_RT1 )
        !      AX2_HK2 = 0.0
        !      AX2_T2  = 0.0
        !      AX2_RT2 = 0.0
        !C
        !      AX1_A1 = 0.0
        !      AX2_A2 = 0.0
        !C
        !      AX     = AX1
        !      AX_AX1 = 1.0
        !      AX_AX2 = 0.0
        !C
        !      ARG = MIN( 20.0*(ACRIT-A1) , 20.0 )
        !      EXN    = EXP(-ARG)
        !      EXN_A1 = 20.0*EXN
        !      EXN_A2 = 0.
        !C
        !      DAX    = EXN   * 0.0004/T1
        !      DAX_A1 = EXN_A1* 0.0004/T1
        !      DAX_A2 = 0.
        !      DAX_T1 = -DAX/T1
        !      DAX_T2 = 0.
        !
        !==========================
        !---- 2nd-order
        if (Idampv==0) then
            call dampl(Hk1, T1, Rt1, ax1, ax1_hk1, ax1_t1, ax1_rt1)
            call dampl(Hk2, T2, Rt2, ax2, ax2_hk2, ax2_t2, ax2_rt2)
        else
            call dampl2(Hk1, T1, Rt1, ax1, ax1_hk1, ax1_t1, ax1_rt1)
            call dampl2(Hk2, T2, Rt2, ax2, ax2_hk2, ax2_t2, ax2_rt2)
        endif
        !
        !C---- simple-average version
        !      AXA = 0.5*(AX1 + AX2)
        !      IF(AXA .LE. 0.0) THEN
        !       AXA = 0.0
        !       AXA_AX1 = 0.0
        !       AXA_AX2 = 0.0
        !      ELSE
        !       AXA_AX1 = 0.5
        !       AXA_AX2 = 0.5
        !      ENDIF
        !
        !---- rms-average version (seems a little better on coarse grids)
        axsq = 0.5 * (ax1**2 + ax2**2)
        if (axsq<=0.0) then
            axa = 0.0
            axa_ax1 = 0.0
            axa_ax2 = 0.0
        else
            axa = sqrt(axsq)
            axa_ax1 = 0.5 * ax1 / axa
            axa_ax2 = 0.5 * ax2 / axa
        endif
        !
        !----- small additional term to ensure  dN/dx > 0  near  N = Ncrit
        arg = min(20.0 * (Acrit - 0.5 * (A1 + A2)), 20.0)
        if (arg<=0.0) then
            exn = 1.0
            !C      EXN_AC = 0.
            exn_a1 = 0.
            exn_a2 = 0.
        else
            exn = exp(-arg)
            !C      EXN_AC = -20.0    *EXN
            exn_a1 = 20.0 * 0.5 * exn
            exn_a2 = 20.0 * 0.5 * exn
        endif
        !
        dax = exn * 0.002 / (T1 + T2)
        !C     DAX_AC = EXN_AC * 0.002/(T1+T2)
        dax_a1 = exn_a1 * 0.002 / (T1 + T2)
        dax_a2 = exn_a2 * 0.002 / (T1 + T2)
        dax_t1 = -dax / (T1 + T2)
        dax_t2 = -dax / (T1 + T2)
        !
        !
        !        DAX    = 0.
        !        DAX_A1 = 0.
        !        DAX_A2 = 0.
        !        DAX_AC = 0.
        !        DAX_T1 = 0.
        !        DAX_T2 = 0.
        !==========================
        !
        Ax = axa + dax
        !
        Ax_hk1 = axa_ax1 * ax1_hk1
        Ax_t1 = axa_ax1 * ax1_t1 + dax_t1
        Ax_rt1 = axa_ax1 * ax1_rt1
        Ax_a1 = dax_a1
        !
        Ax_hk2 = axa_ax2 * ax2_hk2
        Ax_t2 = axa_ax2 * ax2_t2 + dax_t2
        Ax_rt2 = axa_ax2 * ax2_rt2
        Ax_a2 = dax_a2
        !
    end subroutine axset
    !*==TRCHEK2.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    !      SUBROUTINE TRCHEK1
    !C-------------------------------------------------
    !C     Checks if transition occurs in the current
    !C     interval 1..2  (IBL-1...IBL) on side IS.
    !C
    !C     Old first-order version.
    !C
    !C     Growth rate is evaluated at the upstream
    !C     point "1". The discrete amplification
    !C     equation is
    !C
    !C       Ncrit - N(X1)
    !C       -------------  =  N'(X1)
    !C          XT - X1
    !C
    !C     which can be immediately solved for
    !C     the transition location XT.
    !C-------------------------------------------------
    !      INCLUDE 'XBL.INC'
    !C
    !C---- calculate AMPL2 value
    !      CALL AXSET( HK1,    T1,    RT1, AMPL1,
    !     &            HK2,    T2,    RT2, AMPL2,  AMCRIT, IDAMPV,
    !     &     AX, AX_HK1, AX_T1, AX_RT1, AX_A1,
    !     &         AX_HK2, AX_T2, AX_RT2, AX_A2 )
    !      AMPL2 = AMPL1 + AX*(X2-X1)
    !C
    !C---- test for free or forced transition
    !      TRFREE = AMPL2.GE.AMCRIT
    !      TRFORC = XIFORC.GT.X1 .AND. XIFORC.LE.X2
    !C
    !C---- set transition interval flag
    !      TRAN = TRFORC .OR. TRFREE
    !C
    !C---- if no transition yet, just return
    !      IF(.NOT.TRAN) RETURN
    !C
    !C---- resolve if both forced and free transition
    !      IF(TRFREE .AND. TRFORC) THEN
    !       XT = (AMCRIT-AMPL1)/AX  +  X1
    !       TRFORC = XIFORC .LT. XT
    !       TRFREE = XIFORC .GE. XT
    !      ENDIF
    !C
    !      IF(TRFORC) THEN
    !C----- if forced transition, then XT is prescribed
    !       XT = XIFORC
    !       XT_A1 = 0.
    !       XT_X1 = 0.
    !       XT_T1 = 0.
    !       XT_D1 = 0.
    !       XT_U1 = 0.
    !       XT_X2 = 0.
    !       XT_T2 = 0.
    !       XT_D2 = 0.
    !       XT_U2 = 0.
    !       XT_MS = 0.
    !       XT_RE = 0.
    !       XT_XF = 1.0
    !      ELSE
    !C----- if free transition, XT is related to BL variables
    !C-     by the amplification equation
    !C
    !       XT    =  (AMCRIT-AMPL1)/AX     + X1
    !       XT_AX = -(AMCRIT-AMPL1)/AX**2
    !C
    !       XT_A1 = -1.0/AX - (AMCRIT-AMPL1)/AX**2 * AX_A1
    !       XT_X1 = 1.0
    !       XT_T1 = XT_AX*(AX_HK1*HK1_T1 + AX_T1 + AX_RT1*RT1_T1)
    !       XT_D1 = XT_AX*(AX_HK1*HK1_D1                        )
    !       XT_U1 = XT_AX*(AX_HK1*HK1_U1         + AX_RT1*RT1_U1)
    !       XT_X2 = 0.
    !       XT_T2 = 0.
    !       XT_D2 = 0.
    !       XT_U2 = 0.
    !       XT_MS = XT_AX*(AX_HK1*HK1_MS         + AX_RT1*RT1_MS)
    !       XT_RE = XT_AX*(                        AX_RT1*RT1_RE)
    !       XT_XF = 0.0
    !      ENDIF
    !C
    !      RETURN
    !      END


    function trchek2()
        use i_xfoil, only: show_output, abort_on_nan
        use i_xbl
        implicit none

        logical :: trchek2
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: amplt, amplt_a2, amsave, ax, ax_a1, ax_a2, ax_at, ax_d1, ax_d2, ax_hk1, ax_hk2, ax_hkt, ax_ms, &
                & ax_re, ax_rt1, ax_rt2, ax_rtt, ax_t1, ax_t2, ax_tt, ax_u1, ax_u2, ax_x1, ax_x2, ax_xf, da2, dt, &
                & dt_a1, dt_a2, dt_d1, dt_d2, dt_x1, dt_x2, dt_xf, dxt, hkt, hkt_dt, hkt_ms, hkt_tt, hkt_ut, res, &
                & res_a2, rlx, rtt, rtt_ms, rtt_re, rtt_tt, rtt_ut, sfa, sfa_a1, sfa_a2, sfx, sfx_x1, sfx_x2, &
                & sfx_xf, tt, tt_a1, tt_a2, tt_t1, tt_t2, tt_x1, tt_x2, tt_xf, ut, ut_a1, ut_a2, ut_u1, ut_u2, &
                & ut_x1, ut_x2, ut_xf, wf1, wf1_a1, wf1_a2, wf1_x1, wf1_x2, wf1_xf, wf2, wf2_a1, wf2_a2
        real, save :: daeps
        integer :: icom, itam
        real :: wf2_x1, wf2_x2, wf2_xf, xt_a2, z_a1, z_a2, z_ax, z_d1, z_d2, z_ms, z_re, z_t1, z_t2, z_u1, &
                & z_u2, z_x1, z_x2, z_xf
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
        !     New second-order version:  December 1994.
        !
        !     Checks if transition occurs in the current interval X1..X2.
        !     If transition occurs, then set transition location XT, and
        !     its sensitivities to "1" and "2" variables.  If no transition,
        !     set amplification AMPL2.
        !
        !
        !     Solves the implicit amplification equation for N2:
        !
        !       N2 - N1     N'(XT,NT) + N'(X1,N1)
        !       -------  =  ---------------------
        !       X2 - X1               2
        !
        !     In effect, a 2-point central difference is used between
        !     X1..X2 (no transition), or X1..XT (transition).  The switch
        !     is done by defining XT,NT in the equation above depending
        !     on whether N2 exceeds Ncrit.
        !
        !  If N2<Ncrit:  NT=N2    , XT=X2                  (no transition)
        !
        !  If N2>Ncrit:  NT=Ncrit , XT=(Ncrit-N1)/(N2-N1)  (transition)
        !
        !
        !----------------------------------------------------------------
        data daeps/5.0E-5/
        !CC   DATA DAEPS / 1.0D-12 /

        trchek2 = .true.
        !
        !---- save variables and sensitivities at IBL ("2") for future restoration
        do icom = 1, NCOM
            C2Sav(icom) = COM2(icom)
        enddo
        !
        !---- calculate average amplification rate AX over X1..X2 interval
        call axset(HK1, T1, RT1, AMPl1, HK2, T2, RT2, AMPl2, AMCrit, IDAmpv, ax, ax_hk1, ax_t1, ax_rt1, ax_a1, ax_hk2, &
                ax_t2, ax_rt2, ax_a2)
        !
        !---- set initial guess for iterate N2 (AMPL2) at X2
        AMPl2 = AMPl1 + ax * (X2 - X1)
        !
        !---- solve implicit system for amplification AMPL2
        do itam = 1, 30
            !
            !---- define weighting factors WF1,WF2 for defining "T" quantities from 1,2
            !
            if (AMPl2<=AMCrit) then
                !------ there is no transition yet,  "T" is the same as "2"
                amplt = AMPl2
                amplt_a2 = 1.0
                sfa = 1.0
                sfa_a1 = 0.
                sfa_a2 = 0.
            else
                !------ there is transition in X1..X2, "T" is set from N1, N2
                amplt = AMCrit
                amplt_a2 = 0.
                sfa = (amplt - AMPl1) / (AMPl2 - AMPl1)
                sfa_a1 = (sfa - 1.0) / (AMPl2 - AMPl1)
                sfa_a2 = (-sfa) / (AMPl2 - AMPl1)
            endif
            !
            if (XIForc<X2) then
                sfx = (XIForc - X1) / (X2 - X1)
                sfx_x1 = (sfx - 1.0) / (X2 - X1)
                sfx_x2 = (-sfx) / (X2 - X1)
                sfx_xf = 1.0 / (X2 - X1)
            else
                sfx = 1.0
                sfx_x1 = 0.
                sfx_x2 = 0.
                sfx_xf = 0.
            endif
            !
            !---- set weighting factor from free or forced transition
            if (sfa<sfx) then
                wf2 = sfa
                wf2_a1 = sfa_a1
                wf2_a2 = sfa_a2
                wf2_x1 = 0.
                wf2_x2 = 0.
                wf2_xf = 0.
            else
                wf2 = sfx
                wf2_a1 = 0.
                wf2_a2 = 0.
                wf2_x1 = sfx_x1
                wf2_x2 = sfx_x2
                wf2_xf = sfx_xf
            endif
            !
            !
            !=====================
            !C---- 1st-order (based on "1" quantites only, for testing)
            !      WF2    = 0.0
            !      WF2_A1 = 0.0
            !      WF2_A2 = 0.0
            !      WF2_X1 = 0.0
            !      WF2_X2 = 0.0
            !      WF2_XF = 0.0
            !=====================
            !
            wf1 = 1.0 - wf2
            wf1_a1 = -wf2_a1
            wf1_a2 = -wf2_a2
            wf1_x1 = -wf2_x1
            wf1_x2 = -wf2_x2
            wf1_xf = -wf2_xf
            !
            !---- interpolate BL variables to XT
            XT = X1 * wf1 + X2 * wf2
            tt = T1 * wf1 + T2 * wf2
            dt = D1 * wf1 + D2 * wf2
            ut = U1 * wf1 + U2 * wf2
            !
            xt_a2 = X1 * wf1_a2 + X2 * wf2_a2
            tt_a2 = T1 * wf1_a2 + T2 * wf2_a2
            dt_a2 = D1 * wf1_a2 + D2 * wf2_a2
            ut_a2 = U1 * wf1_a2 + U2 * wf2_a2
            !
            !---- temporarily set "2" variables from "T" for BLKIN
            X2 = XT
            T2 = tt
            D2 = dt
            U2 = ut
            !
            !---- calculate laminar secondary "T" variables HKT, RTT
            call blkin
            !
            hkt = HK2
            hkt_tt = HK2_t2
            hkt_dt = HK2_d2
            hkt_ut = HK2_u2
            hkt_ms = HK2_ms
            !
            rtt = RT2
            rtt_tt = RT2_t2
            rtt_ut = RT2_u2
            rtt_ms = RT2_ms
            rtt_re = RT2_re
            !
            !---- restore clobbered "2" variables, except for AMPL2
            amsave = AMPl2
            do icom = 1, NCOM
                COM2(icom) = C2Sav(icom)
            enddo
            AMPl2 = amsave
            !
            !---- calculate amplification rate AX over current X1-XT interval
            call axset(HK1, T1, RT1, AMPl1, hkt, tt, rtt, amplt, AMCrit, &
                    IDAmpv, ax, ax_hk1, ax_t1, ax_rt1, ax_a1, ax_hkt, ax_tt, ax_rtt, ax_at)
            !
            !---- punch out early if there is no amplification here
            if (ax<=0.0) goto 100
            !
            !---- set sensitivity of AX(A2)
            ax_a2 = (ax_hkt * hkt_tt + ax_tt + ax_rtt * rtt_tt) * tt_a2 &
                    + (ax_hkt * hkt_dt) * dt_a2 + (ax_hkt * hkt_ut + ax_rtt * rtt_ut) * ut_a2 + ax_at * amplt_a2
            !
            !---- residual for implicit AMPL2 definition (amplification equation)
            res = AMPl2 - AMPl1 - ax * (X2 - X1)
            res_a2 = 1.0 - ax_a2 * (X2 - X1)
            !
            da2 = -res / res_a2
            !
            rlx = 1.0
            dxt = xt_a2 * da2
            !
            if (rlx * abs(dxt / (X2 - X1))>0.05) rlx = 0.05 * abs((X2 - X1) / dxt)
            if (rlx * abs(da2)>1.0) rlx = 1.0 * abs(1.0 / da2)
            !
            !---- check if converged
            if (abs(da2)<daeps) goto 100
            !
            if ((AMPl2>AMCrit .and. AMPl2 + rlx * da2<AMCrit) .or. (AMPl2<AMCrit .and. AMPl2 + rlx * da2>AMCrit)) then
                !------ limited Newton step so AMPL2 doesn't step across AMCRIT either way
                AMPl2 = AMCrit
            else
                !------ regular Newton step
                AMPl2 = AMPl2 + rlx * da2
            endif
            !
        enddo
        if (show_output) then
            write (*, *) 'TRCHEK2: N2 convergence failed.'
            write (*, 99001) X1, XT, X2, AMPl1, amplt, AMPl2, ax, da2
            99001 format (1x, 'x:', 3F9.5, '  N:', 3F7.3, '  Nx:', f8.3, '   dN:', e10.3)
        endif

        ! Check if ANY of these printed variables contain NaN's. If they do, convergence will never be reached.
        if (X1 /= X1 .or. XT /= XT .or. X2 /= X2 .or. AMPl1 /= AMPl1 .or. &
            amplt /= amplt .or. AMPl2 /= AMPl2 .or. ax /= ax .or. da2 /= da2) then
            trchek2 = .false.
            if (abort_on_nan) return
        end if
        !
        !
        !
        !---- test for free or forced transition
        100  TRFree = AMPl2>=AMCrit
        TRForc = XIForc>X1 .and. XIForc<=X2
        !
        !---- set transition interval flag
        TRAn = TRForc .or. TRFree
        !
        if (abort_on_nan .and. .not.TRAn) return
        !
        !---- resolve if both forced and free transition
        if (TRFree .and. TRForc) then
            TRForc = XIForc<XT
            TRFree = XIForc>=XT
        endif
        !
        if (TRForc) then
            !----- if forced transition, then XT is prescribed,
            !-     no sense calculating the sensitivities, since we know them...
            XT = XIForc
            XT_a1 = 0.
            XT_x1 = 0.
            XT_t1 = 0.
            XT_d1 = 0.
            XT_u1 = 0.
            XT_x2 = 0.
            XT_t2 = 0.
            XT_d2 = 0.
            XT_u2 = 0.
            XT_ms = 0.
            XT_re = 0.
            XT_xf = 1.0
            return
        endif
        !
        !---- free transition ... set sensitivities of XT
        !
        !---- XT( X1 X2 A1 A2 XF ),  TT( T1 T2 A1 A2 X1 X2 XF),   DT( ...
        !C    XT    = X1*WF1    + X2*WF2
        !C    TT    = T1*WF1    + T2*WF2
        !C    DT    = D1*WF1    + D2*WF2
        !C    UT    = U1*WF1    + U2*WF2
        !
        XT_x1 = wf1
        tt_t1 = wf1
        dt_d1 = wf1
        ut_u1 = wf1
        !
        XT_x2 = wf2
        tt_t2 = wf2
        dt_d2 = wf2
        ut_u2 = wf2
        !
        XT_a1 = X1 * wf1_a1 + X2 * wf2_a1
        tt_a1 = T1 * wf1_a1 + T2 * wf2_a1
        dt_a1 = D1 * wf1_a1 + D2 * wf2_a1
        ut_a1 = U1 * wf1_a1 + U2 * wf2_a1
        !
        !C    XT_A2 = X1*WF1_A2 + X2*WF2_A2
        !C    TT_A2 = T1*WF1_A2 + T2*WF2_A2
        !C    DT_A2 = D1*WF1_A2 + D2*WF2_A2
        !C    UT_A2 = U1*WF1_A2 + U2*WF2_A2
        !
        XT_x1 = X1 * wf1_x1 + X2 * wf2_x1 + XT_x1
        tt_x1 = T1 * wf1_x1 + T2 * wf2_x1
        dt_x1 = D1 * wf1_x1 + D2 * wf2_x1
        ut_x1 = U1 * wf1_x1 + U2 * wf2_x1
        !
        XT_x2 = X1 * wf1_x2 + X2 * wf2_x2 + XT_x2
        tt_x2 = T1 * wf1_x2 + T2 * wf2_x2
        dt_x2 = D1 * wf1_x2 + D2 * wf2_x2
        ut_x2 = U1 * wf1_x2 + U2 * wf2_x2
        !
        XT_xf = X1 * wf1_xf + X2 * wf2_xf
        tt_xf = T1 * wf1_xf + T2 * wf2_xf
        dt_xf = D1 * wf1_xf + D2 * wf2_xf
        ut_xf = U1 * wf1_xf + U2 * wf2_xf
        !
        !---- at this point, AX = AX( HK1, T1, RT1, A1, HKT, TT, RTT, AT )
        !
        !---- set sensitivities of AX( T1 D1 U1 A1 T2 D2 U2 A2 MS RE )
        ax_t1 = ax_hk1 * HK1_t1 + ax_t1 + ax_rt1 * RT1_t1 + (ax_hkt * hkt_tt + ax_tt + ax_rtt * rtt_tt) * tt_t1
        ax_d1 = ax_hk1 * HK1_d1 + (ax_hkt * hkt_dt) * dt_d1
        ax_u1 = ax_hk1 * HK1_u1 + ax_rt1 * RT1_u1 + (ax_hkt * hkt_ut + ax_rtt * rtt_ut) * ut_u1
        ax_a1 = ax_a1 + (ax_hkt * hkt_tt + ax_tt + ax_rtt * rtt_tt) * tt_a1 + (ax_hkt * hkt_dt) * dt_a1 &
                + (ax_hkt * hkt_ut + ax_rtt * rtt_ut) * ut_a1
        ax_x1 = (ax_hkt * hkt_tt + ax_tt + ax_rtt * rtt_tt) * tt_x1 + (ax_hkt * hkt_dt) * dt_x1 &
                + (ax_hkt * hkt_ut + ax_rtt * rtt_ut) * ut_x1
        !
        ax_t2 = (ax_hkt * hkt_tt + ax_tt + ax_rtt * rtt_tt) * tt_t2
        ax_d2 = (ax_hkt * hkt_dt) * dt_d2
        ax_u2 = (ax_hkt * hkt_ut + ax_rtt * rtt_ut) * ut_u2
        ax_a2 = ax_at * amplt_a2 + (ax_hkt * hkt_tt + ax_tt + ax_rtt * rtt_tt) * tt_a2 + (ax_hkt * hkt_dt) &
                & * dt_a2 + (ax_hkt * hkt_ut + ax_rtt * rtt_ut) * ut_a2
        ax_x2 = (ax_hkt * hkt_tt + ax_tt + ax_rtt * rtt_tt) * tt_x2 + (ax_hkt * hkt_dt) * dt_x2 &
                + (ax_hkt * hkt_ut + ax_rtt * rtt_ut) * ut_x2
        !
        ax_xf = (ax_hkt * hkt_tt + ax_tt + ax_rtt * rtt_tt) * tt_xf + (ax_hkt * hkt_dt) * dt_xf &
                + (ax_hkt * hkt_ut + ax_rtt * rtt_ut) * ut_xf
        !
        ax_ms = ax_hkt * hkt_ms + ax_rtt * rtt_ms + ax_hk1 * HK1_ms + ax_rt1 * RT1_ms
        ax_re = ax_rtt * rtt_re + ax_rt1 * RT1_re
        !
        !
        !---- set sensitivities of residual RES
        !CC   RES  = AMPL2 - AMPL1 - AX*(X2-X1)
        z_ax = -(X2 - X1)
        !
        z_a1 = z_ax * ax_a1 - 1.0
        z_t1 = z_ax * ax_t1
        z_d1 = z_ax * ax_d1
        z_u1 = z_ax * ax_u1
        z_x1 = z_ax * ax_x1 + ax
        !
        z_a2 = z_ax * ax_a2 + 1.0
        z_t2 = z_ax * ax_t2
        z_d2 = z_ax * ax_d2
        z_u2 = z_ax * ax_u2
        z_x2 = z_ax * ax_x2 - ax
        !
        z_xf = z_ax * ax_xf
        z_ms = z_ax * ax_ms
        z_re = z_ax * ax_re
        !
        !---- set sensitivities of XT, with RES being stationary for A2 constraint
        XT_a1 = XT_a1 - (xt_a2 / z_a2) * z_a1
        XT_t1 = -(xt_a2 / z_a2) * z_t1
        XT_d1 = -(xt_a2 / z_a2) * z_d1
        XT_u1 = -(xt_a2 / z_a2) * z_u1
        XT_x1 = XT_x1 - (xt_a2 / z_a2) * z_x1
        XT_t2 = -(xt_a2 / z_a2) * z_t2
        XT_d2 = -(xt_a2 / z_a2) * z_d2
        XT_u2 = -(xt_a2 / z_a2) * z_u2
        XT_x2 = XT_x2 - (xt_a2 / z_a2) * z_x2
        XT_ms = -(xt_a2 / z_a2) * z_ms
        XT_re = -(xt_a2 / z_a2) * z_re
        XT_xf = 0.0
        !
    end function trchek2
    !*==BLSYS.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine blsys
        !------------------------------------------------------------------
        !
        !     Sets up the BL Newton system governing the current interval:
        !
        !     |       ||dA1|     |       ||dA2|       |     |
        !     |  VS1  ||dT1|  +  |  VS2  ||dT2|   =   |VSREZ|
        !     |       ||dD1|     |       ||dD2|       |     |
        !              |dU1|              |dU2|
        !              |dX1|              |dX2|
        !
        !        3x5    5x1         3x5    5x1          3x1
        !
        !     The system as shown corresponds to a laminar station
        !     If TRAN, then  dS2  replaces  dA2
        !     If TURB, then  dS1, dS2  replace  dA1, dA2
        !
        !------------------------------------------------------------------
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: icom, k, l
        real :: res_ms, res_u1, res_u2
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
        !---- calculate secondary BL variables and their sensitivities
        if (WAKe) then
            call blvar(3)
            call blmid(3)
        elseif (TURb .or. TRAn) then
            call blvar(2)
            call blmid(2)
        else
            call blvar(1)
            call blmid(1)
        endif
        !
        !---- for the similarity station, "1" and "2" variables are the same
        if (SIMi) then
            do icom = 1, NCOM
                COM1(icom) = COM2(icom)
            enddo
        endif
        !
        !---- set up appropriate finite difference system for current interval
        if (TRAn) then
            call trdif
        elseif (SIMi) then
            call bldif(0)
        elseif (.not.TURb) then
            call bldif(1)
        elseif (WAKe) then
            call bldif(3)
        elseif (TURb) then
            call bldif(2)
        endif
        !
        if (SIMi) then
            !----- at similarity station, "1" variables are really "2" variables
            do k = 1, 4
                do l = 1, 5
                    VS2(k, l) = VS1(k, l) + VS2(k, l)
                    VS1(k, l) = 0.
                enddo
            enddo
        endif
        !
        !---- change system over into incompressible Uei and Mach
        do k = 1, 4
            !
            !------ residual derivatives wrt compressible Uec
            res_u1 = VS1(k, 4)
            res_u2 = VS2(k, 4)
            res_ms = VSM(k)
            !
            !------ combine with derivatives of compressible  U1,U2 = Uec(Uei M)
            VS1(k, 4) = res_u1 * U1_uei
            VS2(k, 4) = res_u2 * U2_uei
            VSM(k) = res_u1 * U1_ms + res_u2 * U2_ms + res_ms
        enddo
        !
    end subroutine blsys
    !*==TESYS.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine tesys(Cte, Tte, Dte)
        !--------------------------------------------------------
        !     Sets up "dummy" BL system between airfoil TE point
        !     and first wake point infinitesimally behind TE.
        !--------------------------------------------------------
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Cte, Dte, Tte
        intent (in) Cte, Dte, Tte
        !
        ! Local variables
        !
        integer :: k, l
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
        do k = 1, 4
            VSRez(k) = 0.
            VSM(k) = 0.
            VSR(k) = 0.
            VSX(k) = 0.
            do l = 1, 5
                VS1(k, l) = 0.
                VS2(k, l) = 0.
            enddo
        enddo
        !
        call blvar(3)
        !
        VS1(1, 1) = -1.0
        VS2(1, 1) = 1.0
        VSRez(1) = Cte - S2
        !
        VS1(2, 2) = -1.0
        VS2(2, 2) = 1.0
        VSRez(2) = Tte - T2
        !
        VS1(3, 3) = -1.0
        VS2(3, 3) = 1.0
        VSRez(3) = Dte - D2 - DW2
        !
    end subroutine tesys
    !*==BLPRV.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine blprv(Xsi, Ami, Cti, Thi, Dsi, Dswaki, Uei)
        !----------------------------------------------------------
        !     Set BL primary "2" variables from parameter list
        !----------------------------------------------------------
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Ami, Cti, Dsi, Dswaki, Thi, Uei, Xsi
        intent (in) Ami, Cti, Dsi, Dswaki, Thi, Uei, Xsi
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
        X2 = Xsi
        AMPl2 = Ami
        S2 = Cti
        T2 = Thi
        D2 = Dsi - Dswaki
        DW2 = Dswaki
        !
        U2 = Uei * (1.0 - TKBl) / (1.0 - TKBl * (Uei / QINfbl)**2)
        U2_uei = (1.0 + TKBl * (2.0 * U2 * Uei / QINfbl**2 - 1.0)) / (1.0 - TKBl * (Uei / QINfbl)**2)
        U2_ms = (U2 * (Uei / QINfbl)**2 - Uei) * TKBl_ms / (1.0 - TKBl * (Uei / QINfbl)**2)
        !
    end subroutine blprv
    !*==BLKIN.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! BLPRV


    subroutine blkin
        !----------------------------------------------------------
        !     Calculates turbulence-independent secondary "2"
        !     variables from the primary "2" variables.
        !----------------------------------------------------------
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: herat, he_ms, he_u2, hk2_h2, hk2_m2, tr2, v2_he
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
        !---- set edge Mach number ** 2
        M2 = U2 * U2 * HSTinv / (GM1bl * (1.0 - 0.5 * U2 * U2 * HSTinv))
        tr2 = 1.0 + 0.5 * GM1bl * M2
        M2_u2 = 2.0 * M2 * tr2 / U2
        M2_ms = U2 * U2 * tr2 / (GM1bl * (1.0 - 0.5 * U2 * U2 * HSTinv)) * HSTinv_ms
        !
        !---- set edge static density (isentropic relation)
        R2 = RSTbl * tr2**(-1.0 / GM1bl)
        R2_u2 = -R2 / tr2 * 0.5 * M2_u2
        R2_ms = -R2 / tr2 * 0.5 * M2_ms + RSTbl_ms * tr2**(-1.0 / GM1bl)
        !
        !---- set shape parameter
        H2 = D2 / T2
        H2_d2 = 1.0 / T2
        H2_t2 = -H2 / T2
        !
        !---- set edge static/stagnation enthalpy
        herat = 1.0 - 0.5 * U2 * U2 * HSTinv
        he_u2 = -U2 * HSTinv
        he_ms = -0.5 * U2 * U2 * HSTinv_ms
        !
        !---- set molecular viscosity
        V2 = sqrt((herat)**3) * (1.0 + HVRat) / (herat + HVRat) / REYbl
        v2_he = V2 * (1.5 / herat - 1.0 / (herat + HVRat))
        !
        V2_u2 = v2_he * he_u2
        V2_ms = -V2 / REYbl * REYbl_ms + v2_he * he_ms
        V2_re = -V2 / REYbl * REYbl_re
        !
        !---- set kinematic shape parameter
        call hkin(H2, M2, HK2, hk2_h2, hk2_m2)
        !
        HK2_u2 = hk2_m2 * M2_u2
        HK2_t2 = hk2_h2 * H2_t2
        HK2_d2 = hk2_h2 * H2_d2
        HK2_ms = hk2_m2 * M2_ms
        !
        !---- set momentum thickness Reynolds number
        RT2 = R2 * U2 * T2 / V2
        RT2_u2 = RT2 * (1.0 / U2 + R2_u2 / R2 - V2_u2 / V2)
        RT2_t2 = RT2 / T2
        RT2_ms = RT2 * (R2_ms / R2 - V2_ms / V2)
        RT2_re = RT2 * (-V2_re / V2)
        !
    end subroutine blkin
    !*==BLVAR.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! BLKIN



    subroutine blvar(Ityp)
        !----------------------------------------------------
        !     Calculates all secondary "2" variables from
        !     the primary "2" variables X2, U2, T2, D2, S2.
        !     Also calculates the sensitivities of the
        !     secondary variables wrt the primary variables.
        !
        !      ITYP = 1 :  laminar
        !      ITYP = 2 :  turbulent
        !      ITYP = 3 :  turbulent wake
        !----------------------------------------------------
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ityp
        intent (in) Ityp
        !
        ! Local variables
        !
        real :: cf2l, cf2l_hk2, cf2l_m2, cf2l_rt2, cf2t, cf2t_d2, cf2t_hk2, cf2t_m2, cf2t_ms, cf2t_re, cf2t_rt2, &
                & cf2t_t2, cf2t_u2, cf2_hk2, cf2_m2, cf2_rt2, cq2_h2, cq2_hk2, cq2_hs2, cq2_rt2, cq2_us2, dd, &
                & dd_hs2, dd_rt2, dd_s2, dd_us2, de2_hk2, dfac, df_fl, df_hk2, df_rt2, di2l, di2l_hk2, di2l_rt2, &
                & di2_cf2t, di2_hk2, di2_hs2, di2_rt2, di2_us2, fl, fl_hk2, fl_rt2, gcc, grt, hc2_hk2, hc2_m2, &
                & hdmax, hkb, hkc, hkc_hk2, hkc_rt2, hmin, hm_rt2, hs2_hk2, hs2_m2, hs2_rt2, tfl, us2_h2, us2_hk2, &
                & us2_hs2, usb
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
        if (Ityp==3) HK2 = max(HK2, 1.00005)
        if (Ityp/=3) HK2 = max(HK2, 1.05000)
        !
        !---- density thickness shape parameter     ( H** )
        call hct(HK2, M2, HC2, hc2_hk2, hc2_m2)
        HC2_u2 = hc2_hk2 * HK2_u2 + hc2_m2 * M2_u2
        HC2_t2 = hc2_hk2 * HK2_t2
        HC2_d2 = hc2_hk2 * HK2_d2
        HC2_ms = hc2_hk2 * HK2_ms + hc2_m2 * M2_ms
        !
        !---- set KE thickness shape parameter from  H - H*  correlations
        if (Ityp==1) then
            call hsl(HK2, RT2, M2, HS2, hs2_hk2, hs2_rt2, hs2_m2)
        else
            call hst(HK2, RT2, M2, HS2, hs2_hk2, hs2_rt2, hs2_m2)
        endif
        !
        HS2_u2 = hs2_hk2 * HK2_u2 + hs2_rt2 * RT2_u2 + hs2_m2 * M2_u2
        HS2_t2 = hs2_hk2 * HK2_t2 + hs2_rt2 * RT2_t2
        HS2_d2 = hs2_hk2 * HK2_d2
        HS2_ms = hs2_hk2 * HK2_ms + hs2_rt2 * RT2_ms + hs2_m2 * M2_ms
        HS2_re = hs2_rt2 * RT2_re
        !
        !---- normalized slip velocity  Us
        US2 = 0.5 * HS2 * (1.0 - (HK2 - 1.0) / (GBCon * H2))
        us2_hs2 = 0.5 * (1.0 - (HK2 - 1.0) / (GBCon * H2))
        us2_hk2 = 0.5 * HS2 * (-1.0 / (GBCon * H2))
        us2_h2 = 0.5 * HS2 * (HK2 - 1.0) / (GBCon * H2**2)
        !
        US2_u2 = us2_hs2 * HS2_u2 + us2_hk2 * HK2_u2
        US2_t2 = us2_hs2 * HS2_t2 + us2_hk2 * HK2_t2 + us2_h2 * H2_t2
        US2_d2 = us2_hs2 * HS2_d2 + us2_hk2 * HK2_d2 + us2_h2 * H2_d2
        US2_ms = us2_hs2 * HS2_ms + us2_hk2 * HK2_ms
        US2_re = us2_hs2 * HS2_re
        !
        if (Ityp<=2 .and. US2>0.95) then
            !CC       WRITE(*,*) 'BLVAR: Us clamped:', US2
            US2 = 0.98
            US2_u2 = 0.
            US2_t2 = 0.
            US2_d2 = 0.
            US2_ms = 0.
            US2_re = 0.
        endif
        !
        if (Ityp==3 .and. US2>0.99995) then
            !CC       WRITE(*,*) 'BLVAR: Wake Us clamped:', US2
            US2 = 0.99995
            US2_u2 = 0.
            US2_t2 = 0.
            US2_d2 = 0.
            US2_ms = 0.
            US2_re = 0.
        endif
        !
        !---- equilibrium wake layer shear coefficient (Ctau)EQ ** 1/2
        !   ...  NEW  12 Oct 94
        gcc = 0.0
        hkc = HK2 - 1.0
        hkc_hk2 = 1.0
        hkc_rt2 = 0.0
        if (Ityp==2) then
            gcc = GCCon
            hkc = HK2 - 1.0 - gcc / RT2
            hkc_hk2 = 1.0
            hkc_rt2 = gcc / RT2**2
            if (hkc<0.01) then
                hkc = 0.01
                hkc_hk2 = 0.0
                hkc_rt2 = 0.0
            endif
        endif
        !
        hkb = HK2 - 1.0
        usb = 1.0 - US2
        CQ2 = sqrt(CTCon * HS2 * hkb * hkc**2 / (usb * H2 * HK2**2))
        cq2_hs2 = CTCon * hkb * hkc**2 / (usb * H2 * HK2**2) * 0.5 / CQ2
        cq2_us2 = CTCon * HS2 * hkb * hkc**2 / (usb * H2 * HK2**2) / usb * 0.5 / CQ2
        cq2_hk2 = CTCon * HS2 * hkc**2 / (usb * H2 * HK2**2) * 0.5 / CQ2 &
                - CTCon * HS2 * hkb * hkc**2 / (usb * H2 * HK2**3) * 2.0 * 0.5 / CQ2 &
                + CTCon * HS2 * hkb * hkc / (usb * H2 * HK2**2) * 2.0 * 0.5 / CQ2 * hkc_hk2
        cq2_rt2 = CTCon * HS2 * hkb * hkc / (usb * H2 * HK2**2) * 2.0 * 0.5 / CQ2 * hkc_rt2
        cq2_h2 = -CTCon * HS2 * hkb * hkc**2 / (usb * H2 * HK2**2) / H2 * 0.5 / CQ2
        !
        CQ2_u2 = cq2_hs2 * HS2_u2 + cq2_us2 * US2_u2 + cq2_hk2 * HK2_u2
        CQ2_t2 = cq2_hs2 * HS2_t2 + cq2_us2 * US2_t2 + cq2_hk2 * HK2_t2
        CQ2_d2 = cq2_hs2 * HS2_d2 + cq2_us2 * US2_d2 + cq2_hk2 * HK2_d2
        CQ2_ms = cq2_hs2 * HS2_ms + cq2_us2 * US2_ms + cq2_hk2 * HK2_ms
        CQ2_re = cq2_hs2 * HS2_re + cq2_us2 * US2_re
        !
        CQ2_u2 = CQ2_u2 + cq2_rt2 * RT2_u2
        CQ2_t2 = CQ2_t2 + cq2_h2 * H2_t2 + cq2_rt2 * RT2_t2
        CQ2_d2 = CQ2_d2 + cq2_h2 * H2_d2
        CQ2_ms = CQ2_ms + cq2_rt2 * RT2_ms
        CQ2_re = CQ2_re + cq2_rt2 * RT2_re
        !
        !
        !---- set skin friction coefficient
        if (Ityp==3) then
            !----- wake
            CF2 = 0.
            cf2_hk2 = 0.
            cf2_rt2 = 0.
            cf2_m2 = 0.
        elseif (Ityp==1) then
            !----- laminar
            call cfl(HK2, RT2, M2, CF2, cf2_hk2, cf2_rt2, cf2_m2)
        else
            !----- turbulent
            call cft(HK2, RT2, M2, CF2, cf2_hk2, cf2_rt2, cf2_m2)
            call cfl(HK2, RT2, M2, cf2l, cf2l_hk2, cf2l_rt2, cf2l_m2)
            if (cf2l>CF2) then
                !------- laminar Cf is greater than turbulent Cf -- use laminar
                !-       (this will only occur for unreasonably small Rtheta)
                !cc      write(*,*) 'Cft Cfl Rt Hk:', CF2, CF2L, RT2, HK2, X2
                CF2 = cf2l
                cf2_hk2 = cf2l_hk2
                cf2_rt2 = cf2l_rt2
                cf2_m2 = cf2l_m2
            endif
        endif
        !
        CF2_u2 = cf2_hk2 * HK2_u2 + cf2_rt2 * RT2_u2 + cf2_m2 * M2_u2
        CF2_t2 = cf2_hk2 * HK2_t2 + cf2_rt2 * RT2_t2
        CF2_d2 = cf2_hk2 * HK2_d2
        CF2_ms = cf2_hk2 * HK2_ms + cf2_rt2 * RT2_ms + cf2_m2 * M2_ms
        CF2_re = cf2_rt2 * RT2_re
        !
        !---- dissipation function    2 CD / H*
        if (Ityp==1) then
            !
            !----- laminar
            call dil(HK2, RT2, DI2, di2_hk2, di2_rt2)
            !
            DI2_u2 = di2_hk2 * HK2_u2 + di2_rt2 * RT2_u2
            DI2_t2 = di2_hk2 * HK2_t2 + di2_rt2 * RT2_t2
            DI2_d2 = di2_hk2 * HK2_d2
            DI2_s2 = 0.
            DI2_ms = di2_hk2 * HK2_ms + di2_rt2 * RT2_ms
            DI2_re = di2_rt2 * RT2_re
            !
        elseif (Ityp==2) then
            !
            !CC       CALL DIT(     HS2,     US2,     CF2,     S2, DI2,
            !CC     &           DI2_HS2, DI2_US2, DI2_CF2, DI2_S2      )
            !
            !----- turbulent wall contribution
            call cft(HK2, RT2, M2, cf2t, cf2t_hk2, cf2t_rt2, cf2t_m2)
            cf2t_u2 = cf2t_hk2 * HK2_u2 + cf2t_rt2 * RT2_u2 + cf2t_m2 * M2_u2
            cf2t_t2 = cf2t_hk2 * HK2_t2 + cf2t_rt2 * RT2_t2
            cf2t_d2 = cf2t_hk2 * HK2_d2
            cf2t_ms = cf2t_hk2 * HK2_ms + cf2t_rt2 * RT2_ms + cf2t_m2 * M2_ms
            cf2t_re = cf2t_rt2 * RT2_re
            !
            DI2 = (0.5 * cf2t * US2) * 2.0 / HS2
            di2_hs2 = -(0.5 * cf2t * US2) * 2.0 / HS2**2
            di2_us2 = (0.5 * cf2t) * 2.0 / HS2
            di2_cf2t = (0.5 * US2) * 2.0 / HS2
            !
            DI2_s2 = 0.0
            DI2_u2 = di2_hs2 * HS2_u2 + di2_us2 * US2_u2 + di2_cf2t * cf2t_u2
            DI2_t2 = di2_hs2 * HS2_t2 + di2_us2 * US2_t2 + di2_cf2t * cf2t_t2
            DI2_d2 = di2_hs2 * HS2_d2 + di2_us2 * US2_d2 + di2_cf2t * cf2t_d2
            DI2_ms = di2_hs2 * HS2_ms + di2_us2 * US2_ms + di2_cf2t * cf2t_ms
            DI2_re = di2_hs2 * HS2_re + di2_us2 * US2_re + di2_cf2t * cf2t_re
            !
            !
            !----- set minimum Hk for wake layer to still exist
            grt = log(RT2)
            hmin = 1.0 + 2.1 / grt
            hm_rt2 = -(2.1 / grt**2) / RT2
            !
            !----- set factor DFAC for correcting wall dissipation for very low Hk
            fl = (HK2 - 1.0) / (hmin - 1.0)
            fl_hk2 = 1.0 / (hmin - 1.0)
            fl_rt2 = (-fl / (hmin - 1.0)) * hm_rt2
            !
            tfl = tanh(fl)
            dfac = 0.5 + 0.5 * tfl
            df_fl = 0.5 * (1.0 - tfl**2)
            !
            df_hk2 = df_fl * fl_hk2
            df_rt2 = df_fl * fl_rt2
            !
            DI2_s2 = DI2_s2 * dfac
            DI2_u2 = DI2_u2 * dfac + DI2 * (df_hk2 * HK2_u2 + df_rt2 * RT2_u2)
            DI2_t2 = DI2_t2 * dfac + DI2 * (df_hk2 * HK2_t2 + df_rt2 * RT2_t2)
            DI2_d2 = DI2_d2 * dfac + DI2 * (df_hk2 * HK2_d2)
            DI2_ms = DI2_ms * dfac + DI2 * (df_hk2 * HK2_ms + df_rt2 * RT2_ms)
            DI2_re = DI2_re * dfac + DI2 * (df_rt2 * RT2_re)
            DI2 = DI2 * dfac
            !
        else
            !
            !----- zero wall contribution for wake
            DI2 = 0.0
            DI2_s2 = 0.0
            DI2_u2 = 0.0
            DI2_t2 = 0.0
            DI2_d2 = 0.0
            DI2_ms = 0.0
            DI2_re = 0.0
            !
        endif
        !
        !
        !---- Add on turbulent outer layer contribution
        if (Ityp/=1) then
            !
            dd = S2**2 * (0.995 - US2) * 2.0 / HS2
            dd_hs2 = -S2**2 * (0.995 - US2) * 2.0 / HS2**2
            dd_us2 = -S2**2 * 2.0 / HS2
            dd_s2 = S2 * 2.0 * (0.995 - US2) * 2.0 / HS2
            !
            DI2 = DI2 + dd
            DI2_s2 = dd_s2
            DI2_u2 = DI2_u2 + dd_hs2 * HS2_u2 + dd_us2 * US2_u2
            DI2_t2 = DI2_t2 + dd_hs2 * HS2_t2 + dd_us2 * US2_t2
            DI2_d2 = DI2_d2 + dd_hs2 * HS2_d2 + dd_us2 * US2_d2
            DI2_ms = DI2_ms + dd_hs2 * HS2_ms + dd_us2 * US2_ms
            DI2_re = DI2_re + dd_hs2 * HS2_re + dd_us2 * US2_re
            !
            !----- add laminar stress contribution to outer layer CD
            !###
            dd = 0.15 * (0.995 - US2)**2 / RT2 * 2.0 / HS2
            dd_us2 = -0.15 * (0.995 - US2) * 2. / RT2 * 2.0 / HS2
            dd_hs2 = -dd / HS2
            dd_rt2 = -dd / RT2
            !
            DI2 = DI2 + dd
            DI2_u2 = DI2_u2 + dd_hs2 * HS2_u2 + dd_us2 * US2_u2 + dd_rt2 * RT2_u2
            DI2_t2 = DI2_t2 + dd_hs2 * HS2_t2 + dd_us2 * US2_t2 + dd_rt2 * RT2_t2
            DI2_d2 = DI2_d2 + dd_hs2 * HS2_d2 + dd_us2 * US2_d2
            DI2_ms = DI2_ms + dd_hs2 * HS2_ms + dd_us2 * US2_ms + dd_rt2 * RT2_ms
            DI2_re = DI2_re + dd_hs2 * HS2_re + dd_us2 * US2_re + dd_rt2 * RT2_re
            !
        endif
        !
        !
        if (Ityp==2) then
            call dil(HK2, RT2, di2l, di2l_hk2, di2l_rt2)
            !
            if (di2l>DI2) then
                !------- laminar CD is greater than turbulent CD -- use laminar
                !-       (this will only occur for unreasonably small Rtheta)
                !cc       write(*,*) 'CDt CDl Rt Hk:', DI2, DI2L, RT2, HK2
                DI2 = di2l
                DI2_s2 = 0.
                DI2_u2 = di2l_hk2 * HK2_u2 + di2l_rt2 * RT2_u2
                DI2_t2 = di2l_hk2 * HK2_t2 + di2l_rt2 * RT2_t2
                DI2_d2 = di2l_hk2 * HK2_d2
                DI2_ms = di2l_hk2 * HK2_ms + di2l_rt2 * RT2_ms
                DI2_re = di2l_rt2 * RT2_re
            endif
        endif
        !
        !C----- add on CD contribution of inner shear layer
        !       IF(ITYP.EQ.3 .AND. DW2.GT.0.0) THEN
        !        DKON = 0.03*0.75**3
        !        DDI = DKON*US2**3
        !        DDI_US2 = 3.0*DKON*US2**2
        !        DI2 = DI2 + DDI * DW2/DWTE
        !        DI2_U2 = DI2_U2 + DDI_US2*US2_U2 * DW2/DWTE
        !        DI2_T2 = DI2_T2 + DDI_US2*US2_T2 * DW2/DWTE
        !        DI2_D2 = DI2_D2 + DDI_US2*US2_D2 * DW2/DWTE
        !        DI2_MS = DI2_MS + DDI_US2*US2_MS * DW2/DWTE
        !        DI2_RE = DI2_RE + DDI_US2*US2_RE * DW2/DWTE
        !       ENDIF
        !
        if (Ityp==3) then
            !------ laminar wake CD
            call dilw(HK2, RT2, di2l, di2l_hk2, di2l_rt2)
            if (di2l>DI2) then
                !c        IF(.true.) THEN
                !------- laminar wake CD is greater than turbulent CD -- use laminar
                !-       (this will only occur for unreasonably small Rtheta)
                !cc         write(*,*) 'CDt CDl Rt Hk:', DI2, DI2L, RT2, HK2
                DI2 = di2l
                DI2_s2 = 0.
                DI2_u2 = di2l_hk2 * HK2_u2 + di2l_rt2 * RT2_u2
                DI2_t2 = di2l_hk2 * HK2_t2 + di2l_rt2 * RT2_t2
                DI2_d2 = di2l_hk2 * HK2_d2
                DI2_ms = di2l_hk2 * HK2_ms + di2l_rt2 * RT2_ms
                DI2_re = di2l_rt2 * RT2_re
            endif
        endif
        !
        !
        if (Ityp==3) then
            !----- double dissipation for the wake (two wake halves)
            DI2 = DI2 * 2.0
            DI2_s2 = DI2_s2 * 2.0
            DI2_u2 = DI2_u2 * 2.0
            DI2_t2 = DI2_t2 * 2.0
            DI2_d2 = DI2_d2 * 2.0
            DI2_ms = DI2_ms * 2.0
            DI2_re = DI2_re * 2.0
        endif
        !
        !---- BL thickness (Delta) from simplified Green's correlation
        DE2 = (3.15 + 1.72 / (HK2 - 1.0)) * T2 + D2
        de2_hk2 = (-1.72 / (HK2 - 1.0)**2) * T2
        !
        DE2_u2 = de2_hk2 * HK2_u2
        DE2_t2 = de2_hk2 * HK2_t2 + (3.15 + 1.72 / (HK2 - 1.0))
        DE2_d2 = de2_hk2 * HK2_d2 + 1.0
        DE2_ms = de2_hk2 * HK2_ms
        !
        !cc      HDMAX = 15.0
        hdmax = 12.0
        if (DE2>hdmax * T2) then
            !ccc      IF(DE2 .GT. HDMAX*T2 .AND. (HK2 .GT. 4.0 .OR. ITYP.EQ.3)) THEN
            DE2 = hdmax * T2
            DE2_u2 = 0.0
            DE2_t2 = hdmax
            DE2_d2 = 0.0
            DE2_ms = 0.0
        endif
        !
    end subroutine blvar
    !*==BLMID.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine blmid(Ityp)
        !----------------------------------------------------
        !     Calculates midpoint skin friction CFM
        !
        !      ITYP = 1 :  laminar
        !      ITYP = 2 :  turbulent
        !      ITYP = 3 :  turbulent wake
        !----------------------------------------------------
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ityp
        intent (in) Ityp
        !
        ! Local variables
        !
        real :: cfml, cfml_hka, cfml_ma, cfml_rta, cfm_hka, cfm_ma, cfm_rta, hka, ma, rta
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
        !---- set similarity variables if not defined
        if (SIMi) then
            HK1 = HK2
            HK1_t1 = HK2_t2
            HK1_d1 = HK2_d2
            HK1_u1 = HK2_u2
            HK1_ms = HK2_ms
            RT1 = RT2
            RT1_t1 = RT2_t2
            RT1_u1 = RT2_u2
            RT1_ms = RT2_ms
            RT1_re = RT2_re
            M1 = M2
            M1_u1 = M2_u2
            M1_ms = M2_ms
        endif
        !
        !---- define stuff for midpoint CF
        hka = 0.5 * (HK1 + HK2)
        rta = 0.5 * (RT1 + RT2)
        ma = 0.5 * (M1 + M2)
        !
        !---- midpoint skin friction coefficient  (zero in wake)
        if (Ityp==3) then
            CFM = 0.
            cfm_hka = 0.
            cfm_rta = 0.
            cfm_ma = 0.
            CFM_ms = 0.
        elseif (Ityp==1) then
            call cfl(hka, rta, ma, CFM, cfm_hka, cfm_rta, cfm_ma)
        else
            call cft(hka, rta, ma, CFM, cfm_hka, cfm_rta, cfm_ma)
            call cfl(hka, rta, ma, cfml, cfml_hka, cfml_rta, cfml_ma)
            if (cfml>CFM) then
                !cc      write(*,*) 'Cft Cfl Rt Hk:', CFM, CFML, RTA, HKA, 0.5*(X1+X2)
                CFM = cfml
                cfm_hka = cfml_hka
                cfm_rta = cfml_rta
                cfm_ma = cfml_ma
            endif
        endif
        !
        CFM_u1 = 0.5 * (cfm_hka * HK1_u1 + cfm_ma * M1_u1 + cfm_rta * RT1_u1)
        CFM_t1 = 0.5 * (cfm_hka * HK1_t1 + cfm_rta * RT1_t1)
        CFM_d1 = 0.5 * (cfm_hka * HK1_d1)
        !
        CFM_u2 = 0.5 * (cfm_hka * HK2_u2 + cfm_ma * M2_u2 + cfm_rta * RT2_u2)
        CFM_t2 = 0.5 * (cfm_hka * HK2_t2 + cfm_rta * RT2_t2)
        CFM_d2 = 0.5 * (cfm_hka * HK2_d2)
        !
        CFM_ms = 0.5 * (cfm_hka * HK1_ms + cfm_ma * M1_ms + cfm_rta * RT1_ms &
                + cfm_hka * HK2_ms + cfm_ma * M2_ms + cfm_rta * RT2_ms)
        CFM_re = 0.5 * (cfm_rta * RT1_re + cfm_rta * RT2_re)
        !
    end subroutine blmid
    !*==TRDIF.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! BLMID


    subroutine trdif
        !-----------------------------------------------
        !     Sets up the Newton system governing the
        !     transition interval.  Equations governing
        !     the  laminar  part  X1 < xi < XT  and
        !     the turbulent part  XT < xi < X2
        !     are simply summed.
        !-----------------------------------------------
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real, dimension(4, 5) :: bl1, bl2, bt1, bt2
        real, dimension(4) :: blm, blr, blrez, blx, btm, btr, btrez, btx
        real :: ctr, ctr_hk2, dt, dt_a1, dt_d1, dt_d2, dt_ms, dt_re, dt_t1, dt_t2, dt_u1, dt_u2, dt_x1, dt_x2, &
                & dt_xf, st, st_a1, st_d1, st_d2, st_dt, st_ms, st_re, st_t1, st_t2, st_tt, st_u1, st_u2, st_ut, &
                & st_x1, st_x2, st_xf, tt, tt_a1, tt_d1, tt_d2, tt_ms, tt_re, tt_t1, tt_t2, tt_u1, tt_u2, tt_x1, &
                & tt_x2, tt_xf, ut, ut_a1, ut_d1, ut_d2, ut_ms, ut_re, ut_t1, ut_t2, ut_u1, ut_u2, ut_x1, ut_x2, &
                & ut_xf, wf1, wf1_a1, wf1_d1, wf1_d2, wf1_ms, wf1_re, wf1_t1, wf1_t2, wf1_u1, wf1_u2, wf1_x1, &
                & wf1_x2, wf1_xf, wf2, wf2_a1, wf2_d1, wf2_d2, wf2_ms, wf2_re, wf2_t1, wf2_t2, wf2_u1, wf2_u2
        integer :: icom, k, l
        real :: wf2_x1, wf2_x2, wf2_xf, wf2_xt
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
        !---- save variables and sensitivities for future restoration
        do icom = 1, NCOM
            C1Sav(icom) = COM1(icom)
            C2Sav(icom) = COM2(icom)
        enddo
        !
        !---- weighting factors for linear interpolation to transition point
        wf2 = (XT - X1) / (X2 - X1)
        wf2_xt = 1.0 / (X2 - X1)
        !
        wf2_a1 = wf2_xt * XT_a1
        wf2_x1 = wf2_xt * XT_x1 + (wf2 - 1.0) / (X2 - X1)
        wf2_x2 = wf2_xt * XT_x2 - wf2 / (X2 - X1)
        wf2_t1 = wf2_xt * XT_t1
        wf2_t2 = wf2_xt * XT_t2
        wf2_d1 = wf2_xt * XT_d1
        wf2_d2 = wf2_xt * XT_d2
        wf2_u1 = wf2_xt * XT_u1
        wf2_u2 = wf2_xt * XT_u2
        wf2_ms = wf2_xt * XT_ms
        wf2_re = wf2_xt * XT_re
        wf2_xf = wf2_xt * XT_xf
        !
        wf1 = 1.0 - wf2
        wf1_a1 = -wf2_a1
        wf1_x1 = -wf2_x1
        wf1_x2 = -wf2_x2
        wf1_t1 = -wf2_t1
        wf1_t2 = -wf2_t2
        wf1_d1 = -wf2_d1
        wf1_d2 = -wf2_d2
        wf1_u1 = -wf2_u1
        wf1_u2 = -wf2_u2
        wf1_ms = -wf2_ms
        wf1_re = -wf2_re
        wf1_xf = -wf2_xf
        !
        !
        !**** FIRST,  do laminar part between X1 and XT
        !
        !-----interpolate primary variables to transition point
        tt = T1 * wf1 + T2 * wf2
        tt_a1 = T1 * wf1_a1 + T2 * wf2_a1
        tt_x1 = T1 * wf1_x1 + T2 * wf2_x1
        tt_x2 = T1 * wf1_x2 + T2 * wf2_x2
        tt_t1 = T1 * wf1_t1 + T2 * wf2_t1 + wf1
        tt_t2 = T1 * wf1_t2 + T2 * wf2_t2 + wf2
        tt_d1 = T1 * wf1_d1 + T2 * wf2_d1
        tt_d2 = T1 * wf1_d2 + T2 * wf2_d2
        tt_u1 = T1 * wf1_u1 + T2 * wf2_u1
        tt_u2 = T1 * wf1_u2 + T2 * wf2_u2
        tt_ms = T1 * wf1_ms + T2 * wf2_ms
        tt_re = T1 * wf1_re + T2 * wf2_re
        tt_xf = T1 * wf1_xf + T2 * wf2_xf
        !
        dt = D1 * wf1 + D2 * wf2
        dt_a1 = D1 * wf1_a1 + D2 * wf2_a1
        dt_x1 = D1 * wf1_x1 + D2 * wf2_x1
        dt_x2 = D1 * wf1_x2 + D2 * wf2_x2
        dt_t1 = D1 * wf1_t1 + D2 * wf2_t1
        dt_t2 = D1 * wf1_t2 + D2 * wf2_t2
        dt_d1 = D1 * wf1_d1 + D2 * wf2_d1 + wf1
        dt_d2 = D1 * wf1_d2 + D2 * wf2_d2 + wf2
        dt_u1 = D1 * wf1_u1 + D2 * wf2_u1
        dt_u2 = D1 * wf1_u2 + D2 * wf2_u2
        dt_ms = D1 * wf1_ms + D2 * wf2_ms
        dt_re = D1 * wf1_re + D2 * wf2_re
        dt_xf = D1 * wf1_xf + D2 * wf2_xf
        !
        ut = U1 * wf1 + U2 * wf2
        ut_a1 = U1 * wf1_a1 + U2 * wf2_a1
        ut_x1 = U1 * wf1_x1 + U2 * wf2_x1
        ut_x2 = U1 * wf1_x2 + U2 * wf2_x2
        ut_t1 = U1 * wf1_t1 + U2 * wf2_t1
        ut_t2 = U1 * wf1_t2 + U2 * wf2_t2
        ut_d1 = U1 * wf1_d1 + U2 * wf2_d1
        ut_d2 = U1 * wf1_d2 + U2 * wf2_d2
        ut_u1 = U1 * wf1_u1 + U2 * wf2_u1 + wf1
        ut_u2 = U1 * wf1_u2 + U2 * wf2_u2 + wf2
        ut_ms = U1 * wf1_ms + U2 * wf2_ms
        ut_re = U1 * wf1_re + U2 * wf2_re
        ut_xf = U1 * wf1_xf + U2 * wf2_xf
        !
        !---- set primary "T" variables at XT  (really placed into "2" variables)
        X2 = XT
        T2 = tt
        D2 = dt
        U2 = ut
        !
        AMPl2 = AMCrit
        S2 = 0.
        !
        !---- calculate laminar secondary "T" variables
        call blkin
        call blvar(1)
        !
        !---- calculate X1-XT midpoint CFM value
        call blmid(1)
        !=
        !=    at this point, all "2" variables are really "T" variables at XT
        !=
        !
        !---- set up Newton system for dAm, dTh, dDs, dUe, dXi  at  X1 and XT
        call bldif(1)
        !
        !---- The current Newton system is in terms of "1" and "T" variables,
        !-    so calculate its equivalent in terms of "1" and "2" variables.
        !-    In other words, convert residual sensitivities wrt "T" variables
        !-    into sensitivities wrt "1" and "2" variables.  The amplification
        !-    equation is unnecessary here, so the K=1 row is left empty.
        do k = 2, 3
            blrez(k) = VSRez(k)
            blm(k) = VSM(k) + VS2(k, 2) * tt_ms + VS2(k, 3) * dt_ms + VS2(k, 4) * ut_ms + VS2(k, 5) * XT_ms
            blr(k) = VSR(k) + VS2(k, 2) * tt_re + VS2(k, 3) * dt_re + VS2(k, 4) * ut_re + VS2(k, 5) * XT_re
            blx(k) = VSX(k) + VS2(k, 2) * tt_xf + VS2(k, 3) * dt_xf + VS2(k, 4) * ut_xf + VS2(k, 5) * XT_xf
            !
            bl1(k, 1) = VS1(k, 1) + VS2(k, 2) * tt_a1 + VS2(k, 3) * dt_a1 + VS2(k, 4) * ut_a1 + VS2(k, 5) * XT_a1
            bl1(k, 2) = VS1(k, 2) + VS2(k, 2) * tt_t1 + VS2(k, 3) * dt_t1 + VS2(k, 4) * ut_t1 + VS2(k, 5) * XT_t1
            bl1(k, 3) = VS1(k, 3) + VS2(k, 2) * tt_d1 + VS2(k, 3) * dt_d1 + VS2(k, 4) * ut_d1 + VS2(k, 5) * XT_d1
            bl1(k, 4) = VS1(k, 4) + VS2(k, 2) * tt_u1 + VS2(k, 3) * dt_u1 + VS2(k, 4) * ut_u1 + VS2(k, 5) * XT_u1
            bl1(k, 5) = VS1(k, 5) + VS2(k, 2) * tt_x1 + VS2(k, 3) * dt_x1 + VS2(k, 4) * ut_x1 + VS2(k, 5) * XT_x1
            !
            bl2(k, 1) = 0.
            bl2(k, 2) = VS2(k, 2) * tt_t2 + VS2(k, 3) * dt_t2 + VS2(k, 4) * ut_t2 + VS2(k, 5) * XT_t2
            bl2(k, 3) = VS2(k, 2) * tt_d2 + VS2(k, 3) * dt_d2 + VS2(k, 4) * ut_d2 + VS2(k, 5) * XT_d2
            bl2(k, 4) = VS2(k, 2) * tt_u2 + VS2(k, 3) * dt_u2 + VS2(k, 4) * ut_u2 + VS2(k, 5) * XT_u2
            bl2(k, 5) = VS2(k, 2) * tt_x2 + VS2(k, 3) * dt_x2 + VS2(k, 4) * ut_x2 + VS2(k, 5) * XT_x2
            !
        enddo
        !
        !
        !**** SECOND, set up turbulent part between XT and X2  ****
        !
        !---- calculate equilibrium shear coefficient CQT at transition point
        call blvar(2)
        !
        !---- set initial shear coefficient value ST at transition point
        !-    ( note that CQ2, CQ2_T2, etc. are really "CQT", "CQT_TT", etc.)
        !
        ctr = CTRcon * exp(-CTRcex / (HK2 - 1.0))
        ctr_hk2 = ctr * CTRcex / (HK2 - 1.0)**2
        !
        !      CTR     = 1.1*EXP(-10.0/HK2**2)
        !      CTR_HK2 = CTR * 10.0 * 2.0/HK2**3
        !
        !CC      CTR = 1.2
        !CC      CTR = 0.7
        !CC      CTR_HK2 = 0.0
        !
        st = ctr * CQ2
        st_tt = ctr * CQ2_t2 + CQ2 * ctr_hk2 * HK2_t2
        st_dt = ctr * CQ2_d2 + CQ2 * ctr_hk2 * HK2_d2
        st_ut = ctr * CQ2_u2 + CQ2 * ctr_hk2 * HK2_u2
        st_ms = ctr * CQ2_ms + CQ2 * ctr_hk2 * HK2_ms
        st_re = ctr * CQ2_re
        !
        !---- calculate ST sensitivities wrt the actual "1" and "2" variables
        st_a1 = st_tt * tt_a1 + st_dt * dt_a1 + st_ut * ut_a1
        st_x1 = st_tt * tt_x1 + st_dt * dt_x1 + st_ut * ut_x1
        st_x2 = st_tt * tt_x2 + st_dt * dt_x2 + st_ut * ut_x2
        st_t1 = st_tt * tt_t1 + st_dt * dt_t1 + st_ut * ut_t1
        st_t2 = st_tt * tt_t2 + st_dt * dt_t2 + st_ut * ut_t2
        st_d1 = st_tt * tt_d1 + st_dt * dt_d1 + st_ut * ut_d1
        st_d2 = st_tt * tt_d2 + st_dt * dt_d2 + st_ut * ut_d2
        st_u1 = st_tt * tt_u1 + st_dt * dt_u1 + st_ut * ut_u1
        st_u2 = st_tt * tt_u2 + st_dt * dt_u2 + st_ut * ut_u2
        st_ms = st_tt * tt_ms + st_dt * dt_ms + st_ut * ut_ms + st_ms
        st_re = st_tt * tt_re + st_dt * dt_re + st_ut * ut_re + st_re
        st_xf = st_tt * tt_xf + st_dt * dt_xf + st_ut * ut_xf
        !
        AMPl2 = 0.
        S2 = st
        !
        !---- recalculate turbulent secondary "T" variables using proper CTI
        call blvar(2)
        !
        !---- set "1" variables to "T" variables and reset "2" variables
        !-    to their saved turbulent values
        do icom = 1, NCOM
            COM1(icom) = COM2(icom)
            COM2(icom) = C2Sav(icom)
        enddo
        !
        !---- calculate XT-X2 midpoint CFM value
        call blmid(2)
        !
        !---- set up Newton system for dCt, dTh, dDs, dUe, dXi  at  XT and X2
        call bldif(2)
        !
        !---- convert sensitivities wrt "T" variables into sensitivities
        !-    wrt "1" and "2" variables as done before for the laminar part
        do k = 1, 3
            btrez(k) = VSRez(k)
            btm(k) = VSM(k) &
                    + VS1(k, 1) * st_ms + VS1(k, 2) * tt_ms + VS1(k, 3) * dt_ms + VS1(k, 4) * ut_ms + VS1(k, 5) * XT_ms
            btr(k) = VSR(k) &
                    + VS1(k, 1) * st_re + VS1(k, 2) * tt_re + VS1(k, 3) * dt_re + VS1(k, 4) * ut_re + VS1(k, 5) * XT_re
            btx(k) = VSX(k) &
                    + VS1(k, 1) * st_xf + VS1(k, 2) * tt_xf + VS1(k, 3) * dt_xf + VS1(k, 4) * ut_xf + VS1(k, 5) * XT_xf
            !
            bt1(k, 1) = VS1(k, 1) * st_a1 + VS1(k, 2) * tt_a1 + VS1(k, 3) * dt_a1 + VS1(k, 4) * ut_a1 + VS1(k, 5) * XT_a1
            bt1(k, 2) = VS1(k, 1) * st_t1 + VS1(k, 2) * tt_t1 + VS1(k, 3) * dt_t1 + VS1(k, 4) * ut_t1 + VS1(k, 5) * XT_t1
            bt1(k, 3) = VS1(k, 1) * st_d1 + VS1(k, 2) * tt_d1 + VS1(k, 3) * dt_d1 + VS1(k, 4) * ut_d1 + VS1(k, 5) * XT_d1
            bt1(k, 4) = VS1(k, 1) * st_u1 + VS1(k, 2) * tt_u1 + VS1(k, 3) * dt_u1 + VS1(k, 4) * ut_u1 + VS1(k, 5) * XT_u1
            bt1(k, 5) = VS1(k, 1) * st_x1 + VS1(k, 2) * tt_x1 + VS1(k, 3) * dt_x1 + VS1(k, 4) * ut_x1 + VS1(k, 5) * XT_x1
            !
            bt2(k, 1) = VS2(k, 1)
            bt2(k, 2) = VS2(k, 2) &
                    + VS1(k, 1) * st_t2 + VS1(k, 2) * tt_t2 + VS1(k, 3) * dt_t2 + VS1(k, 4) * ut_t2 + VS1(k, 5) * XT_t2
            bt2(k, 3) = VS2(k, 3) &
                    + VS1(k, 1) * st_d2 + VS1(k, 2) * tt_d2 + VS1(k, 3) * dt_d2 + VS1(k, 4) * ut_d2 + VS1(k, 5) * XT_d2
            bt2(k, 4) = VS2(k, 4) &
                    + VS1(k, 1) * st_u2 + VS1(k, 2) * tt_u2 + VS1(k, 3) * dt_u2 + VS1(k, 4) * ut_u2 + VS1(k, 5) * XT_u2
            bt2(k, 5) = VS2(k, 5) &
                    + VS1(k, 1) * st_x2 + VS1(k, 2) * tt_x2 + VS1(k, 3) * dt_x2 + VS1(k, 4) * ut_x2 + VS1(k, 5) * XT_x2
            !
        enddo
        !
        !---- Add up laminar and turbulent parts to get final system
        !-    in terms of honest-to-God "1" and "2" variables.
        VSRez(1) = btrez(1)
        VSRez(2) = blrez(2) + btrez(2)
        VSRez(3) = blrez(3) + btrez(3)
        VSM(1) = btm(1)
        VSM(2) = blm(2) + btm(2)
        VSM(3) = blm(3) + btm(3)
        VSR(1) = btr(1)
        VSR(2) = blr(2) + btr(2)
        VSR(3) = blr(3) + btr(3)
        VSX(1) = btx(1)
        VSX(2) = blx(2) + btx(2)
        VSX(3) = blx(3) + btx(3)
        do l = 1, 5
            VS1(1, l) = bt1(1, l)
            VS2(1, l) = bt2(1, l)
            VS1(2, l) = bl1(2, l) + bt1(2, l)
            VS2(2, l) = bl2(2, l) + bt2(2, l)
            VS1(3, l) = bl1(3, l) + bt1(3, l)
            VS2(3, l) = bl2(3, l) + bt2(3, l)
        enddo
        !
        !---- To be sanitary, restore "1" quantities which got clobbered
        !-    in all of the numerical gymnastics above.  The "2" variables
        !-    were already restored for the XT-X2 differencing part.
        do icom = 1, NCOM
            COM1(icom) = C1Sav(icom)
        enddo
        !
    end subroutine trdif
    !*==BLDIF.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine bldif(Ityp)
        !-----------------------------------------------------------
        !     Sets up the Newton system coefficients and residuals
        !
        !        ITYP = 0 :  similarity station
        !        ITYP = 1 :  laminar interval
        !        ITYP = 2 :  turbulent interval
        !        ITYP = 3 :  wake interval
        !
        !     This routine knows nothing about a transition interval,
        !     which is taken care of by TRDIF.
        !-----------------------------------------------------------
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ityp
        intent (in) Ityp
        !
        ! Local variables
        !
        real :: ald, arg, ax, ax_a1, ax_a2, ax_hk1, ax_hk2, ax_rt1, ax_rt2, ax_t1, ax_t2, btmp, cfa, cfx, &
                & cfx_cf1, cfx_cf2, cfx_cfm, cfx_t1, cfx_t2, cfx_ta, cfx_upw, cfx_x1, cfx_x2, cfx_xa, cqa, da, &
                & ddlog, dea, dix, dix_upw, dxi, ehh, gcc, ha, hca, hdcon, hd_hk1, hd_hk2, hka, hkc, hkc_hka, &
                & hkc_rta, hl, hlog, hlsq, hl_hk1, hl_hk2, hr, hr_hka, hr_rta, hsa, hupwt, hwa, ma, rezc, rezh, &
                & rezt, rta, sa, scc, scc_us1, scc_us2, scc_usa, slog, ta, tlog, ulog, upw, upw_d1, upw_d2, &
                & upw_hd, upw_hk1, upw_hk2, upw_hl, upw_ms, upw_t1, upw_t2, upw_u1, upw_u2, uq
        integer :: k, l
        real :: uq_cfa, uq_d1, uq_d2, uq_da, uq_hka, uq_ms, uq_re, uq_rta, uq_t1, uq_t2, uq_u1, uq_u2, uq_upw, &
                & usa, xa, xlog, xot1, xot2, z_ax, z_cf1, z_cf2, z_cfa, z_cfm, z_cfx, z_cq1, z_cq2, z_cqa, z_d1, &
                & z_d2, z_da, z_de1, z_de2, z_dea, z_di1, z_di2, z_dix, z_dxi, z_ha, z_hca, z_hk1, z_hk2, z_hka, &
                & z_hl, z_hs1, z_hs2, z_hwa, z_ma, z_s1, z_s2, z_sa, z_sl, z_t1, z_t2, z_tl, z_u1, z_u2, z_ul, &
                & z_upw, z_us1, z_us2, z_usa, z_x1, z_x2, z_xl
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
        if (Ityp==0) then
            !----- similarity logarithmic differences  (prescribed)
            xlog = 1.0
            ulog = BULe
            tlog = 0.5 * (1.0 - BULe)
            hlog = 0.
            ddlog = 0.
        else
            !----- usual logarithmic differences
            xlog = log(X2 / X1)
            ulog = log(U2 / U1)
            tlog = log(T2 / T1)
            hlog = log(HS2 / HS1)
            !       XLOG = 2.0*(X2-X1)/(X2+X1)
            !       ULOG = 2.0*(U2-U1)/(U2+U1)
            !       TLOG = 2.0*(T2-T1)/(T2+T1)
            !       HLOG = 2.0*(HS2-HS1)/(HS2+HS1)
            ddlog = 1.0
        endif
        !
        do k = 1, 4
            VSRez(k) = 0.
            VSM(k) = 0.
            VSR(k) = 0.
            VSX(k) = 0.
            do l = 1, 5
                VS1(k, l) = 0.
                VS2(k, l) = 0.
            enddo
        enddo
        !
        !---- set triggering constant for local upwinding
        hupwt = 1.0
        !
        !cc      HDCON = 5.0*HUPWT
        !cc      HD_HK1 = 0.0
        !cc      HD_HK2 = 0.0
        !
        hdcon = 5.0 * hupwt / HK2**2
        hd_hk1 = 0.0
        hd_hk2 = -hdcon * 2.0 / HK2
        !
        !---- use less upwinding in the wake
        if (Ityp==3) then
            hdcon = hupwt / HK2**2
            hd_hk1 = 0.0
            hd_hk2 = -hdcon * 2.0 / HK2
        endif
        !
        !---- local upwinding is based on local change in  log(Hk-1)
        !-    (mainly kicks in at transition)
        arg = abs((HK2 - 1.0) / (HK1 - 1.0))
        hl = log(arg)
        hl_hk1 = -1.0 / (HK1 - 1.0)
        hl_hk2 = 1.0 / (HK2 - 1.0)
        !
        !---- set local upwinding parameter UPW and linearize it
        !
        !       UPW = 0.5   Trapezoidal
        !       UPW = 1.0   Backward Euler
        !
        hlsq = min(hl**2, 15.0)
        ehh = exp(-hlsq * hdcon)
        upw = 1.0 - 0.5 * ehh
        upw_hl = ehh * hl * hdcon
        upw_hd = 0.5 * ehh * hlsq
        !
        upw_hk1 = upw_hl * hl_hk1 + upw_hd * hd_hk1
        upw_hk2 = upw_hl * hl_hk2 + upw_hd * hd_hk2
        !
        upw_u1 = upw_hk1 * HK1_u1
        upw_t1 = upw_hk1 * HK1_t1
        upw_d1 = upw_hk1 * HK1_d1
        upw_u2 = upw_hk2 * HK2_u2
        upw_t2 = upw_hk2 * HK2_t2
        upw_d2 = upw_hk2 * HK2_d2
        upw_ms = upw_hk1 * HK1_ms + upw_hk2 * HK2_ms
        !
        !
        if (Ityp==0) then
            !
            !***** LE point -->  set zero amplification factor
            VS2(1, 1) = 1.0
            VSR(1) = 0.
            VSRez(1) = -AMPl2
            !
        elseif (Ityp==1) then
            !
            !***** laminar part -->  set amplification equation
            !
            !----- set average amplification AX over interval X1..X2
            call axset(HK1, T1, RT1, AMPl1, HK2, T2, RT2, AMPl2, AMCrit, &
                    IDAmpv, ax, ax_hk1, ax_t1, ax_rt1, ax_a1, ax_hk2, ax_t2, ax_rt2, ax_a2)
            !
            rezc = AMPl2 - AMPl1 - ax * (X2 - X1)
            z_ax = -(X2 - X1)
            !
            VS1(1, 1) = z_ax * ax_a1 - 1.0
            VS1(1, 2) = z_ax * (ax_hk1 * HK1_t1 + ax_t1 + ax_rt1 * RT1_t1)
            VS1(1, 3) = z_ax * (ax_hk1 * HK1_d1)
            VS1(1, 4) = z_ax * (ax_hk1 * HK1_u1 + ax_rt1 * RT1_u1)
            VS1(1, 5) = ax
            VS2(1, 1) = z_ax * ax_a2 + 1.0
            VS2(1, 2) = z_ax * (ax_hk2 * HK2_t2 + ax_t2 + ax_rt2 * RT2_t2)
            VS2(1, 3) = z_ax * (ax_hk2 * HK2_d2)
            VS2(1, 4) = z_ax * (ax_hk2 * HK2_u2 + ax_rt2 * RT2_u2)
            VS2(1, 5) = -ax
            VSM(1) = z_ax * (ax_hk1 * HK1_ms + ax_rt1 * RT1_ms + ax_hk2 * HK2_ms + ax_rt2 * RT2_ms)
            VSR(1) = z_ax * (ax_rt1 * RT1_re + ax_rt2 * RT2_re)
            VSX(1) = 0.
            VSRez(1) = -rezc
            !
        else
            !
            !***** turbulent part -->  set shear lag equation
            !
            sa = (1.0 - upw) * S1 + upw * S2
            cqa = (1.0 - upw) * CQ1 + upw * CQ2
            cfa = (1.0 - upw) * CF1 + upw * CF2
            hka = (1.0 - upw) * HK1 + upw * HK2
            !
            usa = 0.5 * (US1 + US2)
            rta = 0.5 * (RT1 + RT2)
            dea = 0.5 * (DE1 + DE2)
            da = 0.5 * (D1 + D2)
            !
            !
            if (Ityp==3) then
                !------ increased dissipation length in wake (decrease its reciprocal)
                ald = DLCon
            else
                ald = 1.0
            endif
            !
            !----- set and linearize  equilibrium 1/Ue dUe/dx   ...  NEW  12 Oct 94
            if (Ityp==2) then
                gcc = GCCon
                hkc = hka - 1.0 - gcc / rta
                hkc_hka = 1.0
                hkc_rta = gcc / rta**2
                if (hkc<0.01) then
                    hkc = 0.01
                    hkc_hka = 0.0
                    hkc_rta = 0.0
                endif
            else
                gcc = 0.0
                hkc = hka - 1.0
                hkc_hka = 1.0
                hkc_rta = 0.0
            endif
            !
            hr = hkc / (GACon * ald * hka)
            hr_hka = hkc_hka / (GACon * ald * hka) - hr / hka
            hr_rta = hkc_rta / (GACon * ald * hka)
            !
            uq = (0.5 * cfa - hr**2) / (GBCon * da)
            uq_hka = -2.0 * hr * hr_hka / (GBCon * da)
            uq_rta = -2.0 * hr * hr_rta / (GBCon * da)
            uq_cfa = 0.5 / (GBCon * da)
            uq_da = -uq / da
            uq_upw = uq_cfa * (CF2 - CF1) + uq_hka * (HK2 - HK1)
            !
            uq_t1 = (1.0 - upw) * (uq_cfa * CF1_t1 + uq_hka * HK1_t1) + uq_upw * upw_t1
            uq_d1 = (1.0 - upw) * (uq_cfa * CF1_d1 + uq_hka * HK1_d1) + uq_upw * upw_d1
            uq_u1 = (1.0 - upw) * (uq_cfa * CF1_u1 + uq_hka * HK1_u1) + uq_upw * upw_u1
            uq_t2 = upw * (uq_cfa * CF2_t2 + uq_hka * HK2_t2) + uq_upw * upw_t2
            uq_d2 = upw * (uq_cfa * CF2_d2 + uq_hka * HK2_d2) + uq_upw * upw_d2
            uq_u2 = upw * (uq_cfa * CF2_u2 + uq_hka * HK2_u2) + uq_upw * upw_u2
            uq_ms = (1.0 - upw) * (uq_cfa * CF1_ms + uq_hka * HK1_ms) &
                    + uq_upw * upw_ms + upw * (uq_cfa * CF2_ms + uq_hka * HK2_ms)
            uq_re = (1.0 - upw) * uq_cfa * CF1_re + upw * uq_cfa * CF2_re
            !
            uq_t1 = uq_t1 + 0.5 * uq_rta * RT1_t1
            uq_d1 = uq_d1 + 0.5 * uq_da
            uq_u1 = uq_u1 + 0.5 * uq_rta * RT1_u1
            uq_t2 = uq_t2 + 0.5 * uq_rta * RT2_t2
            uq_d2 = uq_d2 + 0.5 * uq_da
            uq_u2 = uq_u2 + 0.5 * uq_rta * RT2_u2
            uq_ms = uq_ms + 0.5 * uq_rta * RT1_ms + 0.5 * uq_rta * RT2_ms
            uq_re = uq_re + 0.5 * uq_rta * RT1_re + 0.5 * uq_rta * RT2_re
            !
            scc = SCCon * 1.333 / (1.0 + usa)
            scc_usa = -scc / (1.0 + usa)
            !
            scc_us1 = scc_usa * 0.5
            scc_us2 = scc_usa * 0.5
            !
            !
            slog = log(S2 / S1)
            dxi = X2 - X1
            !
            rezc = scc * (cqa - sa * ald) * dxi - dea * 2.0 * slog + dea * 2.0 * (uq * dxi - ulog) * DUXcon
            !

            !        if(  ! (rt2.gt.1.0e3 .and. rt1.le.1.0e3) .or.
            !     &     (rt2.gt.1.0e4 .and. rt1.le.1.0e4) .or.
            !     &     (rt2.gt.1.0e5 .and. rt1.le.1.0e5)        ) then
            !           gga = (HKA-1.0-GCC/RTA)/HKA / sqrt(0.5*CFA)
            !           write(*,4455) rta, hka, gga, cfa, cqa, sa, uq, ulog/dxi
            ! 4455      format(1x,f7.0, 2f9.4,f10.6,2f8.5,2f10.5)
            !        endif

            z_cfa = dea * 2.0 * uq_cfa * dxi * DUXcon
            z_hka = dea * 2.0 * uq_hka * dxi * DUXcon
            z_da = dea * 2.0 * uq_da * dxi * DUXcon
            z_sl = -dea * 2.0
            z_ul = -dea * 2.0 * DUXcon
            z_dxi = scc * (cqa - sa * ald) + dea * 2.0 * uq * DUXcon
            z_usa = scc_usa * (cqa - sa * ald) * dxi
            z_cqa = scc * dxi
            z_sa = -scc * dxi * ald
            z_dea = 2.0 * ((uq * dxi - ulog) * DUXcon - slog)
            !
            z_upw = z_cqa * (CQ2 - CQ1) + z_sa * (S2 - S1) + z_cfa * (CF2 - CF1) + z_hka * (HK2 - HK1)
            z_de1 = 0.5 * z_dea
            z_de2 = 0.5 * z_dea
            z_us1 = 0.5 * z_usa
            z_us2 = 0.5 * z_usa
            z_d1 = 0.5 * z_da
            z_d2 = 0.5 * z_da
            z_u1 = -z_ul / U1
            z_u2 = z_ul / U2
            z_x1 = -z_dxi
            z_x2 = z_dxi
            z_s1 = (1.0 - upw) * z_sa - z_sl / S1
            z_s2 = upw * z_sa + z_sl / S2
            z_cq1 = (1.0 - upw) * z_cqa
            z_cq2 = upw * z_cqa
            z_cf1 = (1.0 - upw) * z_cfa
            z_cf2 = upw * z_cfa
            z_hk1 = (1.0 - upw) * z_hka
            z_hk2 = upw * z_hka
            !
            VS1(1, 1) = z_s1
            VS1(1, 2) = z_upw * upw_t1 + z_de1 * DE1_t1 + z_us1 * US1_t1
            VS1(1, 3) = z_d1 + z_upw * upw_d1 + z_de1 * DE1_d1 + z_us1 * US1_d1
            VS1(1, 4) = z_u1 + z_upw * upw_u1 + z_de1 * DE1_u1 + z_us1 * US1_u1
            VS1(1, 5) = z_x1
            VS2(1, 1) = z_s2
            VS2(1, 2) = z_upw * upw_t2 + z_de2 * DE2_t2 + z_us2 * US2_t2
            VS2(1, 3) = z_d2 + z_upw * upw_d2 + z_de2 * DE2_d2 + z_us2 * US2_d2
            VS2(1, 4) = z_u2 + z_upw * upw_u2 + z_de2 * DE2_u2 + z_us2 * US2_u2
            VS2(1, 5) = z_x2
            VSM(1) = z_upw * upw_ms + z_de1 * DE1_ms + z_us1 * US1_ms + z_de2 * DE2_ms + z_us2 * US2_ms
            !
            VS1(1, 2) = VS1(1, 2) + z_cq1 * CQ1_t1 + z_cf1 * CF1_t1 + z_hk1 * HK1_t1
            VS1(1, 3) = VS1(1, 3) + z_cq1 * CQ1_d1 + z_cf1 * CF1_d1 + z_hk1 * HK1_d1
            VS1(1, 4) = VS1(1, 4) + z_cq1 * CQ1_u1 + z_cf1 * CF1_u1 + z_hk1 * HK1_u1
            !
            VS2(1, 2) = VS2(1, 2) + z_cq2 * CQ2_t2 + z_cf2 * CF2_t2 + z_hk2 * HK2_t2
            VS2(1, 3) = VS2(1, 3) + z_cq2 * CQ2_d2 + z_cf2 * CF2_d2 + z_hk2 * HK2_d2
            VS2(1, 4) = VS2(1, 4) + z_cq2 * CQ2_u2 + z_cf2 * CF2_u2 + z_hk2 * HK2_u2
            !
            VSM(1) = VSM(1) + z_cq1 * CQ1_ms + z_cf1 * CF1_ms &
                    + z_hk1 * HK1_ms + z_cq2 * CQ2_ms + z_cf2 * CF2_ms + z_hk2 * HK2_ms
            VSR(1) = z_cq1 * CQ1_re + z_cf1 * CF1_re + z_cq2 * CQ2_re + z_cf2 * CF2_re
            VSX(1) = 0.
            VSRez(1) = -rezc
            !
        endif
        !
        !**** Set up momentum equation
        ha = 0.5 * (H1 + H2)
        ma = 0.5 * (M1 + M2)
        xa = 0.5 * (X1 + X2)
        ta = 0.5 * (T1 + T2)
        hwa = 0.5 * (DW1 / T1 + DW2 / T2)
        !
        !---- set Cf term, using central value CFM for better accuracy in drag
        cfx = 0.50 * CFM * xa / ta + 0.25 * (CF1 * X1 / T1 + CF2 * X2 / T2)
        cfx_xa = 0.50 * CFM / ta
        cfx_ta = -.50 * CFM * xa / ta**2
        !
        cfx_x1 = 0.25 * CF1 / T1 + cfx_xa * 0.5
        cfx_x2 = 0.25 * CF2 / T2 + cfx_xa * 0.5
        cfx_t1 = -.25 * CF1 * X1 / T1**2 + cfx_ta * 0.5
        cfx_t2 = -.25 * CF2 * X2 / T2**2 + cfx_ta * 0.5
        cfx_cf1 = 0.25 * X1 / T1
        cfx_cf2 = 0.25 * X2 / T2
        cfx_cfm = 0.50 * xa / ta
        !
        btmp = ha + 2.0 - ma + hwa
        !
        rezt = tlog + btmp * ulog - xlog * 0.5 * cfx
        z_cfx = -xlog * 0.5
        z_ha = ulog
        z_hwa = ulog
        z_ma = -ulog
        z_xl = -ddlog * 0.5 * cfx
        z_ul = ddlog * btmp
        z_tl = ddlog
        !
        z_cfm = z_cfx * cfx_cfm
        z_cf1 = z_cfx * cfx_cf1
        z_cf2 = z_cfx * cfx_cf2
        !
        z_t1 = -z_tl / T1 + z_cfx * cfx_t1 + z_hwa * 0.5 * (-DW1 / T1**2)
        z_t2 = z_tl / T2 + z_cfx * cfx_t2 + z_hwa * 0.5 * (-DW2 / T2**2)
        z_x1 = -z_xl / X1 + z_cfx * cfx_x1
        z_x2 = z_xl / X2 + z_cfx * cfx_x2
        z_u1 = -z_ul / U1
        z_u2 = z_ul / U2
        !
        VS1(2, 2) = 0.5 * z_ha * H1_t1 + z_cfm * CFM_t1 + z_cf1 * CF1_t1 + z_t1
        VS1(2, 3) = 0.5 * z_ha * H1_d1 + z_cfm * CFM_d1 + z_cf1 * CF1_d1
        VS1(2, 4) = 0.5 * z_ma * M1_u1 + z_cfm * CFM_u1 + z_cf1 * CF1_u1 + z_u1
        VS1(2, 5) = z_x1
        VS2(2, 2) = 0.5 * z_ha * H2_t2 + z_cfm * CFM_t2 + z_cf2 * CF2_t2 + z_t2
        VS2(2, 3) = 0.5 * z_ha * H2_d2 + z_cfm * CFM_d2 + z_cf2 * CF2_d2
        VS2(2, 4) = 0.5 * z_ma * M2_u2 + z_cfm * CFM_u2 + z_cf2 * CF2_u2 + z_u2
        VS2(2, 5) = z_x2
        !
        VSM(2) = 0.5 * z_ma * M1_ms + z_cfm * CFM_ms + z_cf1 * CF1_ms + 0.5 * z_ma * M2_ms + z_cf2 * CF2_ms
        VSR(2) = z_cfm * CFM_re + z_cf1 * CF1_re + z_cf2 * CF2_re
        VSX(2) = 0.
        VSRez(2) = -rezt
        !
        !**** Set up shape parameter equation
        !
        xot1 = X1 / T1
        xot2 = X2 / T2
        !
        ha = 0.5 * (H1 + H2)
        hsa = 0.5 * (HS1 + HS2)
        hca = 0.5 * (HC1 + HC2)
        hwa = 0.5 * (DW1 / T1 + DW2 / T2)
        !
        dix = (1.0 - upw) * DI1 * xot1 + upw * DI2 * xot2
        cfx = (1.0 - upw) * CF1 * xot1 + upw * CF2 * xot2
        dix_upw = DI2 * xot2 - DI1 * xot1
        cfx_upw = CF2 * xot2 - CF1 * xot1
        !
        btmp = 2.0 * hca / hsa + 1.0 - ha - hwa
        !
        rezh = hlog + btmp * ulog + xlog * (0.5 * cfx - dix)
        z_cfx = xlog * 0.5
        z_dix = -xlog
        z_hca = 2.0 * ulog / hsa
        z_ha = -ulog
        z_hwa = -ulog
        z_xl = ddlog * (0.5 * cfx - dix)
        z_ul = ddlog * btmp
        z_hl = ddlog
        !
        z_upw = z_cfx * cfx_upw + z_dix * dix_upw
        !
        z_hs1 = -hca * ulog / hsa**2 - z_hl / HS1
        z_hs2 = -hca * ulog / hsa**2 + z_hl / HS2
        !
        z_cf1 = (1.0 - upw) * z_cfx * xot1
        z_cf2 = upw * z_cfx * xot2
        z_di1 = (1.0 - upw) * z_dix * xot1
        z_di2 = upw * z_dix * xot2
        !
        z_t1 = (1.0 - upw) * (z_cfx * CF1 + z_dix * DI1) * (-xot1 / T1)
        z_t2 = upw * (z_cfx * CF2 + z_dix * DI2) * (-xot2 / T2)
        z_x1 = (1.0 - upw) * (z_cfx * CF1 + z_dix * DI1) / T1 - z_xl / X1
        z_x2 = upw * (z_cfx * CF2 + z_dix * DI2) / T2 + z_xl / X2
        z_u1 = -z_ul / U1
        z_u2 = z_ul / U2
        !
        z_t1 = z_t1 + z_hwa * 0.5 * (-DW1 / T1**2)
        z_t2 = z_t2 + z_hwa * 0.5 * (-DW2 / T2**2)
        !
        VS1(3, 1) = z_di1 * DI1_s1
        VS1(3, 2) = z_hs1 * HS1_t1 + z_cf1 * CF1_t1 + z_di1 * DI1_t1 + z_t1
        VS1(3, 3) = z_hs1 * HS1_d1 + z_cf1 * CF1_d1 + z_di1 * DI1_d1
        VS1(3, 4) = z_hs1 * HS1_u1 + z_cf1 * CF1_u1 + z_di1 * DI1_u1 + z_u1
        VS1(3, 5) = z_x1
        VS2(3, 1) = z_di2 * DI2_s2
        VS2(3, 2) = z_hs2 * HS2_t2 + z_cf2 * CF2_t2 + z_di2 * DI2_t2 + z_t2
        VS2(3, 3) = z_hs2 * HS2_d2 + z_cf2 * CF2_d2 + z_di2 * DI2_d2
        VS2(3, 4) = z_hs2 * HS2_u2 + z_cf2 * CF2_u2 + z_di2 * DI2_u2 + z_u2
        VS2(3, 5) = z_x2
        VSM(3) = z_hs1 * HS1_ms + z_cf1 * CF1_ms + z_di1 * DI1_ms + z_hs2 * HS2_ms + z_cf2 * CF2_ms + z_di2 * DI2_ms
        VSR(3) = z_hs1 * HS1_re + z_cf1 * CF1_re + z_di1 * DI1_re + z_hs2 * HS2_re + z_cf2 * CF2_re + z_di2 * DI2_re
        !
        VS1(3, 2) = VS1(3, 2) + 0.5 * (z_hca * HC1_t1 + z_ha * H1_t1) + z_upw * upw_t1
        VS1(3, 3) = VS1(3, 3) + 0.5 * (z_hca * HC1_d1 + z_ha * H1_d1) + z_upw * upw_d1
        VS1(3, 4) = VS1(3, 4) + 0.5 * (z_hca * HC1_u1) + z_upw * upw_u1
        VS2(3, 2) = VS2(3, 2) + 0.5 * (z_hca * HC2_t2 + z_ha * H2_t2) + z_upw * upw_t2
        VS2(3, 3) = VS2(3, 3) + 0.5 * (z_hca * HC2_d2 + z_ha * H2_d2) + z_upw * upw_d2
        VS2(3, 4) = VS2(3, 4) + 0.5 * (z_hca * HC2_u2) + z_upw * upw_u2
        !
        VSM(3) = VSM(3) + 0.5 * (z_hca * HC1_ms) + z_upw * upw_ms + 0.5 * (z_hca * HC2_ms)
        !
        VSX(3) = 0.
        VSRez(3) = -rezh
        !
    end subroutine bldif
    !*==DAMPL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine dampl(Hk, Th, Rt, Ax, Ax_hk, Ax_th, Ax_rt)
        !==============================================================
        !     Amplification rate routine for envelope e^n method.
        !     Reference:
        !                Drela, M., Giles, M.,
        !               "Viscous/Inviscid Analysis of Transonic and
        !                Low Reynolds Number Airfoils",
        !                AIAA Journal, Oct. 1987.
        !
        !     NEW VERSION.   March 1991       (latest bug fix  July 93)
        !          - m(H) correlation made more accurate up to H=20
        !          - for H > 5, non-similar profiles are used
        !            instead of Falkner-Skan profiles.  These
        !            non-similar profiles have smaller reverse
        !            velocities, are more representative of typical
        !            separation bubble profiles.
        !--------------------------------------------------------------
        !
        !     input :   HK     kinematic shape parameter
        !               TH     momentum thickness
        !               RT     momentum-thickness Reynolds number
        !
        !     output:   AX     envelope spatial amplification rate
        !               AX_(.) sensitivity of AX to parameter (.)
        !
        !
        !     Usage: The log of the envelope amplitude N(x) is
        !            calculated by integrating AX (= dN/dx) with
        !            respect to the streamwise distance x.
        !                      x
        !                     /
        !              N(x) = | AX(H(x),Th(x),Rth(x)) dx
        !                     /
        !                      0
        !            The integration can be started from the leading
        !            edge since AX will be returned as zero when RT
        !            is below the critical Rtheta.  Transition occurs
        !            when N(x) reaches Ncrit (Ncrit= 9 is "standard").
        !==============================================================
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Ax, Ax_hk, Ax_rt, Ax_th, Hk, Rt, Th
        intent (in) Hk, Rt, Th
        intent (out) Ax_hk, Ax_rt, Ax_th
        intent (inout) Ax
        !
        ! Local variables
        !
        real :: aa, aa_hk, af, af_hk, af_hmi, arg, arg_hk, bb, bb_hk, dadr, dadr_hk, ex, ex_hk, gr, grcrit, &
                & grc_hk, gr_rt, hmi, hmi_hk, rfac, rfac_hk, rfac_rn, rfac_rt, rnorm, rn_hk, rn_rt
        real, save :: dgr
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
        !cc   DATA DGR / 0.04 /
        data dgr/0.08/
        !
        hmi = 1.0 / (Hk - 1.0)
        hmi_hk = -hmi**2
        !
        !---- log10(Critical Rth) - H   correlation for Falkner-Skan profiles
        aa = 2.492 * hmi**0.43
        aa_hk = (aa / hmi) * 0.43 * hmi_hk
        !
        bb = tanh(14.0 * hmi - 9.24)
        bb_hk = (1.0 - bb * bb) * 14.0 * hmi_hk
        !
        grcrit = aa + 0.7 * (bb + 1.0)
        grc_hk = aa_hk + 0.7 * bb_hk
        !
        !
        gr = log10(Rt)
        gr_rt = 1.0 / (2.3025851 * Rt)
        !
        if (gr<grcrit - dgr) then
            !
            !----- no amplification for Rtheta < Rcrit
            Ax = 0.
            Ax_hk = 0.
            Ax_th = 0.
            Ax_rt = 0.
            !
        else
            !
            !----- Set steep cubic ramp used to turn on AX smoothly as Rtheta
            !-     exceeds Rcrit (previously, this was done discontinuously).
            !-     The ramp goes between  -DGR < log10(Rtheta/Rcrit) < DGR
            !
            rnorm = (gr - (grcrit - dgr)) / (2.0 * dgr)
            rn_hk = -grc_hk / (2.0 * dgr)
            rn_rt = gr_rt / (2.0 * dgr)
            !
            if (rnorm>=1.0) then
                rfac = 1.0
                rfac_hk = 0.
                rfac_rt = 0.
            else
                rfac = 3.0 * rnorm**2 - 2.0 * rnorm**3
                rfac_rn = 6.0 * rnorm - 6.0 * rnorm**2
                !
                rfac_hk = rfac_rn * rn_hk
                rfac_rt = rfac_rn * rn_rt
            endif
            !
            !----- Amplification envelope slope correlation for Falkner-Skan
            arg = 3.87 * hmi - 2.52
            arg_hk = 3.87 * hmi_hk
            !
            ex = exp(-arg**2)
            ex_hk = ex * (-2.0 * arg * arg_hk)
            !
            dadr = 0.028 * (Hk - 1.0) - 0.0345 * ex
            dadr_hk = 0.028 - 0.0345 * ex_hk
            !
            !----- new m(H) correlation    1 March 91
            af = -0.05 + 2.7 * hmi - 5.5 * hmi**2 + 3.0 * hmi**3
            af_hmi = 2.7 - 11.0 * hmi + 9.0 * hmi**2
            af_hk = af_hmi * hmi_hk
            !
            Ax = (af * dadr / Th) * rfac
            Ax_hk = (af_hk * dadr / Th + af * dadr_hk / Th) * rfac + (af * dadr / Th) * rfac_hk
            Ax_th = -Ax / Th
            Ax_rt = (af * dadr / Th) * rfac_rt
            !
        endif
        !
    end subroutine dampl
    !*==DAMPL2.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! DAMPL



    subroutine dampl2(Hk, Th, Rt, Ax, Ax_hk, Ax_th, Ax_rt)
        !==============================================================
        !     Amplification rate routine for modified envelope e^n method.
        !     Reference:
        !                Drela, M., Giles, M.,
        !               "Viscous/Inviscid Analysis of Transonic and
        !                Low Reynolds Number Airfoils",
        !                AIAA Journal, Oct. 1987.
        !
        !     NEWER VERSION.   Nov 1996
        !          - Amplification rate changes to the Orr-Sommerfeld
        !              maximum ai(H,Rt) function for H > 4 .
        !          - This implicitly assumes that the frequency range
        !              (around w = 0.09 Ue/theta) which experiences this
        !              maximum amplification rate contains the currently
        !              most-amplified frequency.
        !--------------------------------------------------------------
        !
        !     input :   HK     kinematic shape parameter
        !               TH     momentum thickness
        !               RT     momentum-thickness Reynolds number
        !
        !     output:   AX     envelope spatial amplification rate
        !               AX_(.) sensitivity of AX to parameter (.)
        !
        !
        !     Usage: The log of the envelope amplitude N(x) is
        !            calculated by integrating AX (= dN/dx) with
        !            respect to the streamwise distance x.
        !                      x
        !                     /
        !              N(x) = | AX(H(x),Th(x),Rth(x)) dx
        !                     /
        !                      0
        !            The integration can be started from the leading
        !            edge since AX will be returned as zero when RT
        !            is below the critical Rtheta.  Transition occurs
        !            when N(x) reaches Ncrit (Ncrit= 9 is "standard").
        !==============================================================
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Ax, Ax_hk, Ax_rt, Ax_th, Hk, Rt, Th
        intent (in) Hk, Rt, Th
        intent (inout) Ax, Ax_hk, Ax_rt, Ax_th
        !
        ! Local variables
        !
        real :: aa, aa_hk, af, af_hk, af_hmi, arg, arg_hk, ax1, ax1_hk, ax1_rt, ax1_th, ax2, ax2_hk, ax2_rt, &
                & ax2_th, bb, bb_hk, brg, dadr, dadr_hk, ex, ex_hk, gr, gr0, gr0_hk, grc, grc_hk, gr_rt, hfac, &
                & hf_hk, hmi, hmi_hk, hnorm, hn_hk, rfac, rfac_hk, rfac_rn, rfac_rt, rnorm, rn_hk, rn_rt, tnr, &
                & tnr_hk, tnr_rt
        real, save :: dgr, hk1, hk2
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
        data dgr/0.08/
        data hk1, hk2/3.5, 4.0/
        !
        hmi = 1.0 / (Hk - 1.0)
        hmi_hk = -hmi**2
        !
        !---- log10(Critical Rth) -- H   correlation for Falkner-Skan profiles
        aa = 2.492 * hmi**0.43
        aa_hk = (aa / hmi) * 0.43 * hmi_hk
        !
        bb = tanh(14.0 * hmi - 9.24)
        bb_hk = (1.0 - bb * bb) * 14.0 * hmi_hk
        !
        grc = aa + 0.7 * (bb + 1.0)
        grc_hk = aa_hk + 0.7 * bb_hk
        !
        !
        gr = log10(Rt)
        gr_rt = 1.0 / (2.3025851 * Rt)
        !
        if (gr<grc - dgr) then
            !
            !----- no amplification for Rtheta < Rcrit
            Ax = 0.
            Ax_hk = 0.
            Ax_th = 0.
            Ax_rt = 0.
            !
        else
            !
            !----- Set steep cubic ramp used to turn on AX smoothly as Rtheta
            !-     exceeds Rcrit (previously, this was done discontinuously).
            !-     The ramp goes between  -DGR < log10(Rtheta/Rcrit) < DGR
            !
            rnorm = (gr - (grc - dgr)) / (2.0 * dgr)
            rn_hk = -grc_hk / (2.0 * dgr)
            rn_rt = gr_rt / (2.0 * dgr)
            !
            if (rnorm>=1.0) then
                rfac = 1.0
                rfac_hk = 0.
                rfac_rt = 0.
            else
                rfac = 3.0 * rnorm**2 - 2.0 * rnorm**3
                rfac_rn = 6.0 * rnorm - 6.0 * rnorm**2
                !
                rfac_hk = rfac_rn * rn_hk
                rfac_rt = rfac_rn * rn_rt
            endif
            !
            !
            !----- set envelope amplification rate with respect to Rtheta
            !-       DADR = d(N)/d(Rtheta) = f(H)
            !
            arg = 3.87 * hmi - 2.52
            arg_hk = 3.87 * hmi_hk
            !
            ex = exp(-arg**2)
            ex_hk = ex * (-2.0 * arg * arg_hk)
            !
            dadr = 0.028 * (Hk - 1.0) - 0.0345 * ex
            dadr_hk = 0.028 - 0.0345 * ex_hk
            !
            !
            !----- set conversion factor from d/d(Rtheta) to d/dx
            !-       AF = Theta d(Rtheta)/dx = f(H)
            !
            brg = -20.0 * hmi
            af = -0.05 + 2.7 * hmi - 5.5 * hmi**2 + 3.0 * hmi**3 + 0.1 * exp(brg)
            af_hmi = 2.7 - 11.0 * hmi + 9.0 * hmi**2 - 2.0 * exp(brg)
            af_hk = af_hmi * hmi_hk
            !
            !
            !----- set amplification rate with respect to x,
            !-     with RFAC shutting off amplification when below Rcrit
            !
            Ax = (af * dadr / Th) * rfac
            Ax_hk = (af_hk * dadr / Th + af * dadr_hk / Th) * rfac + (af * dadr / Th) * rfac_hk
            Ax_th = -Ax / Th
            Ax_rt = (af * dadr / Th) * rfac_rt
            !
        endif
        !
        if (Hk<hk1) return
        !
        !---- non-envelope max-amplification correction for separated profiles
        !
        hnorm = (Hk - hk1) / (hk2 - hk1)
        hn_hk = 1.0 / (hk2 - hk1)
        !
        !---- set blending fraction HFAC = 0..1 over HK1 < HK < HK2
        if (hnorm>=1.0) then
            hfac = 1.0
            hf_hk = 0.
        else
            hfac = 3.0 * hnorm**2 - 2.0 * hnorm**3
            hf_hk = (6.0 * hnorm - 6.0 * hnorm**2) * hn_hk
        endif
        !
        !---- "normal" envelope amplification rate AX1
        ax1 = Ax
        ax1_hk = Ax_hk
        ax1_th = Ax_th
        ax1_rt = Ax_rt
        !
        !---- set modified amplification rate AX2
        gr0 = 0.30 + 0.35 * exp(-0.15 * (Hk - 5.0))
        gr0_hk = -0.35 * exp(-0.15 * (Hk - 5.0)) * 0.15
        !
        tnr = tanh(1.2 * (gr - gr0))
        tnr_rt = (1.0 - tnr**2) * 1.2 * gr_rt
        tnr_hk = -(1.0 - tnr**2) * 1.2 * gr0_hk
        !
        ax2 = (0.086 * tnr - 0.25 / (Hk - 1.0)**1.5) / Th
        ax2_hk = (0.086 * tnr_hk + 1.5 * 0.25 / (Hk - 1.0)**2.5) / Th
        ax2_rt = (0.086 * tnr_rt) / Th
        ax2_th = -ax2 / Th
        !
        if (ax2<0.0) then
            ax2 = 0.0
            ax2_hk = 0.
            ax2_rt = 0.
            ax2_th = 0.
        endif
        !
        !---- blend the two amplification rates
        Ax = hfac * ax2 + (1.0 - hfac) * ax1
        Ax_hk = hfac * ax2_hk + (1.0 - hfac) * ax1_hk + hf_hk * (ax2 - ax1)
        Ax_rt = hfac * ax2_rt + (1.0 - hfac) * ax1_rt
        Ax_th = hfac * ax2_th + (1.0 - hfac) * ax1_th
        !
    end subroutine dampl2
    !*==HKIN.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! DAMPL2



    subroutine hkin(H, Msq, Hk, Hk_h, Hk_msq)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: H, Hk, Hk_h, Hk_msq, Msq
        intent (in) H, Msq
        intent (out) Hk_h, Hk_msq
        intent (inout) Hk
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
        !---- calculate kinematic shape parameter (assuming air)
        !     (from Whitfield )
        Hk = (H - 0.29 * Msq) / (1.0 + 0.113 * Msq)
        Hk_h = 1.0 / (1.0 + 0.113 * Msq)
        Hk_msq = (-.29 - 0.113 * Hk) / (1.0 + 0.113 * Msq)
        !
    end subroutine hkin
    !*==DIL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine dil(Hk, Rt, Di, Di_hk, Di_rt)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Di, Di_hk, Di_rt, Hk, Rt
        intent (in) Hk, Rt
        intent (out) Di_hk, Di_rt
        intent (inout) Di
        !
        ! Local variables
        !
        real :: den, hkb
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
        !---- Laminar dissipation function  ( 2 CD/H* )     (from Falkner-Skan)
        if (Hk<4.0) then
            Di = (0.00205 * (4.0 - Hk)**5.5 + 0.207) / Rt
            Di_hk = (-.00205 * 5.5 * (4.0 - Hk)**4.5) / Rt
        else
            hkb = Hk - 4.0
            den = 1.0 + 0.02 * hkb**2
            Di = (-.0016 * hkb**2 / den + 0.207) / Rt
            Di_hk = (-.0016 * 2.0 * hkb * (1.0 / den - 0.02 * hkb**2 / den**2)) / Rt
        endif
        Di_rt = -Di / Rt
        !
    end subroutine dil
    !*==DILW.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine dilw(Hk, Rt, Di, Di_hk, Di_rt)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Di, Di_hk, Di_rt, Hk, Rt
        intent (out) Di_hk, Di_rt
        intent (inout) Di
        !
        ! Local variables
        !
        real :: hs, hs_hk, hs_msq, hs_rt, msq, rcd, rcd_hk
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
        msq = 0.
        call hsl(Hk, Rt, msq, hs, hs_hk, hs_rt, hs_msq)
        !
        !---- Laminar wake dissipation function  ( 2 CD/H* )
        rcd = 1.10 * (1.0 - 1.0 / Hk)**2 / Hk
        rcd_hk = -1.10 * (1.0 - 1.0 / Hk) * 2.0 / Hk**3 - rcd / Hk
        !
        Di = 2.0 * rcd / (hs * Rt)
        Di_hk = 2.0 * rcd_hk / (hs * Rt) - (Di / hs) * hs_hk
        Di_rt = -Di / Rt - (Di / hs) * hs_rt
        !
    end subroutine dilw
    !*==HSL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine hsl(Hk, Rt, Msq, Hs, Hs_hk, Hs_rt, Hs_msq)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Hk, Hs, Hs_hk, Hs_msq, Hs_rt, Msq, Rt
        intent (in) Hk
        intent (out) Hs, Hs_hk, Hs_msq, Hs_rt
        !
        ! Local variables
        !
        real :: hs2, tmp
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
        !---- Laminar HS correlation
        if (Hk<4.35) then
            tmp = Hk - 4.35
            Hs = 0.0111 * tmp**2 / (Hk + 1.0) - 0.0278 * tmp**3 / (Hk + 1.0) + 1.528 - 0.0002 * (tmp * Hk)**2
            Hs_hk = 0.0111 * (2.0 * tmp - tmp**2 / (Hk + 1.0)) / (Hk + 1.0) &
                    - 0.0278 * (3.0 * tmp**2 - tmp**3 / (Hk + 1.0)) / (Hk + 1.0) &
                    - 0.0002 * 2.0 * tmp * Hk * (tmp + Hk)
        else
            hs2 = 0.015
            !       HS2 = 0.09
            Hs = hs2 * (Hk - 4.35)**2 / Hk + 1.528
            Hs_hk = hs2 * 2.0 * (Hk - 4.35) / Hk - hs2 * (Hk - 4.35)**2 / Hk**2
        endif
        !
        Hs_rt = 0.
        Hs_msq = 0.
        !
    end subroutine hsl
    !*==CFL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine cfl(Hk, Rt, Msq, Cf, Cf_hk, Cf_rt, Cf_msq)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Cf, Cf_hk, Cf_msq, Cf_rt, Hk, Msq, Rt
        intent (in) Hk, Rt
        intent (out) Cf_hk, Cf_msq, Cf_rt
        intent (inout) Cf
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
        !
        !---- Laminar skin friction function  ( Cf )    ( from Falkner-Skan )
        if (Hk<5.5) then
            tmp = (5.5 - Hk)**3 / (Hk + 1.0)
            Cf = (0.0727 * tmp - 0.07) / Rt
            Cf_hk = (-.0727 * tmp * 3.0 / (5.5 - Hk) - 0.0727 * tmp / (Hk + 1.0)) / Rt
        else
            tmp = 1.0 - 1.0 / (Hk - 4.5)
            Cf = (0.015 * tmp**2 - 0.07) / Rt
            Cf_hk = (0.015 * tmp * 2.0 / (Hk - 4.5)**2) / Rt
        endif
        Cf_rt = -Cf / Rt
        Cf_msq = 0.0
        !
    end subroutine cfl
    !*==DIT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine dit(Hs, Us, Cf, St, Di, Di_hs, Di_us, Di_cf, Di_st)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Cf, Di, Di_cf, Di_hs, Di_st, Di_us, Hs, St, Us
        intent (in) Cf, Hs, St, Us
        intent (out) Di, Di_cf, Di_hs, Di_st, Di_us
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
        !---- Turbulent dissipation function  ( 2 CD/H* )
        Di = (0.5 * Cf * Us + St * St * (1.0 - Us)) * 2.0 / Hs
        Di_hs = -(0.5 * Cf * Us + St * St * (1.0 - Us)) * 2.0 / Hs**2
        Di_us = (0.5 * Cf - St * St) * 2.0 / Hs
        Di_cf = (0.5 * Us) * 2.0 / Hs
        Di_st = (2.0 * St * (1.0 - Us)) * 2.0 / Hs
        !
    end subroutine dit
    !*==HST.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine hst(Hk, Rt, Msq, Hs, Hs_hk, Hs_rt, Hs_msq)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Hk, Hs, Hs_hk, Hs_msq, Hs_rt, Msq, Rt
        intent (in) Hk, Msq, Rt
        intent (out) Hs_msq
        intent (inout) Hs, Hs_hk, Hs_rt
        !
        ! Local variables
        !
        real, save :: dhsinf, hsmin
        real :: fm, grt, hdif, ho, ho_rt, hr, hr_hk, hr_rt, htmp, htmp_hk, htmp_rt, rtmp, rtz, rtz_rt
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
        !---- Turbulent HS correlation
        !
        data hsmin, dhsinf/1.500, 0.015/
        !
        !---- ###  12/4/94
        !---- limited Rtheta dependence for Rtheta < 200
        !
        !
        if (Rt>400.0) then
            ho = 3.0 + 400.0 / Rt
            ho_rt = -400.0 / Rt**2
        else
            ho = 4.0
            ho_rt = 0.
        endif
        !
        if (Rt>200.0) then
            rtz = Rt
            rtz_rt = 1.
        else
            rtz = 200.0
            rtz_rt = 0.
        endif
        !
        if (Hk<ho) then
            !----- attached branch
            !=======================================================
            !----- old correlation
            !-     (from Swafford profiles)
            !       SRT = SQRT(RT)
            !       HEX = (HO-HK)**1.6
            !       RTMP = 0.165 - 1.6/SRT
            !       HS    = HSMIN + 4.0/RT + RTMP*HEX/HK
            !       HS_HK = RTMP*HEX/HK*(-1.6/(HO-HK) - 1.0/HK)
            !       HS_RT = -4.0/RT**2 + HEX/HK*0.8/SRT/RT
            !     &             + RTMP*HEX/HK*1.6/(HO-HK)*HO_RT
            !=======================================================
            !----- new correlation  29 Nov 91
            !-     (from  arctan(y+) + Schlichting  profiles)
            hr = (ho - Hk) / (ho - 1.0)
            hr_hk = -1.0 / (ho - 1.0)
            hr_rt = (1.0 - hr) / (ho - 1.0) * ho_rt
            Hs = (2.0 - hsmin - 4.0 / rtz) * hr**2 * 1.5 / (Hk + 0.5) + hsmin + 4.0 / rtz
            Hs_hk = -(2.0 - hsmin - 4.0 / rtz) * hr**2 * 1.5 / (Hk + 0.5)**2 &
                    + (2.0 - hsmin - 4.0 / rtz) * hr * 2.0 * 1.5 / (Hk + 0.5) * hr_hk
            Hs_rt = (2.0 - hsmin - 4.0 / rtz) * hr * 2.0 * 1.5 / (Hk + 0.5) * hr_rt &
                    + (hr**2 * 1.5 / (Hk + 0.5) - 1.0) * 4.0 / rtz**2 * rtz_rt
            !
        else
            !
            !----- separated branch
            grt = log(rtz)
            hdif = Hk - ho
            rtmp = Hk - ho + 4.0 / grt
            htmp = 0.007 * grt / rtmp**2 + dhsinf / Hk
            htmp_hk = -.014 * grt / rtmp**3 - dhsinf / Hk**2
            htmp_rt = -.014 * grt / rtmp**3 * (-ho_rt - 4.0 / grt**2 / rtz * rtz_rt) + 0.007 / rtmp**2 / rtz * rtz_rt
            Hs = hdif**2 * htmp + hsmin + 4.0 / rtz
            Hs_hk = hdif * 2.0 * htmp + hdif**2 * htmp_hk
            Hs_rt = hdif**2 * htmp_rt - 4.0 / rtz**2 * rtz_rt + hdif * 2.0 * htmp * (-ho_rt)
            !
        endif
        !
        !---- fudge HS slightly to make sure   HS -> 2   as   HK -> 1
        !-    (unnecessary with new correlation)
        !      HTF    = 0.485/9.0 * (HK-4.0)**2/HK  +  1.515
        !      HTF_HK = 0.485/9.0 * (1.0-16.0/HK**2)
        !      ARG = MAX( 10.0*(1.0 - HK) , -15.0 )
        !      HXX = EXP(ARG)
        !      HXX_HK = -10.0*HXX
        !C
        !      HS_HK  = (1.0-HXX)*HS_HK  +  HXX*HTF_HK
        !     &       + (        -HS     +      HTF    )*HXX_HK
        !      HS_RT  = (1.0-HXX)*HS_RT
        !      HS     = (1.0-HXX)*HS     +  HXX*HTF
        !
        !---- Whitfield's minor additional compressibility correction
        fm = 1.0 + 0.014 * Msq
        Hs = (Hs + 0.028 * Msq) / fm
        Hs_hk = (Hs_hk) / fm
        Hs_rt = (Hs_rt) / fm
        Hs_msq = 0.028 / fm - 0.014 * Hs / fm
        !
    end subroutine hst
    !*==CFT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine cft(Hk, Rt, Msq, Cf, Cf_hk, Cf_rt, Cf_msq)
        use i_blpar
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Cf, Cf_hk, Cf_msq, Cf_rt, Hk, Msq, Rt
        intent (in) Hk, Msq, Rt
        intent (out) Cf_hk, Cf_msq, Cf_rt
        intent (inout) Cf
        !
        ! Local variables
        !
        real :: arg, cfo, fc, gex, gm1, grt, thk
        real, save :: gam
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
        data gam/1.4/
        !
        !---- Turbulent skin friction function  ( Cf )    (Coles)
        gm1 = gam - 1.0
        fc = sqrt(1.0 + 0.5 * gm1 * Msq)
        grt = log(Rt / fc)
        grt = max(grt, 3.0)
        !
        gex = -1.74 - 0.31 * Hk
        !
        arg = -1.33 * Hk
        arg = max(-20.0, arg)
        !
        thk = tanh(4.0 - Hk / 0.875)
        !
        cfo = CFFac * 0.3 * exp(arg) * (grt / 2.3026)**gex
        Cf = (cfo + 1.1E-4 * (thk - 1.0)) / fc
        Cf_hk = (-1.33 * cfo - 0.31 * log(grt / 2.3026) * cfo - 1.1E-4 * (1.0 - thk**2) / 0.875) / fc
        Cf_rt = gex * cfo / (fc * grt) / Rt
        Cf_msq = gex * cfo / (fc * grt) * (-0.25 * gm1 / fc**2) - 0.25 * gm1 * Cf / fc**2
        !
    end subroutine cft
    !*==HCT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! CFT



    subroutine hct(Hk, Msq, Hc, Hc_hk, Hc_msq)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Hc, Hc_hk, Hc_msq, Hk, Msq
        intent (in) Hk, Msq
        intent (out) Hc, Hc_hk, Hc_msq
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
        !---- density shape parameter    (from Whitfield)
        Hc = Msq * (0.064 / (Hk - 0.8) + 0.251)
        Hc_hk = Msq * (-.064 / (Hk - 0.8)**2)
        Hc_msq = 0.064 / (Hk - 0.8) + 0.251
        !
    end subroutine hct

end module m_xblsys