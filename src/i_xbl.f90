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

!*==I_XBL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module i_xbl
    use i_blpar
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! PARAMETER definitions
    !
    integer, parameter :: NCOM = 73
    !
    ! Local variables
    !
    real :: amcrit, bule, cfm, cfm_d1, cfm_d2, cfm_ms, cfm_re, cfm_t1, cfm_t2, cfm_u1, cfm_u2, dwte, gambl, &
            & gm1bl, hstinv, hstinv_ms, hvrat, qinfbl, reybl, reybl_ms, reybl_re, rstbl, rstbl_ms, tkbl, &
            & tkbl_ms, xiforc, xt, xt_a1, xt_d1, xt_d2, xt_ms, xt_re, xt_t1, xt_t2, xt_u1, xt_u2, xt_x1, &
            & xt_x2, xt_xf
    real, pointer :: ampl1, ampl2, cf1, cf1_d1, cf1_ms, cf1_re, cf1_t1, cf1_u1, cf2, cf2_d2, cf2_ms, cf2_re, &
            & cf2_t2, cf2_u2, cq1, cq1_d1, cq1_ms, cq1_re, cq1_t1, cq1_u1, cq2, cq2_d2, cq2_ms, &
            & cq2_re, cq2_t2, cq2_u2, d1, d2, de1, de1_d1, de1_ms, de1_t1, de1_u1, de2, de2_d2, &
            & de2_ms, de2_t2, de2_u2, di1, di1_d1, di1_ms, di1_re, di1_s1, di1_t1, di1_u1, di2, &
            & di2_d2, di2_ms, di2_re, di2_s2, di2_t2, di2_u2, dw1, dw2, h1, h1_d1, h1_t1, h2, h2_d2, &
            & h2_t2, hc1, hc1_d1, hc1_ms, hc1_t1, hc1_u1, hc2, hc2_d2, hc2_ms, hc2_t2, hc2_u2, hk1, &
            & hk1_d1, hk1_ms, hk1_t1, hk1_u1, hk2, hk2_d2, hk2_ms, hk2_t2, hk2_u2
    real, dimension(NCOM) :: c1sav, c2sav
    real, target, dimension(NCOM) :: com1, com2
    real, pointer :: hs1, hs1_d1, hs1_ms, hs1_re, hs1_t1, hs1_u1, hs2, hs2_d2, hs2_ms, hs2_re, hs2_t2, &
            & hs2_u2, m1, m1_ms, m1_u1, m2, m2_ms, m2_u2, r1, r1_ms, r1_u1, r2, r2_ms, r2_u2, rt1, &
            & rt1_ms, rt1_re, rt1_t1, rt1_u1, rt2, rt2_ms, rt2_re, rt2_t2, rt2_u2, s1, s2, t1, t2, &
            & u1, u1_ms, u1_uei, u2, u2_ms, u2_uei, us1, us1_d1, us1_ms, us1_re, us1_t1, us1_u1, us2, &
            & us2_d2, us2_ms, us2_re, us2_t2, us2_u2, v1, v1_ms, v1_re, v1_u1, v2, v2_ms, v2_re, &
            & v2_u2, x1, x2
    integer :: idampv
    logical :: simi, tran, trforc, trfree, turb, wake
    real, dimension(4, 5) :: vs1, vs2
    real, dimension(4) :: vsm, vsr, vsrez, vsx
    !
    !*** End of declarations rewritten by SPAG
    !
    !
    ! PARAMETER definitions
    !
    !
    ! COMMON /V_COM/
    !
    !
    ! COMMON /V_INT/
    !
    !
    ! COMMON /V_SAV/
    !
    !
    ! COMMON /V_SYS/
    !
    !
    ! COMMON /V_VAR/
    !
    !
    ! COMMON /V_VAR1/
    !
    !
    ! COMMON /V_VAR2/
    !
    !
    ! COMMON /V_VARA/
    !
end module i_xbl
