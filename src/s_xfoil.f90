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

module s_xfoil
contains
    subroutine mrcl(Cls, M_cls, R_cls)
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Cls, M_cls, R_cls
        intent (in) Cls
        intent (out) M_cls, R_cls
        !
        ! Local variables
        !
        real :: cla, rrat
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
        !     Sets actual Mach, Reynolds numbers
        !     from unit-CL values and specified CLS
        !     depending on MATYP,RETYP flags.
        !-------------------------------------------
        !
        cla = max(Cls, 0.000001)
        !
        if (RETyp<1 .or. RETyp>3) then
            if (show_output) then
                write (*, *) 'MRCL:  Illegal Re(CL) dependence trigger.'
                write (*, *) '       Setting fixed Re.'
            endif
            RETyp = 1
        endif
        if (MATyp<1 .or. MATyp>3) then
            if (show_output) then
                write (*, *) 'MRCL:  Illegal Mach(CL) dependence trigger.'
                write (*, *) '       Setting fixed Mach.'
            endif
            MATyp = 1
        endif
        !
        !
        if (MATyp==1) then
            !
            MINf = MINf1
            M_cls = 0.
            !
        elseif (MATyp==2) then
            !
            MINf = MINf1 / sqrt(cla)
            M_cls = -0.5 * MINf / cla
            !
        elseif (MATyp==3) then
            !
            MINf = MINf1
            M_cls = 0.
            !
        endif
        !
        !
        if (RETyp==1) then
            !
            REInf = REInf1
            R_cls = 0.
            !
        elseif (RETyp==2) then
            !
            REInf = REInf1 / sqrt(cla)
            R_cls = -0.5 * REInf / cla
            !
        elseif (RETyp==3) then
            !
            REInf = REInf1 / cla
            R_cls = -REInf / cla
            !
        endif
        !
        !
        if (MINf>=0.99) then
            if (show_output) then
                write (*, *)
                write (*, *) 'MRCL: CL too low for chosen Mach(CL) dependence'
                write (*, *) '      Aritificially limiting Mach to  0.99'
            endif
            MINf = 0.99
            M_cls = 0.
        endif
        !
        rrat = 1.0
        if (REInf1>0.0) rrat = REInf / REInf1
        !
        if (rrat>100.0) then
            if (show_output) then
                write (*, *)
                write (*, *) 'MRCL: CL too low for chosen Re(CL) dependence'
                write (*, *) '      Aritificially limiting Re to ', REInf1 * 100.0
            endif
            REInf = REInf1 * 100.0
            R_cls = 0.
        endif
        !
    end subroutine mrcl
    !*==GETDEF.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! MRCL


    subroutine comset
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: beta, beta_msq
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
        !---- set Karman-Tsien parameter TKLAM
        beta = sqrt(1.0 - MINf**2)
        beta_msq = -0.5 / beta
        !
        TKLam = MINf**2 / (1.0 + beta)**2
        TKL_msq = 1.0 / (1.0 + beta)**2 - 2.0 * TKLam / (1.0 + beta) * beta_msq
        !
        !---- set sonic Pressure coefficient and speed
        if (MINf==0.0) then
            CPStar = -999.0
            QSTar = 999.0
        else
            CPStar = 2.0 / (GAMma * MINf**2) &
                    * (((1.0 + 0.5 * GAMm1 * MINf**2) / (1.0 + 0.5 * GAMm1))**(GAMma / GAMm1) - 1.0)
            QSTar = QINf / MINf * sqrt((1.0 + 0.5 * GAMm1 * MINf**2) / (1.0 + 0.5 * GAMm1))
        endif
        !
    end subroutine comset
    !*==CPCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! COMSET

    subroutine cpcalc(N, Q, Qinf, Minf, Cp)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Minf, Qinf
        integer :: N
        real, dimension(N) :: Cp, Q
        intent (in) Minf, N, Q, Qinf
        intent (out) Cp
        !
        ! Local variables
        !
        real :: beta, bfac, cpinc, den
        logical :: denneg
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
        !---------------------------------------------
        !     Sets compressible Cp from speed.
        !---------------------------------------------
        !
        !
        beta = sqrt(1.0 - Minf**2)
        bfac = 0.5 * Minf**2 / (1.0 + beta)
        !
        denneg = .false.
        !
        do i = 1, N
            cpinc = 1.0 - (Q(i) / Qinf)**2
            den = beta + bfac * cpinc
            Cp(i) = cpinc / den
            if (den<=0.0) denneg = .true.
        enddo
        !
        if (denneg) then
            if (show_output) then
                write (*, *)
                write (*, *) 'CPCALC: Local speed too large. ', 'Compressibility corrections invalid.'
            endif
        endif
        !
    end subroutine cpcalc
    !*==CLCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! CPCALC


    subroutine clcalc(N, X, Y, Gam, Gam_a, Alfa, Minf, Qinf, Xref, Yref, Cl, Cm, Cdp, Cl_alf, Cl_msq)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Alfa, Cdp, Cl, Cl_alf, Cl_msq, Cm, Minf, Qinf, Xref, Yref
        integer :: N
        real, dimension(N) :: Gam, Gam_a, X, Y
        intent (in) Alfa, Gam, Gam_a, Minf, N, Qinf, X, Xref, Y, Yref
        intent (inout) Cdp, Cl, Cl_alf, Cl_msq, Cm
        !
        ! Local variables
        !
        real :: ag, ag_alf, ag_msq, ax, ay, beta, beta_msq, bfac, bfac_msq, ca, cginc, cpc_cpi, cpg1, cpg1_alf, &
                & cpg1_msq, cpg2, cpg2_alf, cpg2_msq, cpi_gam, dg, dx, dx_alf, dy, sa
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
        !-----------------------------------------------------------
        !     Integrates surface pressures to get CL and CM.
        !     Integrates skin friction to get CDF.
        !     Calculates dCL/dAlpha for prescribed-CL routines.
        !-----------------------------------------------------------
        !
        !cC---- moment-reference coordinates
        !c      XREF = 0.25
        !c      YREF = 0.
        !
        sa = sin(Alfa)
        ca = cos(Alfa)
        !
        beta = sqrt(1.0 - Minf**2)
        beta_msq = -0.5 / beta
        !
        bfac = 0.5 * Minf**2 / (1.0 + beta)
        bfac_msq = 0.5 / (1.0 + beta) - bfac / (1.0 + beta) * beta_msq
        !
        Cl = 0.0
        Cm = 0.0

        Cdp = 0.0
        !
        Cl_alf = 0.
        Cl_msq = 0.
        !
        i = 1
        cginc = 1.0 - (Gam(i) / Qinf)**2
        cpg1 = cginc / (beta + bfac * cginc)
        cpg1_msq = -cpg1 / (beta + bfac * cginc) * (beta_msq + bfac_msq * cginc)
        !
        cpi_gam = -2.0 * Gam(i) / Qinf**2
        cpc_cpi = (1.0 - bfac * cpg1) / (beta + bfac * cginc)
        cpg1_alf = cpc_cpi * cpi_gam * Gam_a(i)
        !
        do i = 1, N
            ip = i + 1
            if (i==N) ip = 1
            !
            cginc = 1.0 - (Gam(ip) / Qinf)**2
            cpg2 = cginc / (beta + bfac * cginc)
            cpg2_msq = -cpg2 / (beta + bfac * cginc) * (beta_msq + bfac_msq * cginc)
            !
            cpi_gam = -2.0 * Gam(ip) / Qinf**2
            cpc_cpi = (1.0 - bfac * cpg2) / (beta + bfac * cginc)
            cpg2_alf = cpc_cpi * cpi_gam * Gam_a(ip)
            !
            dx = (X(ip) - X(i)) * ca + (Y(ip) - Y(i)) * sa
            dy = (Y(ip) - Y(i)) * ca - (X(ip) - X(i)) * sa
            dg = cpg2 - cpg1
            !
            ax = (0.5 * (X(ip) + X(i)) - Xref) * ca + (0.5 * (Y(ip) + Y(i)) - Yref) * sa
            ay = (0.5 * (Y(ip) + Y(i)) - Yref) * ca - (0.5 * (X(ip) + X(i)) - Xref) * sa
            ag = 0.5 * (cpg2 + cpg1)
            !
            dx_alf = -(X(ip) - X(i)) * sa + (Y(ip) - Y(i)) * ca
            ag_alf = 0.5 * (cpg2_alf + cpg1_alf)
            ag_msq = 0.5 * (cpg2_msq + cpg1_msq)
            !
            Cl = Cl + dx * ag
            Cdp = Cdp - dy * ag
            Cm = Cm - dx * (ag * ax + dg * dx / 12.0) - dy * (ag * ay + dg * dy / 12.0)
            !
            Cl_alf = Cl_alf + dx * ag_alf + ag * dx_alf
            Cl_msq = Cl_msq + dx * ag_msq
            !
            cpg1 = cpg2
            cpg1_alf = cpg2_alf
            cpg1_msq = cpg2_msq
        enddo
        !
    end subroutine clcalc
    !*==CDCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! CLCALC


    subroutine cdcalc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: ca, dx, sa, shwake, thwake, uewake, urat
        integer :: i, ibl, im, is
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
        sa = sin(ALFa)
        ca = cos(ALFa)
        !
        if (LVIsc .and. LBLini) then
            !
            !----- set variables at the end of the wake
            thwake = THEt(NBL(2), 2)
            urat = UEDg(NBL(2), 2) / QINf
            uewake = UEDg(NBL(2), 2) * (1.0 - TKLam) / (1.0 - TKLam * urat**2)
            shwake = DSTr(NBL(2), 2) / THEt(NBL(2), 2)
            !
            !----- extrapolate wake to downstream infinity using Squire-Young relation
            !      (reduces errors of the wake not being long enough)
            CD = 2.0 * thwake * (uewake / QINf)**(0.5 * (5.0 + shwake))
            !
        else
            !
            CD = 0.0
            !
        endif
        !
        !---- calculate friction drag coefficient
        CDF = 0.0
        do is = 1, 2
            do ibl = 3, IBLte(is)
                i = IPAn(ibl, is)
                im = IPAn(ibl - 1, is)
                dx = (X(i) - X(im)) * ca + (Y(i) - Y(im)) * sa
                CDF = CDF + 0.5 * (TAU(ibl, is) + TAU(ibl - 1, is)) * dx * 2.0 / QINf**2
            enddo
        enddo
        !
    end subroutine cdcalc
    !*==LOAD.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! CDCALC


    subroutine tecalc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: dxs, dxte, dys, dyte, scs, sds
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
        !-------------------------------------------
        !     Calculates total and projected TE gap
        !     areas and TE panel strengths.
        !-------------------------------------------
        !
        !---- set TE base vector and TE bisector components
        dxte = X(1) - X(N)
        dyte = Y(1) - Y(N)
        dxs = 0.5 * (-XP(1) + XP(N))
        dys = 0.5 * (-YP(1) + YP(N))
        !
        !---- normal and streamwise projected TE gap areas
        ANTe = dxs * dyte - dys * dxte
        ASTe = dxs * dxte + dys * dyte
        !
        !---- total TE gap area
        DSTe = sqrt(dxte**2 + dyte**2)
        !
        SHArp = DSTe<0.0001 * CHOrd
        !
        if (SHArp) then
            scs = 1.0
            sds = 0.0
        else
            scs = ANTe / DSTe
            sds = ASTe / DSTe
        endif
        !
        !---- TE panel source and vorticity strengths
        SIGte = 0.5 * (GAM(1) - GAM(N)) * scs
        GAMte = -.5 * (GAM(1) - GAM(N)) * sds
        !
        SIGte_a = 0.5 * (GAM_a(1) - GAM_a(N)) * scs
        GAMte_a = -.5 * (GAM_a(1) - GAM_a(N)) * sds
        !
    end subroutine tecalc
    !*==INTE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! TECALC

end module s_xfoil