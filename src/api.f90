!***********************************************************************
!   Copyright (c) 2019 D. de Vries
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

module api
    use, intrinsic :: iso_c_binding, only: c_float, c_int, c_bool, c_null_ptr
    implicit none
contains
    subroutine set_print(setting) bind(c, name='set_print')
        use i_xfoil, only: show_output
        logical(c_bool), intent(in) :: setting
        show_output = setting
    end subroutine set_print

    function get_print() bind(c, name='get_print')
        use i_xfoil, only: show_output
        logical(c_bool) :: get_print
        get_print = show_output
    end function get_print

    subroutine set_airfoil(x_in, y_in, n_in) bind(c, name='set_airfoil')
        use m_xgeom, only: geopar, norm
        use m_spline, only: seval, segspl, scalc
        use m_xgdes, only: abcopy
        use m_xgeom, only: cang
        use i_xfoil

        integer(c_int), intent(in) :: n_in
        real(c_float), intent(in) :: x_in(n_in), y_in(n_in)

        ! Local variables
        !
        real :: area, xble, xbte, xinl, xout, xtmp, yble, ybot, ybte, ytmp, ytop, amax, angtol
        integer :: i, ip, kdot, lu, imax, Itype

        data angtol/40.0/

        do i=1, n_in
            XB(i) = x_in(i)
            YB(i) = y_in(i)
        end do
        NB = n_in

        !---- calculate airfoil area assuming counterclockwise ordering
        area = 0.0
        do i = 1, NB
            ip = i + 1
            if (i==NB) ip = 1
            area = area + 0.5 * (YB(i) + YB(ip)) * (XB(i) - XB(ip))
        enddo
        !
        if (area>=0.0) then
            LCLock = .false.
            if (show_output) write (*, 99001) NB
            !...............................................................
            99001 format (/' Number of input coordinate points:', i4/' Counterclockwise ordering')
        else
            !----- if area is negative (clockwise order), reverse coordinate order
            LCLock = .true.
            if (show_output) write (*, 99002) NB
            99002 format (/' Number of input coordinate points:', i4/' Clockwise ordering')
            do i = 1, NB / 2
                xtmp = XB(NB - i + 1)
                ytmp = YB(NB - i + 1)
                XB(NB - i + 1) = XB(i)
                YB(NB - i + 1) = YB(i)
                XB(i) = xtmp
                YB(i) = ytmp
            enddo
        endif
        !
        if (LNOrm) then
            call norm(XB, XBP, YB, YBP, SB, NB)
            if (show_output) write (*, 99003)
            99003 format (/' Airfoil has been normalized')
        endif
        !
        call scalc(XB, YB, SB, NB)
        call segspl(XB, XBP, SB, NB)
        call segspl(YB, YBP, SB, NB)
        !
        call geopar(XB, XBP, YB, YBP, SB, NB, W1, SBLe, CHOrdb, AREab, &
                RADble, ANGbte, EI11ba, EI22ba, APX1ba, APX2ba, EI11bt, EI22bt, APX1bt, &
                & APX2bt, THIckb, CAMbrb)
        !
        xble = seval(SBLe, XB, XBP, SB, NB)
        yble = seval(SBLe, YB, YBP, SB, NB)
        xbte = 0.5 * (XB(1) + XB(NB))
        ybte = 0.5 * (YB(1) + YB(NB))
        !
        if (show_output) write (*, 99004) xble, yble, CHOrdb, xbte, ybte
        99004 format (/'  LE  x,y  =', 2F10.5, '  |   Chord =', f10.5/'  TE  x,y  =', 2F10.5, '  |')
        !
        !---- set reasonable MSES domain parameters for non-MSES coordinate file
        if (Itype<=2 .and. ISPars==' ') then
            xble = seval(SBLe, XB, XBP, SB, NB)
            yble = seval(SBLe, YB, YBP, SB, NB)
            xinl = xble - 2.0 * CHOrdb
            xout = xble + 3.0 * CHOrdb
            ybot = yble - 2.5 * CHOrdb
            ytop = yble + 3.5 * CHOrdb
            xinl = aint(20.0 * abs(xinl / CHOrdb) + 0.5) / 20.0 * sign(CHOrdb, xinl)
            xout = aint(20.0 * abs(xout / CHOrdb) + 0.5) / 20.0 * sign(CHOrdb, xout)
            ybot = aint(20.0 * abs(ybot / CHOrdb) + 0.5) / 20.0 * sign(CHOrdb, ybot)
            ytop = aint(20.0 * abs(ytop / CHOrdb) + 0.5) / 20.0 * sign(CHOrdb, ytop)
            write (ISPars, 99005) xinl, xout, ybot, ytop
            99005 format (1x, 4F8.2)
        endif
        !
        !---- wipe out old flap hinge location
        XBF = 0.0
        YBF = 0.0
        LBFlap = .false.
        !
        !---- wipe out off-design alphas, CLs
        !c      NALOFF = 0
        !c      NCLOFF = 0
        !

        call abcopy(.true.)
        !
        call cang(X, Y, N, 0, imax, amax)
        if (abs(amax)>angtol .and. show_output) write (*, 99007) amax, imax
        99007 format (/&
                ' WARNING: Poor input coordinate distribution'/&
                '          Excessive panel angle', f7.1, '  at i =', i4/&
                '          Repaneling with PANE and/or PPAR suggested'/&
                '           (doing GDES,CADD before repaneling _may_'/&
                '            improve excessively coarse LE spacing')
    end subroutine set_airfoil

    function get_n_coords() bind(c, name='get_n_coords')
        use i_xfoil, only: NB
        integer(c_int) :: get_n_coords
        get_n_coords = NB
    end function get_n_coords

    subroutine get_airfoil(x, y, n) bind(c, name='get_airfoil')
        use i_xfoil, only: XB, YB
        integer(c_int), intent(in) :: n
        real(c_float), intent(inout) :: x(n), y(n)
        integer :: i
        do i=1, n
            x(i) = XB(i)
            y(i) = YB(i)
        end do
    end subroutine get_airfoil

    subroutine set_naca(spec) bind(c, name='set_naca')
        use m_xfoil, only: naca
        integer(c_int), intent(in) :: spec
        if (spec <= 0) then
            write(0, *) 'Invalid NACA specifier. Specify a NACA 4 or 5 series airfoil code.'
        else
            call naca(spec)
        end if
    end subroutine set_naca

    subroutine set_reynolds(Re) bind(c, name='set_reynolds')
        use i_xfoil
        real(c_float), intent(in) :: Re

        if (Re == 0.) then
            LVIsc = .false.
        else
            LVIsc = .true.
        end if

        REInf1 = Re

        LBLini = .false.
        LIPan = .false.

        LVConv = .false.
    end subroutine set_reynolds

    function get_reynolds() bind(c, name='get_reynolds')
        use i_xfoil, only: REInf1
        real(c_float) :: get_reynolds
        get_reynolds = REInf1
    end function get_reynolds

    subroutine set_mach(M) bind(c, name='set_mach')
        use s_xfoil, only: comset, cpcalc, clcalc, cdcalc
        use i_xfoil

        real(c_float), intent(in) :: M

        if (M /= MINf) then
            MINf = M

            call comset
            if (MINf>0.0 .and. show_output) write (*, 99003) CPStar, QSTar / QINf
            99003      format (/' Sonic Cp =', f10.2, '      Sonic Q/Qinf =', f10.3/)

            call cpcalc(N, QINv, QINf, MINf, CPI)
            if (LVIsc) call cpcalc(N + NW, QVIs, QINf, MINf, CPV)
            call clcalc(N, X, Y, GAM, GAM_a, ALFa, MINf, QINf, XCMref, YCMref, CL, CM, CDP, CL_alf, CL_msq)
            call cdcalc
        end if

        LVConv = .false.
    end subroutine set_mach

    function get_mach() bind(c, name='get_mach')
        use i_xfoil, only: MINf
        real(c_float) :: get_mach
        get_mach = MINf
    end function get_mach

    subroutine set_xtr(xtr_top, xtr_bot) bind(c, name='set_xtr')
        use i_xfoil, only: XSTrip, LVConv
        real(c_float), intent(in) :: xtr_top, xtr_bot
        XSTrip(1) = xtr_top
        XSTrip(2) = xtr_bot
        LVConv = .false.
    end subroutine set_xtr

    subroutine get_xtr(xtr_top, xtr_bot) bind(c, name='get_xtr')
        use i_xfoil, only: XSTrip
        real(c_float) :: xtr_top, xtr_bot
        xtr_top = XSTrip(1)
        xtr_bot = XSTrip(2)
    end subroutine get_xtr

    subroutine set_n_crit(n_crit) bind(c, name='set_n_crit')
        use i_xfoil, only: ACRit, LVConv
        real(c_float), intent(in) :: n_crit
        ACRit(1) = n_crit
        ACRit(2) = n_crit
        LVConv = .false.
    end subroutine set_n_crit

    function get_n_crit() bind(c, name='get_n_crit')
        use i_xfoil, only: ACRit
        real(c_float) :: get_n_crit
        get_n_crit = ACRit(1)
    end function get_n_crit

    subroutine set_max_iter(max_iter) bind(c, name='set_max_iter')
        use i_xfoil, only: ITMax
        integer(c_int), intent(in) :: max_iter
        ITMax = max_iter
    end subroutine set_max_iter

    function get_max_iter() bind(c, name='get_max_iter')
        use i_xfoil, only: ITMax
        integer(c_int) :: get_max_iter
        get_max_iter = ITMax
    end function get_max_iter

    subroutine reset_bls() bind(c, name='reset_bls')
        use i_xfoil, only: LBLini, LIPan, show_output
        LBLini = .false.
        LIPan = .false.
        if (show_output) write (*, *) 'BLs will be initialized on next point'
    end subroutine reset_bls

    subroutine repanel(n_panel, cv_par, cte_ratio, ctr_ratio, &
                       xs_ref1, xs_ref2, xp_ref1, xp_ref2) bind(c, name='repanel')
        use m_xfoil, only: pangen
        use m_xgeom, only: cang
        use i_xfoil

        integer :: imax
        real :: amax
        integer(c_int), intent(in) :: n_panel
        real(c_float), intent(in) :: cv_par, cte_ratio, ctr_ratio, xs_ref1, xs_ref2, xp_ref1, xp_ref2

        NPAn = min(n_panel, IQX - 6)
        CVPar = cv_par
        CTErat = cte_ratio
        CTRrat = ctr_ratio
        XSRef1 = xs_ref1
        XSRef2 = xs_ref2
        XPRef1 = xp_ref1
        XPRef2 = xp_ref2

        call pangen(.true.)
        if (N>0) call cang(X, Y, N, 1, imax, amax)
    end subroutine repanel

    subroutine filter(factor) bind(c, name='filter')
        use m_xmdes, only: cnfilt, piqsum, qspcir, qspint
        use i_xfoil

        real(c_float), intent(in) :: factor

        real :: cfilt, clq
        integer :: kqsp

        !----- apply modified Hanning filter to Cn coefficients
        cfilt = factor
        call cnfilt(cfilt)
        call piqsum
        call qspcir

        do kqsp = 1, NQSp
            call qspint(ALQsp(kqsp), QSPec(1, kqsp), QINf, MINf, clq, CMQsp(kqsp))
            if (IACqsp==1) CLQsp(kqsp) = clq
        enddo
        LQSppl = .false.
    end subroutine filter

    subroutine alfa_(a_input, cl_out, cd_out, cm_out, cp_out, conv) bind(c, name='alfa')
        use m_xoper, only: specal, viscal, fcpmin
        use i_xfoil

        real(c_float), intent(in) :: a_input
        real(c_float), intent(out) :: cl_out, cd_out, cm_out, cp_out
        logical(c_bool), intent(out) :: conv
        ADEg = a_input

        ALFa = a_input * DTOr
        QINf = 1.0
        LALfa = .true.
        call specal
        if (abs(ALFa - AWAke)>1.0E-5) LWAke = .false.
        if (abs(ALFa - AVIsc)>1.0E-5) LVConv = .false.
        if (abs(MINf - MVIsc)>1.0E-5) LVConv = .false.
        !
        if (LVIsc) then
            conv = viscal(ITMax)
            conv = LVConv .and. conv
        else
            conv = .true.
        end if

        cl_out = CL
        cd_out = CD
        cm_out = CM

        call fcpmin
        cp_out = CPMn
    end subroutine alfa_

    subroutine cl_(cl_input, a_out, cd_out, cm_out, cp_out, conv) bind(c, name='cl')
        use m_xoper, only: speccl, viscal, fcpmin
        use i_xfoil

        real(c_float), intent(in) :: cl_input
        real(c_float), intent(out) :: a_out, cd_out, cm_out, cp_out
        logical(c_bool), intent(out) :: conv

        CLSpec = cl_input
        ALFa = 0.0
        QINf = 1.0
        LALfa = .false.
        call speccl
        ADEg = ALFa / DTOr
        if (abs(ALFa - AWAke)>1.0E-5) LWAke = .false.
        if (abs(ALFa - AVIsc)>1.0E-5) LVConv = .false.
        if (abs(MINf - MVIsc)>1.0E-5) LVConv = .false.

        if (LVIsc) then
            conv = viscal(ITMax)
            conv = LVConv .and. conv
        else
            conv = .true.
        end if

        a_out = ALFa / DTOr
        cd_out = CD
        cm_out = CM

        call fcpmin
        cp_out = CPMn
    end subroutine cl_

    subroutine aseq(a_start, a_end, n_step, &
                    a_arr, cl_arr, cd_arr, cm_arr, cp_arr, conv_arr) bind(c, name='aseq')
        use m_xoper, only: specal, viscal, fcpmin
        use i_xfoil
        real(c_float), intent(in) :: a_start, a_end
        integer(c_int), intent(in) :: n_step
        real(c_float), dimension(n_step), intent(inout) :: a_arr, cl_arr, cd_arr, cm_arr, cp_arr
        logical(c_bool), dimension(n_step), intent(inout) :: conv_arr
        integer :: i, j, iseqex, itmaxs
        real :: a0, da, nan

        nan = 0
        nan = 0/nan

        a0 = a_start * DTOr
        da = (a_end - a_start) / float(n_step) * DTOr

        LALfa = .true.

        !----- initialize unconverged-point counter
        iseqex = 0

        do i=1, n_step
            ALFa = a0 + da * float(i - 1)
            if (abs(ALFa - AWAke)>1.0E-5) LWAke = .false.
            if (abs(ALFa - AVIsc)>1.0E-5) LVConv = .false.
            if (abs(MINf - MVIsc)>1.0E-5) LVConv = .false.
            call specal
            itmaxs = ITMax + 5
            if (LVIsc) then
                conv_arr(i) = viscal(itmaxs)
            end if
            ADEg = ALFa / DTOr

            a_arr(i) = ADEg
            cl_arr(i) = CL
            cd_arr(i) = CD
            cm_arr(i) = CM

            call fcpmin
            cp_arr(i) = CPMn

            if ((LVConv .and. conv_arr(i)) .or. .not.LVIsc) then
                conv_arr(i) = .true.
            elseif (LVIsc .and. .not. (LVConv .and. conv_arr(i))) then
                conv_arr(i) = .false.
            endif
        end do
    end subroutine aseq

    subroutine cseq(cl_start, cl_end, n_step, &
            a_arr, cl_arr, cd_arr, cm_arr, cp_arr, conv_arr) bind(c, name='cseq')
        use m_xoper, only: specal, viscal, speccl, fcpmin
        use i_xfoil
        real(c_float), intent(in) :: cl_start, cl_end
        integer(c_int), intent(in) :: n_step
        real(c_float), dimension(n_step), intent(inout) :: a_arr, cl_arr, cd_arr, cm_arr, cp_arr
        logical(c_bool), dimension(n_step), intent(inout) :: conv_arr
        integer :: i, j, iseqex, itmaxs
        real :: cl0, dcl

        cl0 = cl_start
        dcl = (cl_end - cl_start) / float(n_step)
        LALfa = .false.

        !----- initialize unconverged-point counter
        iseqex = 0

        do i=1, n_step
            CLSpec = cl0 + dcl * float(i - 1)
            call speccl
            if (abs(ALFa - AWAke)>1.0E-5) LWAke = .false.
            if (abs(ALFa - AVIsc)>1.0E-5) LVConv = .false.
            if (abs(MINf - MVIsc)>1.0E-5) LVConv = .false.

            itmaxs = ITMax + 5
            if (LVIsc) then
                conv_arr(i) = viscal(itmaxs)
            end if
            ADEg = ALFa / DTOr

            a_arr(i) = ADEg
            cl_arr(i) = CL
            cd_arr(i) = CD
            cm_arr(i) = CM

            call fcpmin
            cp_arr(i) = CPMn

            if ((LVConv .and. conv_arr(i)) .or. .not.LVIsc) then
                conv_arr(i) = .true.
            elseif (LVIsc .and. .not. (LVConv .and. conv_arr(i))) then
                conv_arr(i) = .false.
            endif
        end do
    end subroutine cseq


    function get_n_cp() bind(c, name='get_n_cp')
        use i_xfoil, only: N
        integer(c_int) :: get_n_cp
        get_n_cp = N
    end function get_n_cp

    subroutine get_cp(x_out, cp_out, n_points) bind(c, name='get_cp')
        use s_xfoil, only: comset
        use m_xoper
        use i_xfoil
        implicit none

        integer(c_int), intent(in) :: n_points
        real(c_float), dimension(n_points), intent(inout) :: x_out, cp_out

        real :: beta, bfac, cpcom, cpinc, den
        integer :: i

        call comset
        beta = sqrt(1.0 - MINf**2)
        bfac = 0.5 * MINf**2 / (1.0 + beta)

        do i = 1, N
            cpinc = 1.0 - (GAM(i) / QINf)**2
            den = beta + bfac * cpinc
            cp_out(i) = cpinc / den
            x_out(i) = X(i)
        enddo
    end subroutine get_cp

end module api
