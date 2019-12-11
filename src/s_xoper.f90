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

module s_xoper
contains
    ! mhinge was originally in xoper between cpdump and vpar
    ! It was moved here to avoid a circular dependency.
    subroutine mhinge
        use m_xgdes, only : getxyf
        use m_spline, only : seval, sinvrt
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: botp, bots, botx, boty, dx, dy, frac, pmid, topp, tops, topx, topy, xmid, ymid
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
        !----------------------------------------------------
        !     Calculates the hinge moment of the flap about
        !     (XOF,YOF) by integrating surface pressures.
        !----------------------------------------------------
        !
        if (.not.LFLap) then
            !
            call getxyf(X, XP, Y, YP, S, N, tops, bots, XOF, YOF)
            LFLap = .true.
            !
        else
            !
            !------ find top and bottom y at hinge x location
            tops = XOF
            bots = S(N) - XOF
            call sinvrt(tops, XOF, X, XP, S, N)
            call sinvrt(bots, XOF, X, XP, S, N)
            !
        endif
        !
        topx = seval(tops, X, XP, S, N)
        topy = seval(tops, Y, YP, S, N)
        botx = seval(bots, X, XP, S, N)
        boty = seval(bots, Y, YP, S, N)
        !
        !
        HMOm = 0.
        HFX = 0.
        HFY = 0.
        !
        !---- integrate pressures on top and bottom sides of flap
        do i = 2, N
            if (S(i - 1)<tops .or. S(i)>bots) then
                !
                dx = X(i) - X(i - 1)
                dy = Y(i) - Y(i - 1)
                xmid = 0.5 * (X(i) + X(i - 1)) - XOF
                ymid = 0.5 * (Y(i) + Y(i - 1)) - YOF
                if (LVIsc) then
                    pmid = 0.5 * (CPV(i) + CPV(i - 1))
                else
                    pmid = 0.5 * (CPI(i) + CPI(i - 1))
                endif
                HMOm = HMOm + pmid * (xmid * dx + ymid * dy)
                HFX = HFX - pmid * dy
                HFY = HFY + pmid * dx
            endif
        enddo
        !
        !---- find S(I)..S(I-1) interval containing s=TOPS
        do i = 2, N
            if (S(i)>tops) exit
        enddo
        !
        !---- add on top surface chunk TOPS..S(I-1),  missed in the DO 20 loop.
        dx = topx - X(i - 1)
        dy = topy - Y(i - 1)
        xmid = 0.5 * (topx + X(i - 1)) - XOF
        ymid = 0.5 * (topy + Y(i - 1)) - YOF
        if (S(i)/=S(i - 1)) then
            frac = (tops - S(i - 1)) / (S(i) - S(i - 1))
        else
            frac = 0.
        endif
        if (LVIsc) then
            topp = CPV(i) * frac + CPV(i - 1) * (1.0 - frac)
            pmid = 0.5 * (topp + CPV(i - 1))
        else
            topp = CPI(i) * frac + CPI(i - 1) * (1.0 - frac)
            pmid = 0.5 * (topp + CPI(i - 1))
        endif
        HMOm = HMOm + pmid * (xmid * dx + ymid * dy)
        HFX = HFX - pmid * dy
        HFY = HFY + pmid * dx
        !
        !---- add on inside flap surface contribution from hinge to top surface
        dx = XOF - topx
        dy = YOF - topy
        xmid = 0.5 * (topx + XOF) - XOF
        ymid = 0.5 * (topy + YOF) - YOF
        HMOm = HMOm + pmid * (xmid * dx + ymid * dy)
        HFX = HFX - pmid * dy
        HFY = HFY + pmid * dx
        !
        !---- find S(I)..S(I-1) interval containing s=BOTS
        do i = N, 2, -1
            if (S(i - 1)<bots) exit
        enddo
        !
        !---- add on bottom surface chunk BOTS..S(I),  missed in the DO 20 loop.
        dx = X(i) - botx
        dy = Y(i) - boty
        xmid = 0.5 * (botx + X(i)) - XOF
        ymid = 0.5 * (boty + Y(i)) - YOF
        if (S(i)/=S(i - 1)) then
            frac = (bots - S(i - 1)) / (S(i) - S(i - 1))
        else
            frac = 0.
        endif
        if (LVIsc) then
            botp = CPV(i) * frac + CPV(i - 1) * (1.0 - frac)
            pmid = 0.5 * (botp + CPV(i))
        else
            botp = CPI(i) * frac + CPI(i - 1) * (1.0 - frac)
            pmid = 0.5 * (botp + CPI(i))
        endif
        HMOm = HMOm + pmid * (xmid * dx + ymid * dy)
        HFX = HFX - pmid * dy
        HFY = HFY + pmid * dx
        !
        !---- add on inside flap surface contribution from hinge to bottom surface
        dx = botx - XOF
        dy = boty - YOF
        xmid = 0.5 * (botx + XOF) - XOF
        ymid = 0.5 * (boty + YOF) - YOF
        HMOm = HMOm + pmid * (xmid * dx + ymid * dy)
        HFX = HFX - pmid * dy
        HFY = HFY + pmid * dx
        !
        !---- add on TE base thickness contribution
        dx = X(1) - X(N)
        dy = Y(1) - Y(N)
        xmid = 0.5 * (X(1) + X(N)) - XOF
        ymid = 0.5 * (Y(1) + Y(N)) - YOF
        if (LVIsc) then
            pmid = 0.5 * (CPV(1) + CPV(N))
        else
            pmid = 0.5 * (CPI(1) + CPI(N))
        endif
        HMOm = HMOm + pmid * (xmid * dx + ymid * dy)
        HFX = HFX - pmid * dy
        HFY = HFY + pmid * dx
        !
    end subroutine mhinge
    !*==VPAR.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! MHINGE
end module s_xoper