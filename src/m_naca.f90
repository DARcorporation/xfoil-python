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

!*==NACA4.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! POLREF
module m_naca
contains
    subroutine naca4(Ides, Xx, Yt, Yc, Nside, Xb, Yb, Nb, Name)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ides, Nb, Nside
        character(*) :: Name
        real, dimension(2 * Nside) :: Xb, Yb
        real, dimension(Nside) :: Xx, Yc, Yt
        intent (in) Ides, Nside
        intent (out) Name, Nb, Xb, Yb
        intent (inout) Xx, Yc, Yt
        !
        ! Local variables
        !
        real, save :: an
        real :: anp, frac, m, p, t
        character(10), save :: digits
        integer :: i, ib, n1, n2, n3, n4
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
        data digits/'0123456789'/
        !
        !---- TE point bunching parameter
        data an/1.5/
        !
        n4 = Ides / 1000
        n3 = (Ides - n4 * 1000) / 100
        n2 = (Ides - n4 * 1000 - n3 * 100) / 10
        n1 = (Ides - n4 * 1000 - n3 * 100 - n2 * 10)
        !
        m = float(n4) / 100.0
        p = float(n3) / 10.0
        t = float(n2 * 10 + n1) / 100.0
        !
        anp = an + 1.0
        do i = 1, Nside
            frac = float(i - 1) / float(Nside - 1)
            if (i==Nside) then
                Xx(i) = 1.0
            else
                Xx(i) = 1.0 - anp * frac * (1.0 - frac)**an - (1.0 - frac)**anp
            endif
            Yt(i) = (0.29690 * sqrt(Xx(i)) - 0.12600 * Xx(i) &
                    - 0.35160 * Xx(i)**2 + 0.28430 * Xx(i)**3 - 0.10150 * Xx(i)**4) * t / 0.20
            if (Xx(i)<p) then
                Yc(i) = m / p**2 * (2.0 * p * Xx(i) - Xx(i)**2)
            else
                Yc(i) = m / (1.0 - p)**2 * ((1.0 - 2.0 * p) + 2.0 * p * Xx(i) - Xx(i)**2)
            endif
        enddo
        !
        ib = 0
        do i = Nside, 1, -1
            ib = ib + 1
            Xb(ib) = Xx(i)
            Yb(ib) = Yc(i) + Yt(i)
        enddo
        do i = 2, Nside
            ib = ib + 1
            Xb(ib) = Xx(i)
            Yb(ib) = Yc(i) - Yt(i)
        enddo
        Nb = ib
        !
        Name = 'NACA'
        Name(6:9) = digits(n4 + 1:n4 + 1) // digits(n3 + 1:n3 + 1) // digits(n2 + 1:n2 + 1) // digits(n1 + 1:n1 + 1)
        !
    end subroutine naca4
    !*==NACA5.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine naca5(Ides, Xx, Yt, Yc, Nside, Xb, Yb, Nb, Name)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ides, Nb, Nside
        character(*) :: Name
        real, dimension(2 * Nside) :: Xb, Yb
        real, dimension(Nside) :: Xx, Yc, Yt
        intent (in) Nside
        intent (out) Name, Nb, Xb, Yb
        intent (inout) Ides, Xx, Yc, Yt
        !
        ! Local variables
        !
        real, save :: an
        real :: anp, c, frac, m, t
        character(10), save :: digits
        integer :: i, ib, n1, n2, n3, n4, n5, n543
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
        data digits/'0123456789'/
        !
        !---- TE point bunching parameter
        data an/1.5/
        !
        n5 = Ides / 10000
        n4 = (Ides - n5 * 10000) / 1000
        n3 = (Ides - n5 * 10000 - n4 * 1000) / 100
        n2 = (Ides - n5 * 10000 - n4 * 1000 - n3 * 100) / 10
        n1 = (Ides - n5 * 10000 - n4 * 1000 - n3 * 100 - n2 * 10)
        !
        n543 = 100 * n5 + 10 * n4 + n3
        !
        if (n543==210) then
            !c     P = 0.05
            m = 0.0580
            c = 361.4
        elseif (n543==220) then
            !c     P = 0.10
            m = 0.1260
            c = 51.64
        elseif (n543==230) then
            !c     P = 0.15
            m = 0.2025
            c = 15.957
        elseif (n543==240) then
            !c     P = 0.20
            m = 0.2900
            c = 6.643
        elseif (n543==250) then
            !c     P = 0.25
            m = 0.3910
            c = 3.230
        else
            if (show_output) then
                write (*, *) 'Illegal 5-digit designation'
                write (*, *) 'First three digits must be 210, 220, ... 250'
            endif
            Ides = 0
            return
        endif
        !
        !
        t = float(n2 * 10 + n1) / 100.0
        !
        anp = an + 1.0
        do i = 1, Nside
            frac = float(i - 1) / float(Nside - 1)
            if (i==Nside) then
                Xx(i) = 1.0
            else
                Xx(i) = 1.0 - anp * frac * (1.0 - frac)**an - (1.0 - frac)**anp
            endif
            !
            Yt(i) = (0.29690 * sqrt(Xx(i)) - 0.12600 * Xx(i) - 0.35160 * Xx(i)**2 &
                    + 0.28430 * Xx(i)**3 - 0.10150 * Xx(i)**4) * t / 0.20
            if (Xx(i)<m) then
                Yc(i) = (c / 6.0) * (Xx(i)**3 - 3.0 * m * Xx(i)**2 + m * m * (3.0 - m) * Xx(i))
            else
                Yc(i) = (c / 6.0) * m**3 * (1.0 - Xx(i))
            endif
        enddo
        !
        ib = 0
        do i = Nside, 1, -1
            ib = ib + 1
            Xb(ib) = Xx(i)
            Yb(ib) = Yc(i) + Yt(i)
        enddo
        do i = 2, Nside
            ib = ib + 1
            Xb(ib) = Xx(i)
            Yb(ib) = Yc(i) - Yt(i)
        enddo
        Nb = ib
        !
        Name = 'NACA'
        Name(6:10) = digits(n5 + 1:n5 + 1) // &
                digits(n4 + 1:n4 + 1) // &
                digits(n3 + 1:n3 + 1) // &
                digits(n2 + 1:n2 + 1) // &
                digits(n1 + 1:n1 + 1)
        !
    end subroutine naca5

end module m_naca