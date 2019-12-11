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

!*==ABCOPY.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! INTX
!
module m_xgdes
contains
    subroutine abcopy(Lconf)
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
        logical :: Lconf
        intent (in) Lconf
        !
        ! Local variables
        !
        integer :: i, j
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
        if (NB<=1) then
            if (show_output) write (*, *) 'ABCOPY: Buffer airfoil not available.'
            return
        elseif (NB>IQX - 5) then
            if (show_output) then
                write (*, *) 'Maximum number of panel nodes  : ', IQX - 5
                write (*, *) 'Number of buffer airfoil points: ', NB
                write (*, *) 'Current airfoil cannot be set.'
                write (*, *) 'Try executing PANE at Top Level instead.'
            endif
            return
        endif
        if (N/=NB) LBLini = .false.
        !
        N = NB
        do i = 1, N
            X(i) = XB(i)
            Y(i) = YB(i)
        enddo
        LGSame = .true.
        !
        if (LBFlap) then
            XOF = XBF
            YOF = YBF
            LFLap = .true.
        endif
        !
        !---- strip out doubled points
        i = 1
        do
            i = i + 1
            if (X(i - 1)==X(i) .and. Y(i - 1)==Y(i)) then
                do j = i, N - 1
                    X(j) = X(j + 1)
                    Y(j) = Y(j + 1)
                enddo
                N = N - 1
            endif
            if (i>=N) then
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
                !CC      LBLINI = .FALSE.
                !
                if (Lconf .and. show_output) write (*, 99001) N
                99001  format (/' Current airfoil nodes set from buffer airfoil nodes (', i4, ' )')
                exit
            endif
        enddo
        !
    end subroutine abcopy
    !*==GETXYF.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! ABCOPY


    subroutine getxyf(X, Xp, Y, Yp, S, N, Tops, Bots, Xf, Yf)
        use i_xfoil, only: show_output
        use m_userio, only: askr
        use m_spline, only: seval, sinvrt
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Bots, Tops, Xf, Yf
        integer :: N
        real, dimension(N) :: S, X, Xp, Y, Yp
        intent (in) Y, Yp
        intent (inout) Bots, Tops, Yf
        !
        ! Local variables
        !
        real :: boty, topy, yrel
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
        if (Xf==-999.0) call askr('Enter flap hinge x location^', Xf)
        !
        !---- find top and bottom y at hinge x location
        Tops = S(1) + (X(1) - Xf)
        Bots = S(N) - (X(N) - Xf)
        call sinvrt(Tops, Xf, X, Xp, S, N)
        call sinvrt(Bots, Xf, X, Xp, S, N)
        topy = seval(Tops, Y, Yp, S, N)
        boty = seval(Bots, Y, Yp, S, N)
        !
        if (show_output) write (*, 99001) topy, boty
        99001 format (/'  Top    surface:  y =', f8.4, '     y/t = 1.0'/'  Bottom surface:  y =', f8.4, '     y/t = 0.0')
        !
        if (Yf==-999.0) call askr('Enter flap hinge y location (or 999 to specify y/t)^', Yf)
        !
        if (Yf==999.0) then
            call askr('Enter flap hinge relative y/t location^', yrel)
            Yf = topy * yrel + boty * (1.0 - yrel)
        endif
        !
    end subroutine getxyf

end module m_xgdes