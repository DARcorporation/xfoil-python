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

!*==M_XUTILS.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module m_xutils
    implicit none
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
contains
    subroutine setexp(S, Ds1, Smax, Nn)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Ds1, Smax
        integer :: Nn
        real, dimension(Nn) :: S
        intent (in) Ds1, Nn, Smax
        intent (inout) S
        !
        ! Local variables
        !
        real :: aaa, bbb, ccc, disc, dratio, dresdr, ds, ratio, res, rnex, rni, sigma, sigman
        integer :: iter, n, nex
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
        !........................................................
        !     Sets geometrically stretched array S:
        !
        !       S(i+1) - S(i)  =  r * [S(i) - S(i-1)]
        !
        !       S     (output)  array to be set
        !       DS1   (input)   first S increment:  S(2) - S(1)
        !       SMAX  (input)   final S value:      S(NN)
        !       NN    (input)   number of points
        !........................................................
        !
        sigma = Smax / Ds1
        nex = Nn - 1
        rnex = float(nex)
        rni = 1.0 / rnex
        !
        !---- solve quadratic for initial geometric ratio guess
        aaa = rnex * (rnex - 1.0) * (rnex - 2.0) / 6.0
        bbb = rnex * (rnex - 1.0) / 2.0
        ccc = rnex - sigma
        !
        disc = bbb**2 - 4.0 * aaa * ccc
        disc = max(0.0, disc)
        !
        if (nex<=1) then
            stop 'SETEXP: Cannot fill array.  N too small.'
        elseif (nex==2) then
            ratio = -ccc / bbb + 1.0
        else
            ratio = (-bbb + sqrt(disc)) / (2.0 * aaa) + 1.0
        endif
        !
        if (ratio/=1.0) then
            !
            !---- Newton iteration for actual geometric ratio
            do iter = 1, 100
                sigman = (ratio**nex - 1.0) / (ratio - 1.0)
                res = sigman**rni - sigma**rni
                dresdr = rni * sigman**rni * (rnex * ratio**(nex - 1) - sigman) / (ratio**nex - 1.0)
                !
                dratio = -res / dresdr
                ratio = ratio + dratio
                !
                if (abs(dratio)<1.0E-5) goto 100
                !
            enddo
            if (show_output) write (*, *) 'SETEXP: Convergence failed.  Continuing anyway ...'
        endif
        !
        !---- set up stretched array using converged geometric ratio
        100  S(1) = 0.0
        ds = Ds1
        do n = 2, Nn
            S(n) = S(n - 1) + ds
            ds = ds * ratio
        enddo
        !
    end subroutine setexp


    function atanc(Y, X, Thold)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Thold, X, Y
        real :: atanc
        intent (in) Thold, X, Y
        !
        ! Local variables
        !
        real :: dtcorr, dthet, thnew
        real, save :: pi, tpi
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
        !---------------------------------------------------------------
        !     ATAN2 function with branch cut checking.
        !
        !     Increments position angle of point X,Y from some previous
        !     value THOLD due to a change in position, ensuring that the
        !     position change does not cross the ATAN2 branch cut
        !     (which is in the -x direction).  For example:
        !
        !       ATANC( -1.0 , -1.0 , 0.75*pi )  returns  1.25*pi , whereas
        !       ATAN2( -1.0 , -1.0 )            returns  -.75*pi .
        !
        !     Typically, ATANC is used to fill an array of angles:
        !
        !        THETA(1) = ATAN2( Y(1) , X(1) )
        !        DO i=2, N
        !          THETA(i) = ATANC( Y(i) , X(i) , THETA(i-1) )
        !        END DO
        !
        !     This will prevent the angle array THETA(i) from jumping by
        !     +/- 2 pi when the path X(i),Y(i) crosses the negative x axis.
        !
        !     Input:
        !       X,Y     point position coordinates
        !       THOLD   position angle of nearby point
        !
        !     Output:
        !       ATANC   position angle of X,Y
        !---------------------------------------------------------------
        data pi/3.1415926535897932384/
        data tpi/6.2831853071795864769/
        !
        !---- set new position angle, ignoring branch cut in ATAN2 function for now
        thnew = atan2(Y, X)
        dthet = thnew - Thold
        !
        !---- angle change cannot exceed +/- pi, so get rid of any multiples of 2 pi
        dtcorr = dthet - tpi * int((dthet + sign(pi, dthet)) / tpi)
        !
        !---- set correct new angle
        atanc = Thold + dtcorr
        !
    end function atanc
    ! ATANC
end module m_xutils
