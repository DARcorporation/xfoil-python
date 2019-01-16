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

!*==I_CIRCLE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module i_circle
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! PARAMETER definitions
    !
    integer, parameter :: ICX = 257, IMX = (ICX - 1) / 4
    !
    ! Local variables
    !
    real :: ag0, agte, dwc, pi, qim0, qimold
    complex :: chordz, dzte, zleold
    complex, dimension(0:IMX) :: cn
    complex, dimension(ICX, 0:IMX) :: eiw
    integer :: mc, mct, nc
    complex, dimension(ICX) :: piq, zc, zcoldw
    real, dimension(ICX) :: sc, scold, wc, xcold, ycold
    complex, dimension(ICX, IMX / 4) :: zc_cn
    !
    !*** End of declarations rewritten by SPAG
    !
    !
    ! PARAMETER definitions
    !
    !
    ! COMMON /CPC01/
    !
    !
    ! COMMON /CPI01/
    !
    !
    ! COMMON /CPR01/
    !
end module i_circle
