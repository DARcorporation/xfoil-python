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

!*==GAUSS.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module m_xsolve
contains
    subroutine gauss(Nsiz, Nn, Z, R, Nrhs)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Nn, Nrhs, Nsiz
        real, dimension(Nsiz, Nrhs) :: R
        real, dimension(Nsiz, Nsiz) :: Z
        intent (in) Nn, Nrhs, Nsiz
        intent (inout) R, Z
        !
        ! Local variables
        !
        integer :: k, l, n, np, np1, nx
        real :: pivot, temp, ztmp
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
        !     *******************************************************
        !     *                                                     *
        !     *   Solves general NxN system in NN unknowns          *
        !     *    with arbitrary number (NRHS) of righthand sides. *
        !     *   Assumes system is invertible...                   *
        !     *    ...if it isn't, a divide by zero will result.    *
        !     *                                                     *
        !     *   Z is the coefficient matrix...                    *
        !     *     ...destroyed during solution process.           *
        !     *   R is the righthand side(s)...                     *
        !     *     ...replaced by the solution vector(s).          *
        !     *                                                     *
        !     *                              Mark Drela  1984       *
        !     *******************************************************
        !
        !
        do np = 1, Nn - 1
            np1 = np + 1
            !
            !------ find max pivot index NX
            nx = np
            do n = np1, Nn
                if (abs(Z(n, np))>abs(Z(nx, np))) nx = n
            enddo
            !
            pivot = 1.0 / Z(nx, np)
            !
            !------ switch pivots
            Z(nx, np) = Z(np, np)
            !
            !------ switch rows & normalize pivot row
            do l = np1, Nn
                temp = Z(nx, l) * pivot
                Z(nx, l) = Z(np, l)
                Z(np, l) = temp
            enddo
            !
            do l = 1, Nrhs
                temp = R(nx, l) * pivot
                R(nx, l) = R(np, l)
                R(np, l) = temp
            enddo
            !
            !------ forward eliminate everything
            do k = np1, Nn
                ztmp = Z(k, np)
                !
                !          IF(ZTMP.EQ.0.0) GO TO 15
                !
                do l = np1, Nn
                    Z(k, l) = Z(k, l) - ztmp * Z(np, l)
                enddo
                do l = 1, Nrhs
                    R(k, l) = R(k, l) - ztmp * R(np, l)
                enddo
            enddo
            !
        enddo
        !
        !---- solve for last row
        do l = 1, Nrhs
            R(Nn, l) = R(Nn, l) / Z(Nn, Nn)
        enddo
        !
        !---- back substitute everything
        do np = Nn - 1, 1, -1
            np1 = np + 1
            do l = 1, Nrhs
                do k = np1, Nn
                    R(np, l) = R(np, l) - Z(np, k) * R(k, l)
                enddo
            enddo
        enddo
        !
    end subroutine gauss
    !*==CGAUSS.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! GAUSS


    subroutine cgauss(Nsiz, Nn, Z, R, Nrhs)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Nn, Nrhs, Nsiz
        complex, dimension(Nsiz, Nrhs) :: R
        complex, dimension(Nsiz, Nsiz) :: Z
        intent (in) Nn, Nrhs, Nsiz
        intent (inout) R, Z
        !
        ! Local variables
        !
        integer :: k, l, n, np, np1, nx
        complex :: pivot, temp, ztmp
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
        !********************************************
        !     Solves general complex linear systems.
        !********************************************
        !
        do np = 1, Nn - 1
            np1 = np + 1
            !
            !------ find max pivot index NX
            nx = np
            do n = np1, Nn
                if (abs(Z(n, np))>abs(Z(nx, np))) nx = n
            enddo
            !
            pivot = (1.0, 0.0) / Z(nx, np)
            !
            !------ switch pivots
            Z(nx, np) = Z(np, np)
            !
            !------ switch rows & normalize pivot row
            do l = np1, Nn
                temp = Z(nx, l) * pivot
                Z(nx, l) = Z(np, l)
                Z(np, l) = temp
            enddo
            !
            do l = 1, Nrhs
                temp = R(nx, l) * pivot
                R(nx, l) = R(np, l)
                R(np, l) = temp
            enddo
            !
            !------ forward eliminate everything
            do k = np1, Nn
                ztmp = Z(k, np)
                !
                !          IF(ZTMP.EQ.0.0) GO TO 15
                !
                do l = np1, Nn
                    Z(k, l) = Z(k, l) - ztmp * Z(np, l)
                enddo
                do l = 1, Nrhs
                    R(k, l) = R(k, l) - ztmp * R(np, l)
                enddo
            enddo
            !
        enddo
        !
        !---- solve for last row
        do l = 1, Nrhs
            R(Nn, l) = R(Nn, l) / Z(Nn, Nn)
        enddo
        !
        !---- back substitute everything
        do np = Nn - 1, 1, -1
            np1 = np + 1
            do l = 1, Nrhs
                do k = np1, Nn
                    R(np, l) = R(np, l) - Z(np, k) * R(k, l)
                enddo
            enddo
        enddo
        !
    end subroutine cgauss
    !*==LUDCMP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! CGAUSS



    subroutine ludcmp(Nsiz, N, A, Indx)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        integer, parameter :: NVX = 500
        !
        ! Dummy arguments
        !
        integer :: N, Nsiz
        real, dimension(Nsiz, Nsiz) :: A
        integer, dimension(Nsiz) :: Indx
        intent (in) N, Nsiz
        intent (out) Indx
        intent (inout) A
        !
        ! Local variables
        !
        real :: aamax, dum, sum
        integer :: i, imax, j, k
        real, dimension(NVX) :: vv
        !
        !*** End of declarations rewritten by SPAG
        !
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! PARAMETER definitions
        !
        !
        ! Dummy arguments
        !
        !
        ! Local variables
        !
        !
        !*** End of declarations rewritten by SPAG
        !
        !     *******************************************************
        !     *                                                     *
        !     *   Factors a full NxN matrix into an LU form.        *
        !     *   Subr. BAKSUB can back-substitute it with some RHS.*
        !     *   Assumes matrix is non-singular...                 *
        !     *    ...if it isn't, a divide by zero will result.    *
        !     *                                                     *
        !     *   A is the matrix...                                *
        !     *     ...replaced with its LU factors.                *
        !     *                                                     *
        !     *                              Mark Drela  1988       *
        !     *******************************************************
        !
        !
        !
        if (N>NVX) stop 'LUDCMP: Array overflow. Increase NVX.'
        !
        do i = 1, N
            aamax = 0.
            do j = 1, N
                aamax = max(abs(A(i, j)), aamax)
            enddo
            vv(i) = 1.0 / aamax
        enddo
        !
        do j = 1, N
            do i = 1, j - 1
                sum = A(i, j)
                do k = 1, i - 1
                    sum = sum - A(i, k) * A(k, j)
                enddo
                A(i, j) = sum
            enddo
            !
            aamax = 0.
            do i = j, N
                sum = A(i, j)
                do k = 1, j - 1
                    sum = sum - A(i, k) * A(k, j)
                enddo
                A(i, j) = sum
                !
                dum = vv(i) * abs(sum)
                if (dum>=aamax) then
                    imax = i
                    aamax = dum
                endif
            enddo
            !
            if (j/=imax) then
                do k = 1, N
                    dum = A(imax, k)
                    A(imax, k) = A(j, k)
                    A(j, k) = dum
                enddo
                vv(imax) = vv(j)
            endif
            !
            Indx(j) = imax
            if (j/=N) then
                dum = 1.0 / A(j, j)
                do i = j + 1, N
                    A(i, j) = A(i, j) * dum
                enddo
            endif
            !
        enddo
        !
    end subroutine ludcmp
    !*==BAKSUB.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! LUDCMP


    subroutine baksub(Nsiz, N, A, Indx, B)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N, Nsiz
        real, dimension(Nsiz, Nsiz) :: A
        real, dimension(Nsiz) :: B
        integer, dimension(Nsiz) :: Indx
        intent (in) A, Indx, N, Nsiz
        intent (inout) B
        !
        ! Local variables
        !
        integer :: i, ii, j, ll
        real :: sum
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
        ii = 0
        do i = 1, N
            ll = Indx(i)
            sum = B(ll)
            B(ll) = B(i)
            if (ii/=0) then
                do j = ii, i - 1
                    sum = sum - A(i, j) * B(j)
                enddo
            elseif (sum/=0.0) then
                ii = i
            endif
            B(i) = sum
        enddo
        !
        do i = N, 1, -1
            sum = B(i)
            if (i<N) then
                do j = i + 1, N
                    sum = sum - A(i, j) * B(j)
                enddo
            endif
            B(i) = sum / A(i, i)
        enddo
        !
    end subroutine baksub
    !*==BLSOLV.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! BAKSUB



    subroutine blsolv
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: iv, ivp, ivte1, ivz, k, kv, l
        real :: pivot, vacc1, vacc2, vacc3, vtmp, vtmp1, vtmp2, vtmp3
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
        !-----------------------------------------------------------------
        !      Custom solver for coupled viscous-inviscid Newton system:
        !
        !        A  |  |  .  |  |  .  |    d       R       S
        !        B  A  |  .  |  |  .  |    d       R       S
        !        |  B  A  .  |  |  .  |    d       R       S
        !        .  .  .  .  |  |  .  |    d   =   R - dRe S
        !        |  |  |  B  A  |  .  |    d       R       S
        !        |  Z  |  |  B  A  .  |    d       R       S
        !        .  .  .  .  .  .  .  |    d       R       S
        !        |  |  |  |  |  |  B  A    d       R       S
        !
        !       A, B, Z  3x3  blocks containing linearized BL equation coefficients
        !       |        3x1  vectors containing mass defect influence
        !                     coefficients on Ue
        !       d        3x1  unknown vectors (Newton deltas for Ctau, Theta, m)
        !       R        3x1  residual vectors
        !       S        3x1  Re influence vectors
        !-----------------------------------------------------------------
        !
        ivte1 = ISYs(IBLte(1), 1)
        !
        vacc1 = VACcel
        vacc2 = VACcel * 2.0 / (S(N) - S(1))
        vacc3 = VACcel * 2.0 / (S(N) - S(1))
        !
        do iv = 1, NSYs
            !
            ivp = iv + 1
            !
            !====== Invert VA(IV) block
            !
            !------ normalize first row
            pivot = 1.0 / VA(1, 1, iv)
            VA(1, 2, iv) = VA(1, 2, iv) * pivot
            do l = iv, NSYs
                VM(1, l, iv) = VM(1, l, iv) * pivot
            enddo
            VDEl(1, 1, iv) = VDEl(1, 1, iv) * pivot
            VDEl(1, 2, iv) = VDEl(1, 2, iv) * pivot
            !
            !------ eliminate lower first column in VA block
            do k = 2, 3
                vtmp = VA(k, 1, iv)
                VA(k, 2, iv) = VA(k, 2, iv) - vtmp * VA(1, 2, iv)
                do l = iv, NSYs
                    VM(k, l, iv) = VM(k, l, iv) - vtmp * VM(1, l, iv)
                enddo
                VDEl(k, 1, iv) = VDEl(k, 1, iv) - vtmp * VDEl(1, 1, iv)
                VDEl(k, 2, iv) = VDEl(k, 2, iv) - vtmp * VDEl(1, 2, iv)
            enddo
            !
            !
            !------ normalize second row
            pivot = 1.0 / VA(2, 2, iv)
            do l = iv, NSYs
                VM(2, l, iv) = VM(2, l, iv) * pivot
            enddo
            VDEl(2, 1, iv) = VDEl(2, 1, iv) * pivot
            VDEl(2, 2, iv) = VDEl(2, 2, iv) * pivot
            !
            !------ eliminate lower second column in VA block
            k = 3
            vtmp = VA(k, 2, iv)
            do l = iv, NSYs
                VM(k, l, iv) = VM(k, l, iv) - vtmp * VM(2, l, iv)
            enddo
            VDEl(k, 1, iv) = VDEl(k, 1, iv) - vtmp * VDEl(2, 1, iv)
            VDEl(k, 2, iv) = VDEl(k, 2, iv) - vtmp * VDEl(2, 2, iv)
            !
            !
            !------ normalize third row
            pivot = 1.0 / VM(3, iv, iv)
            do l = ivp, NSYs
                VM(3, l, iv) = VM(3, l, iv) * pivot
            enddo
            VDEl(3, 1, iv) = VDEl(3, 1, iv) * pivot
            VDEl(3, 2, iv) = VDEl(3, 2, iv) * pivot
            !
            !
            !------ eliminate upper third column in VA block
            vtmp1 = VM(1, iv, iv)
            vtmp2 = VM(2, iv, iv)
            do l = ivp, NSYs
                VM(1, l, iv) = VM(1, l, iv) - vtmp1 * VM(3, l, iv)
                VM(2, l, iv) = VM(2, l, iv) - vtmp2 * VM(3, l, iv)
            enddo
            VDEl(1, 1, iv) = VDEl(1, 1, iv) - vtmp1 * VDEl(3, 1, iv)
            VDEl(2, 1, iv) = VDEl(2, 1, iv) - vtmp2 * VDEl(3, 1, iv)
            VDEl(1, 2, iv) = VDEl(1, 2, iv) - vtmp1 * VDEl(3, 2, iv)
            VDEl(2, 2, iv) = VDEl(2, 2, iv) - vtmp2 * VDEl(3, 2, iv)
            !
            !------ eliminate upper second column in VA block
            vtmp = VA(1, 2, iv)
            do l = ivp, NSYs
                VM(1, l, iv) = VM(1, l, iv) - vtmp * VM(2, l, iv)
            enddo
            VDEl(1, 1, iv) = VDEl(1, 1, iv) - vtmp * VDEl(2, 1, iv)
            VDEl(1, 2, iv) = VDEl(1, 2, iv) - vtmp * VDEl(2, 2, iv)
            !
            !
            if (iv/=NSYs) then
                !
                !====== Eliminate VB(IV+1) block, rows  1 -> 3
                do k = 1, 3
                    vtmp1 = VB(k, 1, ivp)
                    vtmp2 = VB(k, 2, ivp)
                    vtmp3 = VM(k, iv, ivp)
                    do l = ivp, NSYs
                        VM(k, l, ivp) = VM(k, l, ivp) - (vtmp1 * VM(1, l, iv) &
                                + vtmp2 * VM(2, l, iv) + vtmp3 * VM(3, l, iv))
                    enddo
                    VDEl(k, 1, ivp) = VDEl(k, 1, ivp) &
                            - (vtmp1 * VDEl(1, 1, iv) + vtmp2 * VDEl(2, 1, iv) + vtmp3 * VDEl(3, 1, iv))
                    VDEl(k, 2, ivp) = VDEl(k, 2, ivp) &
                            - (vtmp1 * VDEl(1, 2, iv) + vtmp2 * VDEl(2, 2, iv) + vtmp3 * VDEl(3, 2, iv))
                enddo
                !
                if (iv==ivte1) then
                    !------- eliminate VZ block
                    ivz = ISYs(IBLte(2) + 1, 2)
                    !
                    do k = 1, 3
                        vtmp1 = VZ(k, 1)
                        vtmp2 = VZ(k, 2)
                        do l = ivp, NSYs
                            VM(k, l, ivz) = VM(k, l, ivz) - (vtmp1 * VM(1, l, iv) + vtmp2 * VM(2, l, iv))
                        enddo
                        VDEl(k, 1, ivz) = VDEl(k, 1, ivz) - (vtmp1 * VDEl(1, 1, iv) + vtmp2 * VDEl(2, 1, iv))
                        VDEl(k, 2, ivz) = VDEl(k, 2, ivz) - (vtmp1 * VDEl(1, 2, iv) + vtmp2 * VDEl(2, 2, iv))
                    enddo
                endif
                !
                if (ivp/=NSYs) then
                    !
                    !====== Eliminate lower VM column
                    do kv = iv + 2, NSYs
                        vtmp1 = VM(1, iv, kv)
                        vtmp2 = VM(2, iv, kv)
                        vtmp3 = VM(3, iv, kv)
                        !
                        if (abs(vtmp1)>vacc1) then
                            do l = ivp, NSYs
                                VM(1, l, kv) = VM(1, l, kv) - vtmp1 * VM(3, l, iv)
                            enddo
                            VDEl(1, 1, kv) = VDEl(1, 1, kv) - vtmp1 * VDEl(3, 1, iv)
                            VDEl(1, 2, kv) = VDEl(1, 2, kv) - vtmp1 * VDEl(3, 2, iv)
                        endif
                        !
                        if (abs(vtmp2)>vacc2) then
                            do l = ivp, NSYs
                                VM(2, l, kv) = VM(2, l, kv) - vtmp2 * VM(3, l, iv)
                            enddo
                            VDEl(2, 1, kv) = VDEl(2, 1, kv) - vtmp2 * VDEl(3, 1, iv)
                            VDEl(2, 2, kv) = VDEl(2, 2, kv) - vtmp2 * VDEl(3, 2, iv)
                        endif
                        !
                        if (abs(vtmp3)>vacc3) then
                            do l = ivp, NSYs
                                VM(3, l, kv) = VM(3, l, kv) - vtmp3 * VM(3, l, iv)
                            enddo
                            VDEl(3, 1, kv) = VDEl(3, 1, kv) - vtmp3 * VDEl(3, 1, iv)
                            VDEl(3, 2, kv) = VDEl(3, 2, kv) - vtmp3 * VDEl(3, 2, iv)
                        endif
                        !
                    enddo
                endif
            endif
            !
        enddo
        !
        !
        !
        do iv = NSYs, 2, -1
            !
            !------ eliminate upper VM columns
            vtmp = VDEl(3, 1, iv)
            do kv = iv - 1, 1, -1
                VDEl(1, 1, kv) = VDEl(1, 1, kv) - VM(1, iv, kv) * vtmp
                VDEl(2, 1, kv) = VDEl(2, 1, kv) - VM(2, iv, kv) * vtmp
                VDEl(3, 1, kv) = VDEl(3, 1, kv) - VM(3, iv, kv) * vtmp
            enddo
            !
            vtmp = VDEl(3, 2, iv)
            do kv = iv - 1, 1, -1
                VDEl(1, 2, kv) = VDEl(1, 2, kv) - VM(1, iv, kv) * vtmp
                VDEl(2, 2, kv) = VDEl(2, 2, kv) - VM(2, iv, kv) * vtmp
                VDEl(3, 2, kv) = VDEl(3, 2, kv) - VM(3, iv, kv) * vtmp
            enddo
            !
        enddo
        !
    end subroutine blsolv

end module m_xsolve