! ***********************************************************************
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

!*==HSORT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module m_sort
contains
    subroutine hsort(N, A, Indx)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(*) :: A
        integer, dimension(*) :: Indx
        intent (in) A, N
        intent (inout) Indx
        !
        ! Local variables
        !
        integer :: i, indxt, ir, j, l
        real :: q
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
        !--------------------------------------
        !     Heapsort algorithm.
        !     Returns INDX(.) such that
        !
        !       A(INDX(i)) < A(INDX(i+1))
        !
        !     Stolen from Numerical Recipes.
        !--------------------------------------
        !
        do i = 1, N
            Indx(i) = i
        enddo
        !
        if (N<=1) return
        !
        l = N / 2 + 1
        ir = N
        do
            !
            if (l>1) then
                l = l - 1
                indxt = Indx(l)
                q = A(indxt)
            else
                indxt = Indx(ir)
                q = A(indxt)
                Indx(ir) = Indx(1)
                !
                ir = ir - 1
                if (ir==1) then
                    Indx(1) = indxt
                    return
                endif
            endif
            !
            i = l
            j = l + l
            do
                !
                if (j<=ir) then
                    if (j<ir) then
                        if (A(Indx(j))<A(Indx(j + 1))) j = j + 1
                    endif
                    if (q<A(Indx(j))) then
                        Indx(i) = Indx(j)
                        !
                        i = j
                        j = j + j
                    else
                        j = ir + 1
                    endif
                    cycle
                endif
                !
                Indx(i) = indxt
                goto 100
            enddo
            exit
        100  enddo
    end subroutine hsort
    !*==ASORT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019

    subroutine asort(N, A, Indx, Atmp)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(*) :: A, Atmp
        integer, dimension(*) :: Indx
        intent (in) Indx, N
        intent (inout) A, Atmp
        !
        ! Local variables
        !
        integer :: i, isort
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
        !-----------------------------------------------
        !     Applies sorted index array to reorder A.
        !-----------------------------------------------
        do i = 1, N
            Atmp(i) = A(i)
        enddo
        !
        do i = 1, N
            isort = Indx(i)
            A(i) = Atmp(isort)
        enddo
        !
    end subroutine asort
    !*==REMD.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019

    subroutine remd(N, A, Indx, Tol, Nnew)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N, Nnew
        real :: Tol
        real, dimension(*) :: A
        integer, dimension(*) :: Indx
        intent (in) A, N, Tol
        intent (out) Indx, Nnew
        !
        ! Local variables
        !
        integer :: i, k
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
        !----------------------------------------------------
        !     Sets index array, such that
        !     duplicate A values are left out
        !----------------------------------------------------
        k = 1
        Indx(k) = 1
        !
        do i = 2, N
            if (abs(A(i) - A(i - 1))>Tol) then
                k = k + 1
                Indx(k) = i
            endif
        enddo
        !
        Nnew = k
        !
    end subroutine remd
    !*==SORTDUP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! REMD


    subroutine sortdup(Kk, S, W)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Kk
        real, dimension(Kk) :: S, W
        intent (in) Kk
        intent (inout) S, W
        !
        ! Local variables
        !
        logical :: done
        integer :: ipass, n, np
        real :: temp
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
        !--- Sort arrays in S with no removal of duplicates
        !
        !---- sort arrays
        do ipass = 1, 1234
            done = .true.
            do n = 1, Kk - 1
                np = n + 1
                if (S(np)<S(n)) then
                    temp = S(np)
                    S(np) = S(n)
                    S(n) = temp
                    temp = W(np)
                    W(np) = W(n)
                    W(n) = temp
                    done = .false.
                endif
            enddo
            if (done) goto 99999
        enddo
        if (show_output) write (*, *) 'Sort failed'
        !
    99999 end subroutine sortdup
    !*==FIXDUP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine fixdup(Kk, S, W)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Kk
        real, dimension(Kk) :: S, W
        intent (inout) Kk, S, W
        !
        ! Local variables
        !
        logical :: done
        integer :: i, n
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
        do
            !
            !*** End of declarations rewritten by SPAG
            !
            !--- Check arrays in S by removing leading and ending duplicates
            !    eliminate extra duplicates (more than one duplicate point) elsewhere
            !
            done = .true.

            !---- Check first elements for dups
            if (S(2)==S(1)) then
                do n = 1, Kk - 1
                    S(n) = S(n + 1)
                    W(n) = W(n + 1)
                enddo
                Kk = Kk - 1
                done = .false.
            endif
            !
            !---- Check last elements for dups
            if (S(Kk)==S(Kk - 1)) then
                S(Kk - 1) = S(Kk)
                W(Kk - 1) = W(Kk)
                Kk = Kk - 1
                done = .false.
            endif
            do
                !
                !--- Eliminate more than 2 succeeding identical elements
                do n = 1, Kk - 2
                    if (S(n)==S(n + 1) .and. S(n)==S(n + 2)) then
                        do i = n, Kk - 1
                            S(i) = S(i + 1)
                            W(i) = W(i + 1)
                        enddo
                        Kk = Kk - 1
                        goto 50
                    endif
                enddo
                !
                if (.not.(done)) goto 100
                return
            50   enddo
            exit
        100  enddo

    end subroutine fixdup
    !*==SORT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! FIXDUP



    subroutine sort(Kk, S, W)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Kk
        real, dimension(Kk) :: S, W
        intent (inout) Kk, S, W
        !
        ! Local variables
        !
        logical :: done
        integer :: ipass, k, kks, kt, n, np
        real :: temp
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
        !---- sort arrays
        do ipass = 1, 1234
            done = .true.
            do n = 1, Kk - 1
                np = n + 1
                if (S(np)<S(n)) then
                    temp = S(np)
                    S(np) = S(n)
                    S(n) = temp
                    temp = W(np)
                    W(np) = W(n)
                    W(n) = temp
                    done = .false.
                endif
            enddo
            if (done) goto 100
        enddo
        if (show_output) write (*, *) 'Sort failed'
        !
        !---- search for duplicate pairs and eliminate each one
        100  kks = Kk
        do k = 1, kks
            if (k>=Kk) return
            if (S(k)==S(k + 1)) then
                !------- eliminate pair
                Kk = Kk - 2
                do kt = k, Kk
                    S(kt) = S(kt + 2)
                    W(kt) = W(kt + 2)
                enddo
            endif
        enddo
        !
    end subroutine sort
    !*==SORTOL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine sortol(Tol, Kk, S, W)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Kk
        real :: Tol
        real, dimension(Kk) :: S, W
        intent (in) Tol
        intent (inout) Kk, S, W
        !
        ! Local variables
        !
        logical :: done
        real :: dsq, temp
        integer :: ipass, k, kks, kt, n, np
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
        !---- sort arrays
        do ipass = 1, 1234
            done = .true.
            do n = 1, Kk - 1
                np = n + 1
                if (S(np)<S(n)) then
                    temp = S(np)
                    S(np) = S(n)
                    S(n) = temp
                    temp = W(np)
                    W(np) = W(n)
                    W(n) = temp
                    done = .false.
                endif
            enddo
            if (done) goto 100
        enddo
        if (show_output) write (*, *) 'Sort failed'
        100  do
            !
            !---- search for near-duplicate pairs and eliminate extra points
            !---- Modified 4/24/01 HHY to check list until ALL duplicates removed
            !     This cures a bug for sharp LE foils where there were 3 LE points in
            !     camber, thickness lists from GETCAM.
            !
            kks = Kk
            done = .true.
            do k = 1, kks
                if (k<Kk) then
                    dsq = (S(k) - S(k + 1))**2 + (W(k) - W(k + 1))**2
                    if (dsq<Tol * Tol) then
                        !------- eliminate extra point pairs
                        !cc         write(*,*) 'extra on point ',k,kks
                        Kk = Kk - 1
                        do kt = k + 1, Kk
                            S(kt) = S(kt + 1)
                            W(kt) = W(kt + 1)
                        enddo
                        done = .false.
                    endif
                endif
            enddo
            if (done) exit
        enddo
        !
    end subroutine sortol

end module m_sort