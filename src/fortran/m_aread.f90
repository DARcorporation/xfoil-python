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

!*==AREAD.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module m_aread
contains
    subroutine aread(Lu, Fname, Nmax, X, Y, N, Name, Ispars, Itype, Info)
        use i_xfoil, only: show_output
        use m_userio, only: getflt, aski
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Fname, Ispars, Name
        integer :: Info, Itype, Lu, N, Nmax
        real, dimension(Nmax) :: X, Y
        intent (in) Fname, Info, Lu, Nmax
        intent (out) Ispars, Itype, N
        intent (inout) Name, X, Y
        !
        ! Local variables
        !
        real, dimension(10) :: a
        logical :: error, lopen
        integer :: i, iel, na, nel, nfn
        character(80) :: line, line1, line2
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
        !--------------------------------------------------------
        !     Reads in several types of airfoil coordinate file.
        !
        !  Input:
        !       LU      logical unit to use for reading
        !       FNAME   name of coordinate file to be read,
        !               if FNAME(1:1).eq.' ', unit LU is assumed
        !               to be already open
        !       INFO   0 keep quiet
        !              1 print info on airfoil
        !  Output:
        !       X,Y     coordinates
        !       N       number of X,Y coordinates
        !       NAME    character name string        (if ITYPE > 1)
        !       ISPARS  ISES/MSES domain-size string (if ITYPE > 2)
        !       ITYPE returns type of file:
        !           0  None.  Read error occurred.
        !           1  Generic.
        !           2  Labeled generic.
        !           3  MSES single element.
        !           4  MSES multi-element.
        !--------------------------------------------------------
        !
        iel = 0
        nel = 0
        !
        !---- assume read error will occur
        Itype = 0
        !
        lopen = Fname(1:1)/=' '
        if (lopen) open (Lu, file = Fname, status = 'OLD', err = 200)
        do
            !
            read (Lu, 99004, end = 300, err = 200) line1
            if (index('#!', line1(1:1))==0) then
                do
                    !
                    read (Lu, 99004, end = 300) line2
                    if (index('#!', line2(1:1))==0) then
                        !
                        i = 1
                        !
                        !---- try to read two numbers from first line
                        na = 2
                        call getflt(line1, a, na, error)
                        if (error .or. na<2) then
                            !------ must be a name string
                            Name = line1
                        else
                            !------ no name, just two valid numbers... must be plain airfoil file
                            Name = ' '
                            if (Info>0) then
                                if (show_output) then
                                    write (*, *)
                                    write (*, *) 'Plain airfoil file'
                                endif
                            endif
                            Itype = 1
                            rewind (Lu)
                            goto 5
                        endif
                        !
                        !---- if we got here, there's a name line,
                        !-    so now try to read four MSES domain numbers from second line
                        na = 4
                        call getflt(line2, a, na, error)
                        !------ less than two valid numbers... not a valid format
                        if (error .or. na<2) goto 300
                        !
                        if (na<4) then
                            !------ less than four numbers... usual .dat labeled file
                            Name = line1
                            if (Info>0) then
                                if (show_output) then
                                    write (*, *)
                                    write (*, *) 'Labeled airfoil file.  Name:  ', Name
                                endif
                            endif
                            Itype = 2
                            rewind (Lu)
                            read (Lu, 99004, end = 300) line1
                            !
                        else
                            !------ four or more numbers... MSES or MISES file
                            if (Info>0) then
                                if (show_output) then
                                    write (*, *)
                                    write (*, *) 'MSES airfoil file.  Name:  ', Name
                                endif
                            endif
                            Itype = 3
                            Ispars = line2
                        endif
                        5             do
                            !
                            !---- read each element until 999.0 or end of file is encountered
                            nel = nel + 1
                            do i = 1, Nmax
                                do
                                    read (Lu, 99004, end = 100) line
                                    !
                                    !------ skip comment line
                                    if (index('#!', line(1:1))==0) then
                                        !
                                        na = 2
                                        call getflt(line, a, na, error)
                                        if (error) goto 300
                                        !
                                        !------ skip line without at least two numbers
                                        if (na>=2) then
                                            !
                                            X(i) = a(1)
                                            Y(i) = a(2)
                                            !
                                            if (X(i)==999.0 .and. Y(i)==999.0) then
                                                !-------- if this is the element we want, just exit
                                                if (iel==nel) goto 100
                                                !
                                                if (iel==0) then
                                                    call aski('Enter element number^', iel)
                                                    Itype = 4
                                                endif
                                                !
                                                !-------- if this is the specified element, exit.
                                                if (iel==nel) goto 100
                                                goto 10
                                            endif
                                            exit
                                        endif
                                    endif
                                enddo
                            enddo
                            if (show_output) write (*, 99001) Nmax
                            99001              format (/' Buffer array size exceeded'/' Maximum number of points: ', i4)
                            if (show_output) write (*, 99005)
                            if (lopen) close (Lu)
                            Itype = 0
                            return
                        10            enddo
                    endif
                enddo
            endif
        enddo
        !
        100  N = i - 1
        if (lopen) close (Lu)
        return
        !
        200  nfn = index(Fname, ' ') + 1
        if (show_output) write (*, 99002) Fname(1:nfn)
        99002 format (/' File OPEN error.  Nonexistent file:  ', a)
        if (show_output) write (*, 99005)
        Itype = 0
        return
        !
        300  if (lopen) close (Lu)
        if (show_output) write (*, 99003)
        99003 format (/' File READ error.  Unrecognizable file format')
        if (show_output) write (*, 99005)
        Itype = 0
        return
        !...............................................................
        99004 format (a)
        99005 format (' *** LOAD NOT COMPLETED ***')
    end subroutine aread

end module m_aread