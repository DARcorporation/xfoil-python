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

!*==ASKI.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
!
!
!==== user input routines with prompting and error trapping
!
!
module m_userio
contains
    subroutine aski(Prompt, Iinput)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Iinput
        character(*) :: Prompt
        intent (in) Prompt
        intent (out) Iinput
        !
        ! Local variables
        !
        character(80) :: line
        integer :: np
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
        !---- integer input
        !
        !
        np = index(Prompt, '^') - 1
        if (np<=0) np = len(Prompt)
        !
        100  if (show_output) write (*, 99001) Prompt(1:np)
        !
        99001 format (/a, '   i>  ', $)
        !
        read (*, 99002, err = 100) line
        99002 format (a)
        if (line/=' ') read (line, *, err = 100) Iinput
    end subroutine aski
    !*==ASKR.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! ASKI


    subroutine askr(Prompt, Rinput)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Prompt
        real :: Rinput
        intent (in) Prompt
        intent (out) Rinput
        !
        ! Local variables
        !
        character(80) :: line
        integer :: np
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
        !---- real input
        !
        !
        np = index(Prompt, '^') - 1
        if (np<=0) np = len(Prompt)
        !
        100  if (show_output) write (*, 99001) Prompt(1:np)
        !
        99001 format (/a, '   r>  ', $)
        !
        read (*, 99002, err = 100) line
        99002 format (a)
        if (line/=' ') read (line, *, err = 100) Rinput
    end subroutine askr
    !*==ASKL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! ASKR


    subroutine askl(Prompt, Linput)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        logical :: Linput
        character(*) :: Prompt
        intent (in) Prompt
        intent (out) Linput
        !
        ! Local variables
        !
        character(1) :: char
        integer :: np
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
        !---- logical input
        !
        !
        np = index(Prompt, '^') - 1
        if (np<=0) np = len(Prompt)
        do
            !
            if (show_output) write (*, 99001) Prompt(1:np)
            !
            99001 format (/a, ' y/n>  ', $)
            read (*, 99002) char
            99002 format (a)
            if (char=='y') char = 'Y'
            if (char=='n') char = 'N'
            if (char=='Y' .or. char=='N') then
                !
                Linput = char=='Y'
                return
            endif
        enddo
    end subroutine askl
    !*==ASKS.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! ASKL


    subroutine asks(Prompt, Input)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Input, Prompt
        intent (in) Prompt
        intent (out) Input
        !
        ! Local variables
        !
        integer :: np
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
        !---- string of arbitrary length input
        !
        !
        np = index(Prompt, '^') - 1
        if (np<=0) np = len(Prompt)
        !
        if (show_output) write (*, 99001) Prompt(1:np)
        !
        99001 format (/a, '   s>  ', $)
        read (*, 99002) Input
        99002 format (a)
        !
    end subroutine asks
    !*==ASKC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! ASKS


    subroutine askc(Prompt, Comand, Cargs)
        use i_xfoil, only: show_output
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Cargs, Comand, Prompt
        intent (in) Prompt
        !
        ! Local variables
        !
        integer :: i, izero, k, ki, ncargs, np
        character(128) :: line
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
        !---- returns 4-byte character string input converted to uppercase
        !---- also returns rest of input characters in CARGS string
        !
        !
        !
        izero = ichar('0')
        !
        np = index(Prompt, '^') - 1
        if (np<=0) np = len(Prompt)
        !
        if (show_output) write (*, 99001) Prompt(1:np)
        !
        99001 format (/a, '   c>  ', $)
        read (*, 99002) line
        99002 format (a)
        !
        !---- strip off leading blanks
        do k = 1, 128
            if (line(1:1)/=' ') exit
            line = line(2:128)
        enddo
        !
        !---- find position of first blank, "+", "-", ".", ",", or numeral
        k = index(line, ' ')
        ki = index(line, '-')
        if (ki/=0) k = min(k, ki)
        ki = index(line, '+')
        if (ki/=0) k = min(k, ki)
        ki = index(line, '.')
        if (ki/=0) k = min(k, ki)
        ki = index(line, ',')
        if (ki/=0) k = min(k, ki)
        do i = 0, 9
            ki = index(line, char(izero + i))
            if (ki/=0) k = min(k, ki)
        enddo
        !
        !---- there is no blank between command and argument... use first 4 characters
        if (k<=0) k = 5
        !
        if (k==1) then
            !------ the "command" is a number... set entire COMAND string with it
            Comand = line
        else
            !------ the "command" is some string... just use the part up to the argument
            Comand = line(1:k - 1)
        endif
        !
        !---- convert it to uppercase
        call lc2uc(Comand)
        !
        Cargs = line(k:128)
        call strip(Cargs, ncargs)
    end subroutine askc
    !*==LC2UC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! ASKC


    subroutine lc2uc(Input)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Input
        intent (inout) Input
        !
        ! Local variables
        !
        integer :: i, k, n
        character(26), save :: lcase, ucase
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
        data lcase/'abcdefghijklmnopqrstuvwxyz'/
        data ucase/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
        !
        n = len(Input)
        !
        do i = 1, n
            k = index(lcase, Input(i:i))
            if (k>0) Input(i:i) = ucase(k:k)
        enddo
        !
    end subroutine lc2uc
    !*==READI.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! LC2UC



    subroutine readi(N, Ivar, Error)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        logical :: Error
        integer :: N
        integer, dimension(N) :: Ivar
        intent (in) N
        intent (inout) Ivar
        !
        ! Local variables
        !
        integer :: i, ntmp
        integer, dimension(40) :: ivtmp
        character(80) :: line
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
        !--------------------------------------------------
        !     Reads N integer variables, leaving unchanged
        !     if only <return> is entered.
        !--------------------------------------------------
        !
        read (*, 99001) line
        99001 format (a80)
        !
        do i = 1, N
            ivtmp(i) = Ivar(i)
        enddo
        !
        ntmp = 40
        call getint(line, ivtmp, ntmp, Error)
        !
        if (Error) return
        !
        do i = 1, N
            Ivar(i) = ivtmp(i)
        enddo
        !
    end subroutine readi
    !*==READR.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! READI



    subroutine readr(N, Var, Error)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        logical :: Error
        integer :: N
        real, dimension(N) :: Var
        intent (in) N
        intent (inout) Var
        !
        ! Local variables
        !
        integer :: i, ntmp
        character(80) :: line
        real, dimension(40) :: vtmp
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
        !-------------------------------------------------
        !     Reads N real variables, leaving unchanged
        !     if only <return> is entered.
        !-------------------------------------------------
        !
        read (*, 99001) line
        99001 format (a80)
        !
        do i = 1, N
            vtmp(i) = Var(i)
        enddo
        !
        ntmp = 40
        call getflt(line, vtmp, ntmp, Error)
        !
        if (Error) return
        !
        do i = 1, N
            Var(i) = vtmp(i)
        enddo
        !
    end subroutine readr
    !*==GETINT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! READR




    subroutine getint(Input, A, N, Error)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        logical :: Error
        character(*) :: Input
        integer :: N
        integer, dimension(*) :: A
        intent (in) Input
        intent (out) A, Error
        intent (inout) N
        !
        ! Local variables
        !
        integer :: i, ilen, ilenp, ipass, k, kcomma, kspace, ninp
        character(130) :: rec
        character(1) :: tab
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
        !----------------------------------------------------------
        !     Parses character string INPUT into an array
        !     of integer numbers returned in A(1...N)
        !
        !     Will attempt to extract no more than N numbers,
        !     unless N = 0, in which case all numbers present
        !     in INPUT will be extracted.
        !
        !     N returns how many numbers were actually extracted.
        !----------------------------------------------------------
        !
        tab = char(9)
        !
        !---- only first 128 characters in INPUT will be parsed
        ilen = min(len(Input), 128)
        ilenp = ilen + 2
        !
        !---- put input into local work string (which will be munched)
        rec(1:ilenp) = Input(1:ilen) // ' ,'
        !
        !---- ignore everything after a "!" character
        k = index(rec, '!')
        if (k>0) rec(1:ilen) = rec(1:k - 1)
        do
            !
            !---- change tabs to spaces
            k = index(rec(1:ilen), tab)
            if (k>0) then
                rec(k:k) = ' '
                cycle
            endif
            !
            ninp = N
            !
            !---- count up how many numbers are to be extracted
            N = 0
            k = 1
            do ipass = 1, ilen
                !------ search for next space or comma starting with current index K
                kspace = index(rec(k:ilenp), ' ') + k - 1
                kcomma = index(rec(k:ilenp), ',') + k - 1
                !
                if (k==kspace) then
                    !------- just skip this space
                    k = k + 1
                    goto 20
                endif
                !
                if (k==kcomma) then
                    !------- comma found.. increment number count and keep looking
                    N = N + 1
                    k = k + 1
                    goto 20
                endif
                !
                !------ neither space nor comma found, so we ran into a number...
                !-    ...increment number counter and keep looking after next space or comma
                N = N + 1
                k = min(kspace, kcomma) + 1
                !
                20    if (k>=ilen) exit
            enddo
            exit
        enddo
        !
        !---- decide on how many numbers to read, and go ahead and read them
        if (ninp>0) N = min(N, ninp)
        read (rec(1:ilen), *, err = 100) (A(i), i = 1, N)
        Error = .false.
        return
        !
        !---- bzzzt !!!
        !cc   WRITE(*,*) 'GETINT: String-to-integer conversion error.'
        100  N = 0
        Error = .true.
    end subroutine getint
    !*==GETFLT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! GETINT


    subroutine getflt(Input, A, N, Error)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        logical :: Error
        character(*) :: Input
        integer :: N
        real, dimension(*) :: A
        intent (in) Input
        intent (out) A, Error
        intent (inout) N
        !
        ! Local variables
        !
        integer :: i, ilen, ilenp, ipass, k, kcomma, kspace, ninp
        character(130) :: rec
        character(1) :: tab
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
        !----------------------------------------------------------
        !     Parses character string INPUT into an array
        !     of real numbers returned in A(1...N)
        !
        !     Will attempt to extract no more than N numbers,
        !     unless N = 0, in which case all numbers present
        !     in INPUT will be extracted.
        !
        !     N returns how many numbers were actually extracted.
        !----------------------------------------------------------
        !
        tab = char(9)
        !
        !---- only first 128 characters in INPUT will be parsed
        ilen = min(len(Input), 128)
        ilenp = ilen + 2
        !
        !---- put input into local work string (which will be munched)
        rec(1:ilenp) = Input(1:ilen) // ' ,'
        !
        !---- ignore everything after a "!" character
        k = index(rec, '!')
        if (k>0) rec(1:ilen) = rec(1:k - 1)
        do
            !
            !---- change tabs to spaces
            k = index(rec(1:ilen), tab)
            if (k>0) then
                rec(k:k) = ' '
                cycle
            endif
            !
            ninp = N
            !
            !---- count up how many numbers are to be extracted
            N = 0
            k = 1
            do ipass = 1, ilen
                !------ search for next space or comma starting with current index K
                kspace = index(rec(k:ilenp), ' ') + k - 1
                kcomma = index(rec(k:ilenp), ',') + k - 1
                !
                if (k==kspace) then
                    !------- just skip this space
                    k = k + 1
                    goto 20
                endif
                !
                if (k==kcomma) then
                    !------- comma found.. increment number count and keep looking
                    N = N + 1
                    k = k + 1
                    goto 20
                endif
                !
                !------ neither space nor comma found, so we ran into a number...
                !-    ...increment number counter and keep looking after next space or comma
                N = N + 1
                k = min(kspace, kcomma) + 1
                !
                20    if (k>=ilen) exit
            enddo
            exit
        enddo
        !
        !---- decide on how many numbers to read, and go ahead and read them
        if (ninp>0) N = min(N, ninp)
        read (rec(1:ilen), *, err = 100) (A(i), i = 1, N)
        Error = .false.
        return
        !
        !---- bzzzt !!!
        !cc   WRITE(*,*) 'GETFLT: String-to-integer conversion error.'
        100  N = 0
        Error = .true.
    end subroutine getflt
    !*==STRIP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! GETFLT



    subroutine strip(String, Ns)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ns
        character(*) :: String
        intent (inout) Ns, String
        !
        ! Local variables
        !
        integer :: k, k1, k2, nlen
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
        !     Strips leading blanks off STRING and returns
        !     length NS of non-blank part.
        !----------------------------------------------------
        nlen = len(String)
        !
        !---- find last non-blank character
        do k2 = nlen, 1, -1
            if (String(k2:k2)/=' ') goto 100
        enddo
        k2 = 0
        !
        !---- find first non-blank character
        100  do k1 = 1, k2
            if (String(k1:k1)/=' ') exit
        enddo
        !
        !---- number of non-blank characters
        Ns = k2 - k1 + 1
        if (Ns==0) return
        !
        !---- shift STRING so first character is non-blank
        String(1:Ns) = String(k1:k2)
        !
        !---- pad tail of STRING with blanks
        do k = Ns + 1, nlen
            String(k:k) = ' '
        enddo
        !
    end subroutine strip
    !*==BSTRIP.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine bstrip(String, Ns)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: Ns
        character(*) :: String
        intent (inout) Ns, String
        !
        ! Local variables
        !
        integer :: k
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
        !--------------------------------------------------
        !     Strips all blanks from STRING and returns
        !     length NS of non-blank part.
        !     If STRING is all blanks, just returns NS=0
        !--------------------------------------------------
        !
        !---- first remove any leading blanks and get length to be processed
        call strip(String, Ns)
        !
        !---- pass over STRING and strip out all interior blanks
        k = 1
        do
            !
            if (k>=Ns) then
                return
                !
            elseif (String(k:k)==' ') then
                String(k:Ns - 1) = String(k + 1:Ns)
                Ns = Ns - 1
                !
            else
                k = k + 1
                !
                !
            endif
        enddo
        !
    end subroutine bstrip
    !*==GETARG0.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine getarg0(Iarg, Arg)
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Arg
        integer :: Iarg
        !
        ! Local variables
        !
        integer :: narg
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
        !------------------------------------------------
        !     Same as GETARG, but...
        !
        !     ...in the case of Intel Fortran, this one
        !     doesn't barf if there's no Unix argument
        !      (just returns blank string instead)
        !------------------------------------------------
        !
        narg = iargc()
        if (narg>=Iarg) then
            call getarg(Iarg, Arg)
        else
            Arg = ' '
        endif
        !
    end subroutine getarg0

end module m_userio