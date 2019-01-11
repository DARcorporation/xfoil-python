!*==XFOIL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


!***********************************************************************
!    Module:  xfoil.f
!
!    Copyright (C) 2000 Mark Drela
!
!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation; either version 2 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program; if not, write to the Free Software
!    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!***********************************************************************
!
program xfoil
    use m_xfoil
    use m_xpanel
    use m_xoper
    use m_xmdes
    use m_xgeom
    use m_xgdes
    use m_xbl
    use m_userio
    use m_spline
    use m_xqdes
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Local variables
    !
    real :: amax
    real, save :: angtol
    character(1) :: ans
    character(4) :: comand
    character(128) :: comarg
    logical :: error
    integer :: i, imax, itype, kdnew, lu, narg, nfn, ninput
    integer, dimension(20) :: iinput
    real, dimension(20) :: rinput
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
    !--- Uncomment for Win32/Compaq Visual Fortran compiler (needed for GETARG)
    !cc      USE DFLIB
    !
    !
    !
    !---- max panel angle threshold for warning
    data angtol/40.0/
    !
    !---- prepare BL pointers
    call preptrs
    !
    VERsion = 6.99
    write (*, 99001) VERsion
    99001 format (/' ==================================================='/'  XFOIL Version', &
            &f5.2/'  Copyright (C) 2000   Mark Drela, Harold Youngren'//                                               &
            &'  This software comes with ABSOLUTELY NO WARRANTY,'/'    subject to the GNU General Public License.'//   &
            &'  Caveat computor'/' ===================================================')
    !
    call init
    lu = 8
    call getdef(lu, 'xfoil.def', .true.)
    !
    !---- try to read airfoil from command line argument, if any
    FNAme = ' '
    narg = iargc()
    if (narg>0) call getarg(narg, FNAme)
    !
    if (FNAme(1:1)/=' ') then
        call load(FNAme, itype)
        !
        if (itype>0 .and. NB>0) then
            !cc     CALL PANGEN(.TRUE.)
            call abcopy(.true.)
            !
            call cang(X, Y, N, 0, imax, amax)
            if (abs(amax)>angtol) write (*, 99007) amax, imax
        endif
    endif
    !
    write (*, 99008) XCMref, YCMref, NPAn
    do
        !
        !---- start of menu loop
        call askc(' XFOIL^', comand, comarg)
        !
        !---- get command line numeric arguments, if any
        do i = 1, 20
            iinput(i) = 0
            rinput(i) = 0.0
        enddo
        ninput = 0
        call getint(comarg, iinput, ninput, error)
        ninput = 0
        call getflt(comarg, rinput, ninput, error)
        !
        !===============================================
        if (comand=='    ') then
            !
            !===============================================
        elseif (comand=='?   ') then
            write (*, 99008) XCMref, YCMref, NPAn
            !
            !===============================================
        elseif (comand=='QUIT' .or. comand=='Q   ') then
            stop
            !
            !===============================================
        elseif (comand=='OPER') then
            call oper
            !
            !===============================================
        elseif (comand=='MDES') then
            call mdes
            !
            !===============================================
        elseif (comand=='QDES') then
            call qdes
            !
            !===============================================
        elseif (comand=='SAVE') then
            call save(1, comarg)
            !
            !===============================================
        elseif (comand=='PSAV') then
            call save(0, comarg)
            !
            !===============================================
        elseif (comand=='USAV') then
            call save(-1, comarg)
            !
            !===============================================
        elseif (comand=='ISAV') then
            call save(2, comarg)
            !
            !===============================================
        elseif (comand=='REVE') then
            LCLock = .not.LCLock
            if (LCLock) then
                write (*, *) 'Airfoil will be written in clockwise order'
            else
                write (*, *) 'Airfoil will be written in counterclockwise order'
            endif
            !
            !===============================================
        elseif (comand=='DELI') then
            do
                if (ninput>=1) then
                    kdnew = iinput(1)
                else
                    write (*, 99002) KDElim
                    99002          format (/'  --------------------------'/'   0  blank'/'   1  comma'/'   2  tab', &
                            &//'  currently, delimiter =', i2)
                    call aski('Enter new delimiter', kdnew)
                endif
                !
                if (kdnew<0 .or. kdnew>2) then
                    ninput = 0
                    cycle
                else
                    KDElim = kdnew
                endif
                exit
            enddo
            !
            !===============================================
        elseif (comand=='LOAD') then
            call load(comarg, itype)
            if (itype>0 .and. NB>0) then
                !cc       CALL PANGEN(.TRUE.)
                call abcopy(.true.)
                !
                call cang(X, Y, N, 0, imax, amax)
                if (abs(amax)>angtol) write (*, 99007) amax, imax
            endif
            !
            !===============================================
        elseif (comand=='NACA') then
            call naca(iinput(1))
            !
            !===============================================
        elseif (comand=='INTE') then
            call inte
            !
            !===============================================
        elseif (comand=='INTX') then
            call intx
            !
            !===============================================
        elseif (comand=='NORM') then
            LNOrm = .not.LNOrm
            if (LNOrm) then
                write (*, *) 'Loaded airfoil will  be normalized'
            else
                write (*, *) 'Loaded airfoil won''t be normalized'
            endif
            !
            !===============================================
        elseif (comand=='HALF') then
            call half(XB, YB, SB, NB)
            call scalc(XB, YB, SB, NB)
            call segspl(XB, XBP, SB, NB)
            call segspl(YB, YBP, SB, NB)
            !
            call geopar(XB, XBP, YB, YBP, SB, NB, W1, SBLe, CHOrdb, &
                    AREab, RADble, ANGbte, EI11ba, EI22ba, APX1ba, APX2ba, EI11bt, EI22bt, &
                    & APX1bt, APX2bt, THIckb, CAMbrb)
            !
            !==========================================
        elseif (comand=='XYCM') then
            if (ninput>=2) then
                XCMref = rinput(1)
                YCMref = rinput(2)
            else
                call askr('Enter new CM reference X^', XCMref)
                call askr('Enter new CM reference Y^', YCMref)
            endif
            !
            !===============================================
        elseif (comand=='BEND') then
            if (N==0) then
                write (*, *)
                write (*, *) '***  No airfoil available  ***'
                cycle
            endif
            !
            call bendump(N, X, Y)
            !
            !===============================================
        elseif (comand=='BENP') then
            if (N==0) then
                write (*, *)
                write (*, *) '***  No airfoil available  ***'
                cycle
            endif
            !
            do i = 1, N
                W1(i) = 1.0
            enddo
            call bendump2(N, X, Y, W1)
            !
            !===============================================
        elseif (comand=='PCOP') then
            call abcopy(.true.)
            !
            !===============================================
        elseif (comand=='PANE') then
            call pangen(.true.)
            !
            !===============================================
        elseif (comand=='PPAR') then
            call getpan
            !
            !===============================================
        elseif (comand=='WDEF') then
            lu = 8
            if (comarg(1:1)==' ') then
                FNAme = 'xfoil.def'
            else
                FNAme = comarg
            endif
            call strip(FNAme, nfn)
            open (lu, file = FNAme, status = 'OLD', err = 20)
            write (*, 99003) FNAme(1:nfn)
            99003  format (/'  File  ', a, '  exists.  Overwrite?  Y')
            read (*, 99004) ans
            !
            99004  format (a)
            if (index('Nn', ans)==0) goto 40
            write (*, *)
            write (*, *) 'No action taken'
            close (lu)
            !
            20    open (lu, file = FNAme, status = 'UNKNOWN')
            40    call wrtdef(lu)
            write (*, 99005) FNAme(1:nfn)
            99005  format (/'  File  ', a, '  written')
            close (lu)
            !
            !===============================================
        elseif (comand=='RDEF') then
            if (comarg(1:1)==' ') then
                FNAme = 'xfoil.def'
            else
                FNAme = comarg
            endif
            !
            lu = 8
            call getdef(lu, FNAme, .false.)
            !
            !===============================================
        elseif (comand=='NAME') then
            if (comarg==' ') then
                call nammod(NAMe, 0, -1)
            else
                NAMe = comarg
            endif
            call strip(NAMe, NNAme)
            !
            !===============================================
        elseif (comand=='NINC') then
            call nammod(NAMe, 1, 1)
            call strip(NAMe, NNAme)
            !
            !===============================================
        else
            write (*, 99006) comand
            99006  format (1x, a4, ' command not recognized.  Type a "?" for list')
            !
            !
            !===============================================
        endif
    enddo
    99007 format (/' WARNING: Poor input coordinate distribution'/'          Excessive panel angle', f7.1, '  at i =', &
            &i4/'          Repaneling with PANE and/or PPAR suggested'/                                                &
            &'           (doing GDES,CADD before repaneling _may_'/'            improve excessively coarse LE spacing')
    99008 format (&
            /'   QUIT    Exit program'&
            //'  .OPER    Direct operating point(s)'&
            /'  .MDES    Complex mapping design routine'&
            /'  .QDES    Surface speed design routine'&
            //'   SAVE f  Write airfoil to labeled coordinate file'&
            /'   PSAV f  Write airfoil to plain coordinate file'&
            /'   ISAV f  Write airfoil to ISES coordinate file'&
            /'   REVE    Reverse written-airfoil node ordering'&
            /'   DELI i  Change written-airfoil file delimiters'&
            //'   LOAD f  Read buffer airfoil from coordinate file'&
            /'   NACA i  Set NACA 4,5-digit airfoil and buffer airfoil'&
            /'   INTE    Set buffer airfoil by interpolating two airfoils'&
            /'   NORM    Buffer airfoil normalization toggle'&
            /'   HALF    Halve the number of points in buffer airfoil'&
            /'   XYCM rr Change CM reference location, currently ', 2F8.5&
            //'   BEND    Display structural properties of current airfoil'&
            //'   PCOP    Set current-airfoil panel nodes directly', &
            ' from buffer airfoil points'&
            /'   PANE    Set current-airfoil panel nodes (', I4, ' )', &
            ' based on curvature'&
            /'  .PPAR    Show/change paneling'&
            //'   WDEF f  Write  current-settings file'&
            /'   RDEF f  Reread current-settings file'&
            /'   NAME s  Specify new airfoil name'&
            /'   NINC    Increment name version number')
end program xfoil
!*==INIT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! XFOIL
