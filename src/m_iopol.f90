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

!*==POLREAD.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module m_iopol
contains
    subroutine polread(Lu, Fnpol, Error, &
            Nax, Na, Cpol, &
            Reyn1, Mach1, Acrit, Xtrip, &
            Ptrat, Etap, &
            Name, Iretyp, Imatyp, &
            Isx, Nbl, Cpolsd, &
            Code, Version)
        use i_xfoil, only: show_output
        use m_userio, only: getflt, strip
        use i_pindex
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Code, Fnpol, Name
        logical :: Error
        real :: Etap, Mach1, Ptrat, Reyn1, Version
        integer :: Imatyp, Iretyp, Isx, Lu, Na, Nax, Nbl
        real, dimension(Isx) :: Acrit, Xtrip
        real, dimension(Nax, IPTOT) :: Cpol
        real, dimension(Nax, Isx, JPTOT) :: Cpolsd
        intent (in) Fnpol, Isx, Lu, Nax
        intent (out) Code, Cpolsd, Etap, Name, Ptrat, Version
        intent (inout) Acrit, Cpol, Error, Imatyp, Iretyp, Mach1, Na, Nbl, Reyn1, Xtrip
        !
        ! Local variables
        !
        real :: acl
        character(20) :: cpname
        integer :: ia, ip, ipass, ipt, is, is1, is2, itmpp1, jp, k, k1, k2, kb, ke, kend, kf, km, kp, kr, &
                & ks, kt, n, ninp, nipol, nname
        integer, dimension(IPTOT) :: ipol
        integer, dimension(2, IPTOT) :: ispol
        integer, dimension(IPTOT + 2 * JPTOT) :: itmp, itmp0
        logical :: ldlab, lhead, lima, lire, ljnc, ljtp, lopen
        character(128) :: line
        real, dimension(0:IPTOT + 2 * JPTOT) :: rinp
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
        !     Reads in polar save file
        !
        !  Input:
        !     LU      logical unit to use for reading
        !     FNPOL   name of polar file to be read,
        !               if FNPOL(1:1).eq.' ', unit LU will be read
        !               if it is already open
        !     NAX     polar point array dimension
        !     ISX     airfoil side array dimension
        !
        !  Output:
        !     ERROR   T if a READ error occurred
        !     NA      number polar points
        !     CPOL    polar coefficients and parameters
        !     REYN1   Reynolds number for CL=1
        !     MACH1   Mach number for CL=1
        !     ACRIT   Critical amplification ratio
        !     XTRIP   Trip locations
        !     PTRAT   Actuator disk total-pressure ratio
        !     ETAP    Actuator disk thermal efficiency
        !     NAME    airfoil name string
        !     IRETYP  flag giving type of Re variation with CL
        !     IMATYP  flag giving type of Ma variation with CL
        !     NBL     number of airfoil elements
        !     CPOLSD  airfoil side-related parameters
        !     CODE    code used to compute polar
        !     VERSION code version
        !--------------------------------------------------------
        !
        !
        !
        Error = .false.
        lhead = .true.
        !
        Na = 0
        Nbl = 1
        !
        !      KCH = 0
        !      KMC = 0
        !
        nipol = 0
        do ip = 1, IPTOT
            ipol(ip) = 0
        enddo
        do jp = 1, JPTOT
            ispol(1, jp) = 0
            ispol(2, jp) = 0
        enddo
        !
        !---- assume Re,Mach will not be given in header
        Iretyp = 0
        Imatyp = 0
        !
        !---- do we have to open the file?
        lopen = Fnpol/=' '
        !
        if (lopen) open (Lu, file = Fnpol, status = 'OLD', err = 300)
        100  do
            !
            !=============================================================
            !---- start data reading loop
            read (Lu, 99001, end = 200) line
            !
            !..........................................
            99001 format (a)
            if (line/=' ') then
                !
                if (lhead) then
                    !----- parse to get header info
                    !
                    !----- assume this will be the data-label line
                    ldlab = .true.
                    !
                    !--------------------------------------------
                    k = index(line, 'Version')
                    if (k/=0) then
                        !------ code,version line
                        do k1 = 1, 128
                            if (line(k1:k1)/=' ') exit
                        enddo
                        !
                        if (k>k1) then
                            Code = line(k1:k - 1)
                            read (line(k + 7:128), *, err = 105) Version
                        endif
                        105           ldlab = .false.
                    endif
                    !
                    !--------------------------------------------
                    kf = index(line, 'for:')
                    if (kf/=0) then
                        !------ airfoil name line
                        Name = line(kf + 5:128)
                        ldlab = .false.
                    endif
                    !
                    !--------------------------------------------
                    ke = index(line, 'elements')
                    if (ke>0) then
                        !------ element-number line
                        read (line(ke - 4:ke - 1), *, err = 100) Nbl
                        !------ truncate name line to eliminate elements #
                        Name = line(kf + 5:ke - 4)
                        !
                        if (2 * Nbl>Isx) then
                            Nbl = Isx / 2
                            if (show_output) write (*, *) 'POLREAD: Number of elements set to array limit', Nbl
                        endif
                        ldlab = .false.
                    endif
                    !
                    !--------------------------------------------
                    kr = index(line, 'Reynolds number')
                    km = index(line, 'Mach number')
                    !
                    if (kr/=0) then
                        !------ Re-type line
                        if (km>kr) then
                            kend = km - 1
                        else
                            kend = 128
                        endif
                        if (index(line(kr:kend), 'fixed')/=0) then
                            Iretyp = 1
                        elseif (index(line(kr:kend), '1/sqrt(CL)')/=0) then
                            Iretyp = 2
                        elseif (index(line(kr:kend), '1/CL')/=0) then
                            Iretyp = 3
                        endif
                        ldlab = .false.
                    endif
                    !
                    if (km/=0) then
                        !------ Ma-type line
                        if (kr>km) then
                            kend = kr - 1
                        else
                            kend = 128
                        endif
                        if (index(line(km:kend), 'fixed')/=0) then
                            Imatyp = 1
                        elseif (index(line(km:kend), '1/sqrt(CL)')/=0) then
                            Imatyp = 2
                        elseif (index(line(km:kend), '1/CL')/=0) then
                            Imatyp = 3
                        endif
                        ldlab = .false.
                    endif
                    !
                    !--------------------------------------------
                    !---- find specified BL trip location
                    k = index(line, 'xtrf')
                    if (k/=0) then
                        !------ new style xtrip line
                        kt = index(line, '(top)')
                        kb = index(line, '(bottom)')
                        ke = index(line, 'element ')
                        !--- check for old style trip line
                        ks = index(line, '(suc')
                        kp = index(line, '(pre')
                        !
                        if (ke/=0) then
                            read (line(ke + 7:ke + 12), *, err = 110) n
                        else
                            n = 1
                        endif
                        if (n<=Nbl) then
                            is1 = 2 * n - 1
                            is2 = 2 * n
                            Xtrip(is1) = 1.0
                            Xtrip(is2) = 1.0
                            if (kt>0) read (line(k + 6:kt - 1), *, err = 110) Xtrip(is1)
                            if (kb>kt) read (line(kt + 5:kb - 1), *, err = 110) Xtrip(is2)
                            if (ks>0) read (line(k + 6:ks - 1), *, err = 110) Xtrip(is1)
                            if (kp>ks) read (line(ks + 5:kp - 1), *, err = 110) Xtrip(is2)
                        endif
                        110           ldlab = .false.
                    endif
                    !
                    !--------------------------------------------
                    k = index(line, 'Mach =')
                    if (k/=0) then
                        read (line(k + 6:128), *, err = 115) Mach1
                        115           ldlab = .false.
                    endif
                    !
                    !--------------------------------------------
                    k = index(line, 'Re =')
                    if (k/=0) then
                        read (line(k + 4:128), *, err = 120) Reyn1
                        Reyn1 = Reyn1 * 1.0E6
                        120           ldlab = .false.
                    endif
                    !
                    !--------------------------------------------
                    k = index(line, 'Ncrit =')
                    if (k/=0) then
                        ninp = 2
                        call getflt(line(k + 7:128), rinp(1), ninp, Error)
                        if (.not.(ninp<=0 .or. Error)) then
                            !
                            if (ninp==1) then
                                Acrit(1) = rinp(1)
                                Acrit(2) = rinp(1)
                            else
                                Acrit(1) = rinp(1)
                                Acrit(2) = rinp(2)
                            endif
                        endif
                        ldlab = .false.
                    endif
                    !
                    !--------------------------------------------
                    k = index(line, 'pi_p =')
                    if (k/=0) then
                        read (line(k + 6:128), *, err = 125) Ptrat
                        125           ldlab = .false.
                    endif
                    !
                    !--------------------------------------------
                    k = index(line, 'eta_p =')
                    if (k/=0) then
                        read (line(k + 7:128), *, err = 130) Etap
                        130           ldlab = .false.
                    endif
                    !
                    !--------------------------------------------
                    if (ldlab .and. nipol==0) then
                        !------ process line for possible data labels
                        do ip = 1, IPTOT
                            call strip(CPOlname(ip), nname)
                            !
                            !-------- mark this parameter for reading
                            k = index(line, CPOlname(ip)(1:nname))
                            itmp0(ip) = k
                            itmp(ip) = k
                        enddo
                        !
                        do jp = 1, JPTOT
                            call strip(CPOlsname(jp), nname)
                            !
                            cpname = 'Top ' // CPOlsname(jp)
                            k1 = index(line, cpname(1:nname + 4))
                            cpname = 'Top_' // CPOlsname(jp)
                            k2 = index(line, cpname(1:nname + 4))
                            itmp0(IPTOT + jp) = max(k1, k2)
                            itmp(IPTOT + jp) = max(k1, k2)
                            !
                            cpname = 'Bot ' // CPOlsname(jp)
                            k1 = index(line, cpname(1:nname + 4))
                            cpname = 'Bot_' // CPOlsname(jp)
                            k2 = index(line, cpname(1:nname + 4))
                            itmp0(IPTOT + jp + JPTOT) = max(k1, k2)
                            itmp(IPTOT + jp + JPTOT) = max(k1, k2)
                        enddo
                        !
                        !------ bubble-sort data label positions in line string
                        do ipass = 1, IPTOT + 2 * JPTOT
                            do ip = 1, IPTOT + 2 * JPTOT - 1
                                if (itmp(ip)>itmp(ip + 1)) then
                                    itmpp1 = itmp(ip + 1)
                                    itmp(ip + 1) = itmp(ip)
                                    itmp(ip) = itmpp1
                                endif
                            enddo
                        enddo
                        !
                        !------ assign data position to each parameter
                        do ipt = 1, IPTOT + 2 * JPTOT
                            if (itmp(ipt)>0) then
                                nipol = nipol + 1
                                do ip = 1, IPTOT
                                    if (itmp(ipt)==itmp0(ip)) ipol(ip) = nipol
                                enddo
                                do jp = 1, JPTOT
                                    if (itmp(ipt)==itmp0(IPTOT + jp)) ispol(1, jp) = nipol
                                    if (itmp(ipt)==itmp0(IPTOT + JPTOT + jp)) ispol(2, jp) = nipol
                                enddo
                            endif
                        enddo
                        !
                    endif
                    !
                    !--------------------------------------------
                    if (index(line, '-----')/=0) lhead = .false.
                    !
                    !--------------------------------------------------------------
                else
                    !----- read polar data lines
                    ia = Na + 1
                    !
                    ninp = IPTOT + 2 * JPTOT
                    call getflt(line, rinp(1), ninp, Error)
                    if (Error) goto 300
                    !
                    do ip = 1, IPTOT
                        Cpol(ia, ip) = rinp(ipol(ip))
                    enddo
                    !
                    do jp = 1, JPTOT
                        do n = 1, Nbl
                            is1 = 2 * n - 1
                            is2 = 2 * n
                            Cpolsd(ia, is1, jp) = rinp(ispol(1, jp) + 2 * (n - 1))
                            Cpolsd(ia, is2, jp) = rinp(ispol(2, jp) + 2 * (n - 1))
                        enddo
                    enddo
                    !
                    acl = max(Cpol(ia, ICL), 0.001)
                    !
                    !
                    !----- try to find Re, Ma, Ncrit, Xtrip  in polar data
                    lire = .false.
                    lima = .false.
                    ljnc = .false.
                    ljtp = .false.
                    do kp = 1, nipol
                        if (ipol(kp)==IRE) lire = .true.
                        if (ipol(kp)==IMA) lima = .true.
                        if (ispol(1, kp)==JNC) ljnc = .true.
                        if (ispol(1, kp)==JTP) ljtp = .true.
                    enddo
                    !
                    if (.not.lire) then
                        !------ Re was not in polar data... set using header info
                        if (Iretyp==1) then
                            Cpol(ia, IRE) = Reyn1
                        elseif (Iretyp==2) then
                            Cpol(ia, IRE) = Reyn1 / sqrt(acl)
                        elseif (Iretyp==3) then
                            Cpol(ia, IRE) = Reyn1 / acl
                        endif
                    endif
                    !
                    if (.not.lima) then
                        !------ Mach was not in polar data... set using header info
                        if (Imatyp==1) then
                            Cpol(ia, IMA) = Mach1
                        elseif (Imatyp==2) then
                            Cpol(ia, IMA) = Mach1 / sqrt(acl)
                        elseif (Imatyp==3) then
                            Cpol(ia, IMA) = Mach1 / acl
                        endif
                    endif
                    !
                    if (.not.ljnc) then
                        !------ Ncrit was not in polar data... set using header info
                        do is = 1, 2 * Nbl
                            Cpolsd(ia, is, JNC) = Acrit(is)
                        enddo
                    endif
                    !
                    if (.not.ljtp) then
                        !------ set trip data using header info
                        do is = 1, 2 * Nbl
                            Cpolsd(ia, is, JTP) = Xtrip(is)
                        enddo
                    endif
                    !
                    Na = ia
                    !
                    !---- go read next line
                endif
            endif
        enddo
        !=============================================================
        !
        !---- if file was opened here, then close it
        200  if (lopen) close (Lu)
        return
        !
        300  if (lopen) close (Lu)
        Error = .true.
    end subroutine polread
    !*==POLWRIT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! POLREAD


    subroutine polwrit(Lu, Fnpol, Error, Lhead, &
            Nax, Ia1, Ia2, Cpol, Ipol, Nipol, &
            Reyn1, Mach1, Acrit, Xtrip, &
            Ptrat, Etap, &
            Name, Iretyp, Imatyp, &
            Isx, Nbl, Cpolsd, Jpol, Njpol, &
            Code, Version, Lquery)
        use i_xfoil, only: show_output
        use m_userio, only: strip
        use i_pindex
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        character(*) :: Code, Fnpol, Name
        logical :: Error, Lhead, Lquery
        real :: Etap, Mach1, Ptrat, Reyn1, Version
        integer :: Ia1, Ia2, Imatyp, Iretyp, Isx, Lu, Nax, Nbl, Nipol, Njpol
        real, dimension(Isx) :: Acrit, Xtrip
        real, dimension(Nax, IPTOT) :: Cpol
        real, dimension(Nax, Isx, JPTOT) :: Cpolsd
        integer, dimension(IPTOT) :: Ipol
        integer, dimension(JPTOT) :: Jpol
        intent (in) Acrit, Code, Cpol, Cpolsd, Etap, Fnpol, Ia1, Ia2, Imatyp, Ipol, Iretyp, Isx, Jpol, Lhead, &
                & Lquery, Lu, Mach1, Name, Nax, Nbl, Nipol, Njpol, Ptrat, Reyn1, Version, Xtrip
        intent (out) Error
        !
        ! Local variables
        !
        character(1) :: ans
        integer :: ia, iffbc, ip, is, is1, is2, ismom, jp, kd, kdot, kf, kl, kp, n, nblank, nf, nform, nname
        character(29) :: line1, line2
        character(128) :: lined, linef, linel
        logical :: lopen
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
        !     Writes polar save file
        !
        !  Input:
        !     LU       logical unit to use for writing
        !     FNPOL    name of polar file to be read,
        !                if FNPOL(1:1).eq.' ', unit LU is assumed
        !                to be already open
        !     NAX      polar point array dimension
        !     ISX      airfoil side array dimension
        !     IA1,IA2  only polar points IA1..IA2 are written
        !     CPOL     polar coefficients and parameters
        !     IPOL(.)  indices of data quantities to be written
        !     NIPOL    number  of data quantities to be written
        !     REYN1    Reynolds number for CL=1
        !     MACH1    Mach number for CL=1
        !     ACRIT    Critical amplification ratio
        !     XTRIP    Trip locations
        !     PTRAT   Actuator disk total-pressure ratio
        !     ETAP    Actuator disk thermal efficiency
        !     NAME     airfoil name string
        !     IRETYP   flag giving type of Re variation with CL
        !     IMATYP   flag giving type of Ma variation with CL
        !     NBL      number of airfoil elements
        !     CPOLSD   airfoil side-related parameters
        !     JPOL(.)  indices of side data quantities to be written
        !     NJPOL    number  of side data quantities to be written
        !     LHEAD    T if header and column label are to be written
        !     CODE     code used to compute polar
        !     VERSION  code version
        !     LQUERY   if T, asks permission to overwrite existing file
        !
        !  Output:
        !     ERROR   T if a OPER or WRITE error occurred
        !--------------------------------------------------------
        !
        Error = .false.
        !
        !---- do we have to open the file?
        lopen = Fnpol/=' '
        !
        if (lopen) then
            open (Lu, file = Fnpol, status = 'OLD', err = 50)
            !
            if (Lquery) then
                if (show_output) then
                    write (*, *)
                    write (*, *) 'Output file exists.  Overwrite?  Y'
                endif
                read (*, 99010) ans
                !
                if (index('Nn', ans)==0) goto 100
                !
                close (Lu)
                if (show_output) write (*, *) 'Polar file not saved'
                return
            endif
            !
            50   open (Lu, file = Fnpol, status = 'UNKNOWN', err = 200)
            100  rewind (Lu)
        endif
        !
        if (Lhead) then
            write (Lu, *) ' '
            write (Lu, 99001) Code, Version
            99001 format (7x, a, 9x, 'Version', f5.2)
            write (Lu, *) ' '
            if (Nbl==1) then
                write (Lu, 99002) Name
                99002  format (1x, 'Calculated polar for: ', a)
            else
                write (Lu, 99003) Name, Nbl
                99003  format (1x, 'Calculated polar for: ', a, i4, ' elements')
            endif
            !
            iffbc = 0
            ismom = 0
            !
            if (iffbc/=0 .and. ismom/=0) then
                if (iffbc==1) line1 = ' Solid wall far field        '
                if (iffbc==2) line1 = ' Vortex + doublet far field  '
                if (iffbc==3) line1 = ' Constant pressure far field '
                if (iffbc==4) line1 = ' Supersonic wave far field   '
                if (iffbc>=5) line1 = '                             '
                if (ismom==1) line2 = '   S-momentum conserved      '
                if (ismom==2) line2 = '   Entropy conserved         '
                if (ismom==3) line2 = '   Entropy conserved near LE '
                if (ismom==4) line2 = '   S-mom conserved at shocks '
                if (ismom>=5) line2 = '                             '
                write (Lu, 99004) line1, line2
                99004  format (1x, 3x, 2A29)
            endif
            !
            write (Lu, *) ' '
            !
            line1 = ' '
            line2 = ' '
            if (Iretyp==1) line1 = ' Reynolds number fixed       '
            if (Iretyp==2) line1 = ' Reynolds number ~ 1/sqrt(CL)'
            if (Iretyp==3) line1 = ' Reynolds number ~ 1/CL      '
            if (Imatyp==1) line2 = '   Mach number fixed         '
            if (Imatyp==2) line2 = '   Mach number ~ 1/sqrt(CL)  '
            if (Imatyp==3) line2 = '   Mach number ~ 1/CL        '
            write (Lu, 99005) Iretyp, Imatyp, line1, line2
            99005 format (1x, i1, i2, 2A29)
            !
            write (Lu, *) ' '
            do n = 1, Nbl
                is1 = 2 * n - 1
                is2 = 2 * n
                if (Nbl==1) then
                    write (Lu, 99006) Xtrip(is1), Xtrip(is2)
                    99006      format (1x, 'xtrf = ', f7.3, ' (top)    ', f9.3, ' (bottom)  ')
                else
                    write (Lu, 99007) Xtrip(is1), Xtrip(is2), n
                    99007      format (1x, 'xtrf = ', f7.3, ' (top)    ', f9.3, ' (bottom)     element', i3)
                endif
            enddo
            write (Lu, 99008) Mach1, Reyn1 / 1.0E6, Acrit(1), Acrit(2)
            99008 format (1x, 'Mach = ', f7.3, 5x, 'Re = ', f9.3, ' e 6', 5x, 'Ncrit = ', 20F7.3)
            if (Ptrat/=0.0) write (Lu, 99009) Ptrat, Etap
            99009 format (1x, 'pi_p = ', f7.4, 5x, 'eta_p = ', f9.4)
            write (Lu, *) ' '
            !
            linel = ' '
            lined = ' '
            !
            kl = 1
            kd = 1
            !
            do kp = 1, Nipol
                ip = Ipol(kp)
                if (ip/=0) then
                    !
                    kdot = index(CPOlform(ip), '.')
                    if (kdot==0) kdot = len(CPOlform(ip))
                    read (CPOlform(ip)(2:kdot - 1), *, err = 300) nform
                    !
                    call strip(CPOlname(ip), nname)
                    nblank = max((nform - nname + 2) / 2, 0)
                    !
                    linel(kl + 1 + nblank:kl + nname + nblank) = CPOlname(ip)(1:nname)
                    kl = kl + nform
                    !
                    lined(kd + 2:kd + nform) = '--------------------------------'
                    kd = kd + nform
                endif
            enddo
            !
            do kp = 1, Njpol
                jp = Jpol(kp)
                if (jp/=0) then
                    !
                    kdot = index(CPOlsform(jp), '.')
                    if (kdot==0) kdot = len(CPOlsform(jp))
                    read (CPOlsform(jp)(2:kdot - 1), *, err = 300) nform
                    !
                    call strip(CPOlsname(jp), nname)
                    nblank = max((nform - nname - 2) / 2, 0)
                    !
                    do n = 1, Nbl
                        linel(kl + 1 + nblank:kl + 4 + nname + nblank) = 'Top_' // CPOlsname(jp)(1:nname)
                        kl = kl + nform
                        !
                        lined(kd + 2:kd + nform) = '--------------------------------'
                        kd = kd + nform
                        !
                        linel(kl + 1 + nblank:kl + 4 + nname + nblank) = 'Bot_' // CPOlsname(jp)(1:nname)
                        kl = kl + nform
                        !
                        lined(kd + 2:kd + nform) = '--------------------------------'
                        kd = kd + nform
                    enddo
                endif
            enddo
            !
            !
            !
            !       LINEL =
            !     & '  alpha     CL        CD       CDp       CM    Top_Xtr Bot_Xtr'
            !CC     1234567890123456789012345678901234567890123456789012345678901234567890
            !       K = 62
            !
            !
            !       LINEL =
            !     & ' ------- -------- --------- --------- -------- ------- -------'
            !CC       3.453   1.3750   0.00921   0.00512  -0.1450  0.9231  0.5382
            !CC       3.453   1.3750   0.00921   0.00213  -0.1450  0.9231  0.5382
            !       K = 62

            write (Lu, 99010) linel(1:kl)
            write (Lu, 99010) lined(1:kd)
            !
        endif
        !

        linef = '(1X'
        kf = 3
        do kp = 1, Nipol
            ip = Ipol(kp)
            nf = len(CPOlform(ip))
            !
            linef(kf + 1:kf + nf + 1) = ',' // CPOlform(ip)
            kf = kf + nf + 1
        enddo
        do kp = 1, Njpol
            jp = Jpol(kp)
            nf = len(CPOlsform(jp))
            !
            do n = 1, Nbl
                linef(kf + 1:kf + nf + 1) = ',' // CPOlsform(jp)
                kf = kf + nf + 1
                !
                linef(kf + 1:kf + nf + 1) = ',' // CPOlsform(jp)
                kf = kf + nf + 1
            enddo
        enddo
        linef(kf + 1:kf + 1) = ')'
        kf = kf + 1
        !
        !
        do ia = Ia1, Ia2
            write (Lu, linef) (Cpol(ia, Ipol(kp)), kp = 1, Nipol), &
                    ((Cpolsd(ia, is, Jpol(kp)), is = 1, 2 * Nbl), kp = 1, Njpol)
        enddo
        !
        !
        !---- if file was opened here, then close it
        if (lopen) close (Lu)
        return
        !
        200  Error = .true.
        return
        !
        300  if (show_output) write (*, *) '? Bad CPOLFORM set up in PINDEX.INC'
        stop
        !
        !......................................................................
        99010 format (a)
        !CC      3.453   1.3750   0.00921     0.500  -0.1450  0.9231  0.5382 -0.00942
        !CC      3.453   1.3750   0.00921     0.500  -0.1450  0.9231  0.5382
    end subroutine polwrit
    !*==POLREF.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine polref(Lu, Fnref, Error, Nfx, Nf, Xyref, Labref)
        use i_pindex
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        logical :: Error
        character(*) :: Fnref, Labref
        integer :: Lu, Nfx
        integer, dimension(4) :: Nf
        real, dimension(Nfx, 2, 4) :: Xyref
        intent (in) Fnref, Lu, Nfx
        intent (out) Error, Labref, Nf
        intent (inout) Xyref
        !
        ! Local variables
        !
        integer :: i, k
        character(80) :: line
        logical :: lopen
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
        !     Reads in polar reference data file
        !
        !  Input:
        !     LU      logical unit to use for reading
        !     FNREF   name of polar file to be read,
        !               if FNREF(1:1).eq.' ', unit LU is assumed
        !               to be already open
        !     NFX     polar point array dimension
        !
        !  Output:
        !     ERROR      T if a READ error occurred
        !     NF(.)      number of points in each data block
        !     XYREF(...) reference polar data
        !     LABREF(.)  reference polar label
        !--------------------------------------------------------
        !
        Error = .false.
        lopen = Fnref(1:1)/=' '
        if (lopen) open (Lu, file = Fnref, status = 'OLD', err = 100)
        !
        !---- try to read data label
        read (Lu, 99001, end = 100) line
        99001 format (a)
        !
        !---- set data label if present
        if (line(1:1)=='#') then
            Labref = line(2:80)
        else
            Labref = ' '
            rewind (Lu)
        endif
        !
        do k = 1, 4
            do i = 1, Nfx
                read (Lu, *, end = 50, err = 100) Xyref(i, 1, k), Xyref(i, 2, k)
                if (Xyref(i, 1, k)==999.0) exit
            enddo
            50   Nf(k) = i - 1
        enddo
        if (lopen) close (Lu)
        return
        !
        100  Error = .true.
        !
    end subroutine polref

end module m_iopol