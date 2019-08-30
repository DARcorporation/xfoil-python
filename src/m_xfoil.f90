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

!*==XFOIL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module m_xfoil
contains
subroutine xfoil() bind(c, name='xfoil')
!    use m_xfoil, only: getpan, save, intx, pangen, load, init, naca, inte, getdef, wrtdef
    use m_xoper, only: nammod, oper
    use m_xmdes, only: mdes
    use m_xgeom, only: bendump2, half, geopar, cang, bendump
    use m_xgdes, only: abcopy
    use m_userio, only: aski, askr, getflt, getint, strip, askc
    use m_spline, only: scalc, segspl
    use m_xqdes, only: qdes
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
    !
    VERsion = 6.99
    if (show_output) write (*, 99001) VERsion
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
    if (abs(amax)>angtol .and. show_output) write (*, 99007) amax, imax
    endif
    endif
    !
    if (show_output) write (*, 99008) XCMref, YCMref, NPAn
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
    if (show_output) write (*, 99008) XCMref, YCMref, NPAn
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
    if (show_output) write (*, *) 'Airfoil will be written in clockwise order'
    else
    if (show_output) write (*, *) 'Airfoil will be written in counterclockwise order'
    endif
    !
    !===============================================
    elseif (comand=='DELI') then
    do
    if (ninput>=1) then
    kdnew = iinput(1)
    else
    if (show_output) write (*, 99002) KDElim
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
    if (abs(amax)>angtol .and. show_output) write (*, 99007) amax, imax
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
    if (show_output) write (*, *) 'Loaded airfoil will  be normalized'
    else
    if (show_output) write (*, *) 'Loaded airfoil won''t be normalized'
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
    if (show_output) then
        write (*, *)
        write (*, *) '***  No airfoil available  ***'
    endif
    cycle
    endif
    !
    call bendump(N, X, Y)
    !
    !===============================================
    elseif (comand=='BENP') then
    if (N==0) then
    if (show_output) then
        write (*, *)
        write (*, *) '***  No airfoil available  ***'
    endif
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
    if (show_output) write (*, 99003) FNAme(1:nfn)
    99003  format (/'  File  ', a, '  exists.  Overwrite?  Y')
    read (*, 99004) ans
    !
    99004  format (a)
    if (index('Nn', ans)==0) goto 40
    if (show_output) then
        write (*, *)
        write (*, *) 'No action taken'
    endif
    close (lu)
    !
    20    open (lu, file = FNAme, status = 'UNKNOWN')
    40    call wrtdef(lu)
    if (show_output) write (*, 99005) FNAme(1:nfn)
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
    if (show_output) write (*, 99006) comand
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
end subroutine xfoil
!*==INIT.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! XFOIL

subroutine init() bind(c, name='init')
    use s_xfoil, only: mrcl, comset
    use m_xbl, only: blpini, preptrs
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Local variables
    !
    real :: ann
    integer :: i, ip, k, nn
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
    !---- prepare BL pointers
    call preptrs
    !
    !---------------------------------------------------
    !     Variable initialization/default routine.
    !     See file XFOIL.INC for variable description.
    !---------------------------------------------------
    !
    PI = 4.0 * atan(1.0)
    HOPi = 0.50 / PI
    QOPi = 0.25 / PI
    DTOr = PI / 180.0
    !
    !---- default Cp/Cv (air)
    GAMma = 1.4
    GAMm1 = GAMma - 1.0
    !
    !---- set unity freestream speed
    QINf = 1.0
    !
    !---- initialize freestream Mach number to zero
    MATyp = 1
    MINf1 = 0.
    !
    ALFa = 0.0
    COSa = 1.0
    SINa = 0.0
    !
    do i = 1, IQX
        GAMu(i, 1) = 0.
        GAMu(i, 2) = 0.
        GAM(i) = 0.
        GAM_a(i) = 0.
    enddo
    PSIo = 0.
    !
    CL = 0.
    CM = 0.
    CD = 0.
    !
    SIGte = 0.0
    GAMte = 0.0
    SIGte_a = 0.
    GAMte_a = 0.
    !
    do i = 1, IZX
        SIG(i) = 0.
    enddo
    !
    NQSp = 0
    do k = 1, IPX
        ALQsp(k) = 0.
        CLQsp(k) = 0.
        CMQsp(k) = 0.
        do i = 1, IBX
            QSPec(i, k) = 0.
        enddo
    enddo
    !
    AWAke = 0.0
    AVIsc = 0.0
    !
    KIMage = 1
    YIMage = -10.0
    LIMage = .false.
    !
    !---- output coordinate file delimiters
    !-    KDELIM =  0  blanks
    !-           =  1  commas
    !-           =  2  tabs
    KDElim = 0
    !
    LGAmu = .false.
    LQInu = .false.
    LVIsc = .false.
    LWAke = .false.
    LPAcc = .false.
    LBLini = .false.
    LIPan = .false.
    LQAij = .false.
    LADij = .false.
    LWDij = .false.
    LCPxx = .false.
    LQVdes = .false.
    LQSpec = .false.
    LQRefl = .false.
    LVConv = .false.
    LCPref = .false.
    LFOref = .false.
    LPFile = .false.
    LPFilx = .false.
    LPPsho = .false.
    LBFlap = .false.
    LFLap = .false.
    LEIw = .false.
    LSCini = .false.
    LPLot = .false.
    LCLip = .false.
    LVLab = .true.
    LCMinp = .false.
    LHMomp = .false.
    LFReqp = .true.
    !
    LCUrs = .true.
    LLAnd = .true.
    LGSame = .false.
    !
    LGParm = .true.
    LPLcam = .false.
    !
    !---- input airfoil will not be normalized
    LNOrm = .false.
    !
    !---- airfoil will not be forced symmetric
    LQSym = .false.
    LGSym = .false.
    !
    !---- endpoint slopes will be matched
    LQSlop = .true.
    LGSlop = .true.
    LCSlop = .true.
    !
    !---- grids on Qspec(s) and buffer airfoil geometry plots will be plotted
    LQGrid = .true.
    LGGrid = .true.
    LGTick = .true.
    !
    !---- no grid on Cp plots
    LCPgrd = .false.
    !
    !---- overlay inviscid Cp on viscous Cp(x) plot
    LCPinv = .true.

    !---- grid and no symbols are to be used on BL variable plots
    LBLgrd = .true.
    LBLsym = .false.
    !
    !---- buffer and current airfoil flap hinge coordinates
    XBF = 0.0
    YBF = 0.0
    XOF = 0.0
    YOF = 0.0
    !
    NCPref = 0
    !                                               n
    !---- circle plane array size (257, or largest 2  + 1 that will fit array size)
    ann = log(float((2 * IQX) - 1)) / log(2.0)
    nn = int(ann + 0.00001)
    NC1 = 2**nn + 1
    NC1 = min(NC1, 257)
    !
    !---- default paneling parameters
    NPAn = 160
    CVPar = 1.0
    CTErat = 0.15
    CTRrat = 0.2
    !
    !---- default paneling refinement zone x/c endpoints
    XSRef1 = 1.0
    XSRef2 = 1.0
    XPRef1 = 1.0
    XPRef2 = 1.0
    !
    !---- no polars present to begin with
    NPOl = 0
    IPAct = 0
    do ip = 1, NPX
        PFName(ip) = ' '
        PFNamx(ip) = ' '
    enddo
    !
    !---- no reference polars
    NPOlref = 0
    !
    !---- plot aspect ratio, character size
    PLOtar = 0.55
    CH = 0.015
    !
    !---- airfoil node tick-mark size (as fraction of arc length)
    GTIck = 0.0005
    !
    !---- Cp limits in  Cp vs x  plot
    CPMax = 1.0
    CPMin = -2.0
    CPDel = -0.5
    PFAc = PLOtar / (CPMax - CPMin)
    !
    !---- Ue limits in  Ue vs x  plot
    UEMax = 1.8
    UEMin = -1.0
    UEDel = 0.2
    UFAc = PLOtar / (UEMax - UEMin)
    !
    !---- DCp limits in CAMB loading plot
    YPMin = -0.4
    YPMax = 0.4
    !
    !---- scaling factor for Cp vector plot
    VFAc = 0.25
    !
    !---- offsets and scale factor for airfoil in  Cp vs x  plot
    XOFair = 0.09
    YOFair = -.01
    FACair = 0.70
    !
    !---- u/Qinf scale factor for profile plotting
    UPRwt = 0.02
    !
    !---- polar variables to be written to polar save file
    IPOl(1) = IAL
    IPOl(2) = ICL
    IPOl(3) = ICD
    IPOl(4) = ICP
    IPOl(5) = ICM
    NIPol = 5
    NIPol0 = 5
    !
    JPOl(1) = JTN
    JPOl(2) = JTI
    NJPol = 2
    !
    !---- default Cm reference location
    XCMref = 0.25
    YCMref = 0.
    !
    !---- default viscous parameters
    RETyp = 1
    REInf1 = 0.
    ACRit(1) = 9.0
    ACRit(2) = 9.0
    XSTrip(1) = 1.0
    XSTrip(2) = 1.0
    XOCtr(1) = 1.0
    XOCtr(2) = 1.0
    YOCtr(1) = 0.
    YOCtr(2) = 0.
    WAKlen = 1.0
    !
    IDAmp = 0
    !
    !---- select default BL plotting coordinate (can be changed in VPLO)
    IXBlp = 1       !  x
    !cc   IXBLP = 2   !  s
    !
    !---- set BL calibration parameters
    call blpini
    !
    !---- Newton iteration limit
    ITMax = 20
    !
    !---- max number of unconverged sequence points for early exit
    NSEqex = 4
    !
    !---- drop tolerance for BL system solver
    VACcel = 0.01
    !
    !---- inverse-mapping auto-filter level
    FFIlt = 0.0
    !
    !---- default overlay airfoil filename
    ONAme = ' '
    !
    !---- default filename prefix
    PREfix = ' '
    !
    !
    NNAme = 32
    NAMe = '                                '
    !CC             12345678901234567890123456789012
    !
    !---- MSES domain parameters (not used in XFOIL)
    ISPars = ' -2.0  3.0  -2.5  3.5'
    !
    !---- set MINF, REINF, based on current CL-dependence
    call mrcl(1.0, MINf_cl, REInf_cl)
    !
    !---- set various compressibility parameters from MINF
    call comset
    !
end subroutine init
!*==MRCL.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! INIT






subroutine getdef(Lu, Filnam, Lask)
    use s_xfoil, only: mrcl, comset
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Dummy arguments
    !
    character(*) :: Filnam
    logical :: Lask
    integer :: Lu
    intent (in) Filnam, Lask, Lu
    !
    ! Local variables
    !
    real :: aldel, almax, almin, cddel, cdmax, cdmin, cldel, clmax, clmin, cmdel, cmmax, cmmin, rmill
    character(1) :: ans
    integer :: k
    logical :: lcolor
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
    !-----------------------------------------------------
    !     Reads in default parameters from file xfoil.def
    !     If LASK=t, ask user if file is to be read.
    !-----------------------------------------------------
    !
    open (Lu, file = Filnam, status = 'OLD', err = 200)
    if (Lask) then
        if (show_output) write (*, 99001) Filnam
        99001 format (/'  Read settings from file  ', a, ' ?  Y')
        read (*, 99002) ans
        !
        99002 format (a)
        if (index('Nn', ans)/=0) then
            close (Lu)
            return
        endif
    endif
    !
    clmin = CPOlplf(1, ICL)
    clmax = CPOlplf(2, ICL)
    cldel = CPOlplf(3, ICL)
    !
    cdmin = CPOlplf(1, ICD)
    cdmax = CPOlplf(2, ICD)
    cddel = CPOlplf(3, ICD)
    !
    almin = CPOlplf(1, IAL)
    almax = CPOlplf(2, IAL)
    aldel = CPOlplf(3, IAL)
    !
    cmmin = CPOlplf(1, ICM)
    cmmax = CPOlplf(2, ICM)
    cmdel = CPOlplf(3, ICM)
    !
    !---- default paneling parameters (viscous)
    read (Lu, *, err = 100) NPAn, CVPar, CTErat, CTRrat
    read (Lu, *, err = 100) XSRef1, XSRef2, XPRef1, XPRef2
    !
    !---- plotting parameters
    read (Lu, *, err = 100) SIZe, PLOtar, CH, SCRnfr
    !
    !---- plot sizes
    read (Lu, *, err = 100) XPAge, YPAge, XMArg, YMArg
    !
    !---- plot flags
    read (Lu, *, err = 100) lcolor, LCUrs
    !
    !---- Cp limits in  Cp vs x  plot
    read (Lu, *, err = 100) CPMax, CPMin, CPDel
    PFAc = PLOtar / (CPMax - CPMin)
    !
    !---- airfoil x-offset and scale factor in Cp vs x plot, BL profile weight
    read (Lu, *, err = 100) XOFair, FACair, UPRwt
    !
    !---- polar plot CL,CD,alpha,CM  min,max,delta
    read (Lu, *, err = 100) (CPOlplf(k, ICL), k = 1, 3)
    read (Lu, *, err = 100) (CPOlplf(k, ICD), k = 1, 3)
    read (Lu, *, err = 100) (CPOlplf(k, IAL), k = 1, 3)
    read (Lu, *, err = 100) (CPOlplf(k, ICM), k = 1, 3)
    !
    !---- default Mach and viscous parameters
    read (Lu, *, err = 100) MATyp, MINf1, VACcel
    read (Lu, *, err = 100) RETyp, rmill, ACRit(1), ACRit(2)
    read (Lu, *, err = 100) XSTrip(1), XSTrip(2)
    !
    if (lcolor) IDEvrp = 4
    if (.not.lcolor) IDEvrp = 2
    !
    REInf1 = rmill * 1.0E6
    !
    !---- set MINF, REINF
    call mrcl(1.0, MINf_cl, REInf_cl)
    !
    !---- set various compressibility parameters from new MINF
    call comset
    !
    close (Lu)
    if (show_output) write (*, 99003) Filnam
    99003 format (/' Default parameters read in from file  ', a, ':'/)
    call wrtdef(6)
    return
    !
    100  close (Lu)
    if (show_output) write (*, 99004) Filnam
    99004 format (/' File  ', a, '  read error'/' Settings may have been changed')
    return
    !
    200  if (show_output) write (*, 99005) Filnam
    99005 format (/' File  ', a, '  not found')
    !
end subroutine getdef
!*==WRTDEF.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! GETDEF



subroutine wrtdef(Lu)
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Dummy arguments
    !
    integer :: Lu
    intent (in) Lu
    !
    ! Local variables
    !
    integer :: k
    logical :: lcolor
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
    !------------------------------------------
    !     Writes default parameters to unit LU
    !------------------------------------------
    !
    lcolor = IDEvrp==4
    !
    !---- default paneling parameters (viscous)
    write (Lu, 99001) NPAn, CVPar, CTErat, CTRrat
    !...............................................
    99001 format (1x, i5, 4x, f9.4, f9.4, f9.4, ' | Npan    PPanel  TErat  REFrat')
    write (Lu, 99002) XSRef1, XSRef2, XPRef1, XPRef2
    99002 format (1x, f9.4, f9.4, f9.4, f9.4, ' | XrefS1  XrefS2  XrefP1 XrefP2')
    !
    !---- plotting parameters
    write (Lu, 99003) SIZe, PLOtar, CH, SCRnfr
    99003 format (1x, f9.4, f9.4, f9.4, f9.4, ' | Size    plotAR  CHsize ScrnFr')
    !
    !---- plot sizes
    write (Lu, 99004) XPAge, YPAge, XMArg, YMArg
    99004 format (1x, f9.4, f9.4, f9.4, f9.4, ' | Xpage   Ypage   Xmargn Ymargn')
    !
    !---- plot flags
    write (Lu, 99005) lcolor, LCUrs
    99005 format (1x, l2, 7x, l2, 7x, 9x, 9x, ' | Lcolor  Lcursor')
    !
    !---- Cp limits in  Cp vs x  plot
    write (Lu, 99006) CPMax, CPMin, CPDel
    99006 format (1x, f9.4, f9.4, f9.4, 9x, ' | CPmax   CPmin   CPdel')
    !
    !---- x-offset and scale factor for airfoil on Cp vs x plot
    write (Lu, 99007) XOFair, FACair, UPRwt
    99007 format (1x, f9.4, f9.4, f9.4, 9x, ' | XoffAir ScalAir BLUwt')
    !
    !---- polar plot CL,CD,alpha,CM  min,max,delta
    write (Lu, 99008) (CPOlplf(k, ICL), k = 1, 3)
    99008 format (1x, f9.4, f9.4, f9.4, 9x, ' | CLmin   CLmax   CLdel')
    write (Lu, 99009) (CPOlplf(k, ICD), k = 1, 3)
    99009 format (1x, f9.4, f9.4, f9.4, 9x, ' | CDmin   CDmax   CDdel')
    write (Lu, 99010) (CPOlplf(k, IAL), k = 1, 3)
    99010 format (1x, f9.4, f9.4, f9.4, 9x, ' | ALmin   ALmax   ALdel')
    write (Lu, 99011) (CPOlplf(k, ICM), k = 1, 3)
    99011 format (1x, f9.4, f9.4, f9.4, 9x, ' | CMmin   CMmax   CMdel')
    !
    !---- default viscous parameters
    write (Lu, 99012) MATyp, MINf1, VACcel
    99012 format (1x, i3, 6x, f9.4, f9.4, 9x, ' | MAtype  Mach    Vaccel')
    write (Lu, 99013) RETyp, REInf1 / 1.0E6, ACRit(1), ACRit(2)
    99013 format (1x, i3, 6x, f9.4, f9.4, f9.4, ' | REtype  Re/10^6 Ncrit1 Ncrit2')
    write (Lu, 99014) XSTrip(1), XSTrip(2)
    99014 format (1x, f9.4, f9.4, 9x, 9x, ' | XtripT  XtripB')
    !
end subroutine wrtdef
!*==COMSET.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! WRTDEF




subroutine load(Filnam, Itype)
    use m_xgeom, only: geopar, norm
    use m_userio, only: strip, asks
    use m_aread, only: aread
    use m_spline, only: seval, segspl, scalc
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Dummy arguments
    !
    character(*) :: Filnam
    integer :: Itype
    intent (in) Filnam
    !
    ! Local variables
    !
    real :: area, xble, xbte, xinl, xout, xtmp, yble, ybot, ybte, ytmp, ytop
    integer :: i, ip, kdot, lu
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
    !------------------------------------------------------
    !     Reads airfoil file into buffer airfoil
    !     and does various initial processesing on it.
    !------------------------------------------------------
    !
    FNAme = Filnam
    if (FNAme(1:1)==' ') call asks('Enter filename^', FNAme)
    !
    lu = 9
    call aread(lu, FNAme, IBX, XB, YB, NB, NAMe, ISPars, Itype, 1)
    if (Itype==0) return
    !
    if (Itype==1) call asks('Enter airfoil name^', NAMe)
    call strip(NAMe, NNAme)
    !
    !---- set default prefix for other filenames
    kdot = index(FNAme, '.')
    if (kdot==0) then
        PREfix = FNAme
    else
        PREfix = FNAme(1:kdot - 1)
    endif
    call strip(PREfix, NPRefix)
    !
    !---- calculate airfoil area assuming counterclockwise ordering
    area = 0.0
    do i = 1, NB
        ip = i + 1
        if (i==NB) ip = 1
        area = area + 0.5 * (YB(i) + YB(ip)) * (XB(i) - XB(ip))
    enddo
    !
    if (area>=0.0) then
        LCLock = .false.
        if (show_output) write (*, 99001) NB
        !...............................................................
        99001 format (/' Number of input coordinate points:', i4/' Counterclockwise ordering')
    else
        !----- if area is negative (clockwise order), reverse coordinate order
        LCLock = .true.
        if (show_output) write (*, 99002) NB
        99002 format (/' Number of input coordinate points:', i4/' Clockwise ordering')
        do i = 1, NB / 2
            xtmp = XB(NB - i + 1)
            ytmp = YB(NB - i + 1)
            XB(NB - i + 1) = XB(i)
            YB(NB - i + 1) = YB(i)
            XB(i) = xtmp
            YB(i) = ytmp
        enddo
    endif
    !
    if (LNOrm) then
        call norm(XB, XBP, YB, YBP, SB, NB)
        if (show_output) write (*, 99003)
        99003 format (/' Airfoil has been normalized')
    endif
    !
    call scalc(XB, YB, SB, NB)
    call segspl(XB, XBP, SB, NB)
    call segspl(YB, YBP, SB, NB)
    !
    call geopar(XB, XBP, YB, YBP, SB, NB, W1, SBLe, CHOrdb, AREab, &
            RADble, ANGbte, EI11ba, EI22ba, APX1ba, APX2ba, EI11bt, EI22bt, APX1bt, &
            & APX2bt, THIckb, CAMbrb)
    !
    xble = seval(SBLe, XB, XBP, SB, NB)
    yble = seval(SBLe, YB, YBP, SB, NB)
    xbte = 0.5 * (XB(1) + XB(NB))
    ybte = 0.5 * (YB(1) + YB(NB))
    !
    if (show_output) write (*, 99004) xble, yble, CHOrdb, xbte, ybte
    99004 format (/'  LE  x,y  =', 2F10.5, '  |   Chord =', f10.5/'  TE  x,y  =', 2F10.5, '  |')
    !
    !---- set reasonable MSES domain parameters for non-MSES coordinate file
    if (Itype<=2 .and. ISPars==' ') then
        xble = seval(SBLe, XB, XBP, SB, NB)
        yble = seval(SBLe, YB, YBP, SB, NB)
        xinl = xble - 2.0 * CHOrdb
        xout = xble + 3.0 * CHOrdb
        ybot = yble - 2.5 * CHOrdb
        ytop = yble + 3.5 * CHOrdb
        xinl = aint(20.0 * abs(xinl / CHOrdb) + 0.5) / 20.0 * sign(CHOrdb, xinl)
        xout = aint(20.0 * abs(xout / CHOrdb) + 0.5) / 20.0 * sign(CHOrdb, xout)
        ybot = aint(20.0 * abs(ybot / CHOrdb) + 0.5) / 20.0 * sign(CHOrdb, ybot)
        ytop = aint(20.0 * abs(ytop / CHOrdb) + 0.5) / 20.0 * sign(CHOrdb, ytop)
        write (ISPars, 99005) xinl, xout, ybot, ytop
        99005 format (1x, 4F8.2)
    endif
    !
    !---- wipe out old flap hinge location
    XBF = 0.0
    YBF = 0.0
    LBFlap = .false.
    !
    !---- wipe out off-design alphas, CLs
    !c      NALOFF = 0
    !c      NCLOFF = 0
    !
end subroutine load
!*==SAVE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! LOAD



subroutine save(Iftyp, Fname1)
    use m_userio, only: asks, bstrip
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Dummy arguments
    !
    character(*) :: Fname1
    integer :: Iftyp
    intent (in) Fname1, Iftyp
    !
    ! Local variables
    !
    character(1) :: ans, delim
    integer :: i, ibeg, iend, incr, k, lu, nline
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
    !--------------------------------
    !     Writes out current airfoil
    !--------------------------------
    !
    !
    if (KDElim==0) then
        delim = ' '
    elseif (KDElim==1) then
        delim = ','
    elseif (KDElim==2) then
        delim = char(9)
    else
        if (show_output) write (*, *) '? Illegal delimiter.  Using blank.'
        delim = ' '
    endif
    !
    !
    lu = 2
    !
    !---- get output filename if it was not supplied
    if (Fname1(1:1)/=' ') then
        FNAme = Fname1
    else
        call asks('Enter output filename^', FNAme)
    endif
    !
    open (lu, file = FNAme, status = 'OLD', err = 100)
    if (show_output) then
        write (*, *)
        write (*, *) 'Output file exists.  Overwrite?  Y'
    endif
    read (*, 99004) ans
    if (index('Nn', ans)==0) goto 200
    !
    close (lu)
    if (show_output) write (*, *) 'Current airfoil not saved.'
    return
    !
    100  open (lu, file = FNAme, status = 'NEW', err = 300)
    200  rewind (lu)
    !
    !----- write name to first line
    if (Iftyp>=1) write (lu, 99004) NAMe(1:NNAme)
    !
    if (Iftyp>=2) then
        !----- write MSES domain parameters to second line
        do k = 80, 1, -1
            if (index(ISPars(k:k), ' ')/=1) exit
        enddo
        !
        write (lu, 99004) ISPars(1:k)
    endif
    !
    if (LCLock) then
        !----- write out in clockwise order (reversed from internal XFOIL order)
        ibeg = N
        iend = 1
        incr = -1
    else
        !----- write out in counterclockwise order (same as internal XFOIL order)
        ibeg = 1
        iend = N
        incr = 1
    endif
    !
    if (Iftyp==-1) then
        do i = ibeg, iend, incr
            write (lu, 99001) int(X(i) + sign(0.5, X(i))), int(Y(i) + sign(0.5, Y(i)))
            99001  format (1x, i12, i12)
        enddo
        !
    else
        do i = ibeg, iend, incr
            if (KDElim==0) then
                write (lu, 99002) X(i), Y(i)
                99002      format (1x, g15.7, g15.7)
                !
            else
                write (line, 99003) X(i), delim, Y(i)
                99003      format (1x, f10.6, a, f10.6)
                call bstrip(line, nline)
                write (lu, 99004) line(1:nline)
            endif
        enddo
    endif
    !
    close (lu)
    return
    !
    300  if (show_output) write (*, *) 'Bad filename.'
    if (show_output) write (*, *) 'Current airfoil not saved.'
    return
    !
    99004 format (a)
end subroutine save
!*==ROTATE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! SAVE



subroutine rotate(X, Y, N, Alfa)
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Dummy arguments
    !
    real :: Alfa
    integer :: N
    real, dimension(N) :: X, Y
    intent (in) Alfa, N
    intent (inout) X, Y
    !
    ! Local variables
    !
    real :: ca, sa, xoff, xt, yoff, yt
    integer :: i
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
    sa = sin(Alfa)
    ca = cos(Alfa)
    !CC      XOFF = 0.25*(1.0-CA)
    !CC      YOFF = 0.25*SA
    xoff = 0.
    yoff = 0.
    do i = 1, N
        xt = X(i)
        yt = Y(i)
        X(i) = ca * xt + sa * yt + xoff
        Y(i) = ca * yt - sa * xt + yoff
    enddo
    !
end subroutine rotate
!*==NACA.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


subroutine naca(Ides1)
    use m_xgeom, only: geopar
    use m_userio, only: aski, strip
    use m_spline, only: segspl, scalc
    use m_naca, only: naca4, naca5
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Dummy arguments
    !
    integer :: Ides1
    intent (in) Ides1
    !
    ! Local variables
    !
    integer :: ides, itype, nside
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
    !---- number of points per side
    nside = IQX / 3
    !
    if (Ides1<=0) then
        call aski('Enter NACA 4 or 5-digit airfoil designation^', ides)
    else
        ides = Ides1
    endif
    !
    itype = 0
    if (ides<=25099) itype = 5
    if (ides<=9999) itype = 4
    !
    if (itype==0) then
        if (show_output) write (*, *) 'This designation not implemented.'
        return
    endif
    !
    if (itype==4) call naca4(ides, W1, W2, W3, nside, XB, YB, NB, NAMe)
    if (itype==5) call naca5(ides, W1, W2, W3, nside, XB, YB, NB, NAMe)
    call strip(NAMe, NNAme)
    !
    !---- see if routines didn't recognize designator
    if (ides==0) return
    !
    LCLock = .false.
    !
    XBF = 0.0
    YBF = 0.0
    LBFlap = .false.
    !
    call scalc(XB, YB, SB, NB)
    call segspl(XB, XBP, SB, NB)
    call segspl(YB, YBP, SB, NB)
    !
    call geopar(XB, XBP, YB, YBP, SB, NB, W1, SBLe, CHOrdb, AREab, &
            RADble, ANGbte, EI11ba, EI22ba, APX1ba, APX2ba, EI11bt, EI22bt, APX1bt, &
            & APX2bt, THIckb, CAMbrb)
    !
    if (show_output) write (*, 99001) NB
    99001 format (/' Buffer airfoil set using', i4, ' points')
    !
    !---- set paneling
    call pangen(.true.)
    !
end subroutine naca
!*==PANGEN.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! NACA


subroutine pangen(Shopar)
    use m_xpanel, only: apcalc, ncalc
    use m_xgeom, only: lefind
    use m_spline, only: curv, seval, scalc, trisol, segspl, deval
    use s_xfoil, only: tecalc
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Dummy arguments
    !
    logical :: Shopar
    intent (in) Shopar
    !
    ! Local variables
    !
    real :: cavm, cavm_s1, cavm_s2, cavp, cavp_s2, cavp_s3, cc, chbsq, cv1, cv2, cv3, cvavg, cvk, cvle, &
            & cvmax, cvs1, cvs2, cvs3, cvsum, cvte, dds, dmax, ds, dsavg, dsavg1, dsavg2, dsm, dsmax, dsmin, &
            & dso, dsp, dsrat, fm, fp, frac, gap, rdste, rez, rtf, sbcorn, sbk, sbref, smool, smoosq, &
            & xbcorn, xble, xbte, xoc, ybcorn, yble, ybte
    integer :: i, ib, ible, ind, ipfac, iter, j, k, ncorn, nfrac1, nk, nn, nn1, nn2
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
    !---------------------------------------------------
    !     Set paneling distribution from buffer airfoil
    !     geometry, thus creating current airfoil.
    !
    !     If REFINE=True, bunch points at x=XSREF on
    !     top side and at x=XPREF on bottom side
    !     by setting a fictitious local curvature of
    !     CTRRAT*(LE curvature) there.
    !---------------------------------------------------
    !
    if (NB<2) then
        if (show_output) write (*, *) 'PANGEN: Buffer airfoil not available.'
        N = 0
        return
    endif
    !
    !---- Number of temporary nodes for panel distribution calculation
    !       exceeds the specified panel number by factor of IPFAC.
    ipfac = 3
    ipfac = 5
    !
    !---- number of airfoil panel points
    N = NPAn
    !
    !C---- number of wake points
    !      NW = NPAN/8 + 2
    !      IF(NW.GT.IWX) THEN
    !       WRITE(*,*)
    !     &  'Array size (IWX) too small.  Last wake point index reduced.'
    !       NW = IWX
    !      ENDIF
    !
    !---- set arc length spline parameter
    call scalc(XB, YB, SB, NB)
    !
    !---- spline raw airfoil coordinates
    call segspl(XB, XBP, SB, NB)
    call segspl(YB, YBP, SB, NB)
    !
    !---- normalizing length (~ chord)
    sbref = 0.5 * (SB(NB) - SB(1))
    !
    !---- set up curvature array
    do i = 1, NB
        W5(i) = abs(curv(SB(i), XB, XBP, YB, YBP, SB, NB)) * sbref
    enddo
    !
    !---- locate LE point arc length value and the normalized curvature there
    call lefind(SBLe, XB, XBP, YB, YBP, SB, NB)
    cvle = abs(curv(SBLe, XB, XBP, YB, YBP, SB, NB)) * sbref
    !
    !---- check for doubled point (sharp corner) at LE
    ible = 0
    do i = 1, NB - 1
        if (SBLe==SB(i) .and. SBLe==SB(i + 1)) then
            ible = i
            if (show_output) then
                write (*, *)
                write (*, *) 'Sharp leading edge'
            endif
            exit
        endif
    enddo
    !
    !---- set LE, TE points
    xble = seval(SBLe, XB, XBP, SB, NB)
    yble = seval(SBLe, YB, YBP, SB, NB)
    xbte = 0.5 * (XB(1) + XB(NB))
    ybte = 0.5 * (YB(1) + YB(NB))
    chbsq = (xbte - xble)**2 + (ybte - yble)**2
    !
    !---- set average curvature over 2*NK+1 points within Rcurv of LE point
    nk = 3
    cvsum = 0.
    do k = -nk, nk
        frac = float(k) / float(nk)
        sbk = SBLe + frac * sbref / max(cvle, 20.0)
        cvk = abs(curv(sbk, XB, XBP, YB, YBP, SB, NB)) * sbref
        cvsum = cvsum + cvk
    enddo
    cvavg = cvsum / float(2 * nk + 1)
    !
    !---- dummy curvature for sharp LE
    if (ible/=0) cvavg = 10.0
    !
    !---- set curvature attraction coefficient actually used
    cc = 6.0 * CVPar
    !
    !---- set artificial curvature at TE to bunch panels there
    cvte = cvavg * CTErat
    W5(1) = cvte
    W5(NB) = cvte
    !
    !
    !**** smooth curvature array for smoother panel size distribution  ****
    !
    !CC      CALL ASKR('Enter curvature smoothing length/c^',SMOOL)
    !CC      SMOOL = 0.010
    !
    !---- set smoothing length = 1 / averaged LE curvature, but
    !-    no more than 5% of chord and no less than 1/4 average panel spacing
    smool = max(1.0 / max(cvavg, 20.0), 0.25 / float(NPAn / 2))
    !
    smoosq = (smool * sbref)**2
    !
    !---- set up tri-diagonal system for smoothed curvatures
    W2(1) = 1.0
    W3(1) = 0.0
    do i = 2, NB - 1
        dsm = SB(i) - SB(i - 1)
        dsp = SB(i + 1) - SB(i)
        dso = 0.5 * (SB(i + 1) - SB(i - 1))
        !
        if (dsm==0.0 .or. dsp==0.0) then
            !------- leave curvature at corner point unchanged
            W1(i) = 0.0
            W2(i) = 1.0
            W3(i) = 0.0
        else
            W1(i) = smoosq * (-1.0 / dsm) / dso
            W2(i) = smoosq * (1.0 / dsp + 1.0 / dsm) / dso + 1.0
            W3(i) = smoosq * (-1.0 / dsp) / dso
        endif
    enddo
    !
    W1(NB) = 0.0
    W2(NB) = 1.0
    !
    !---- fix curvature at LE point by modifying equations adjacent to LE
    do i = 2, NB - 1
        if (SB(i)==SBLe .or. i==ible .or. i==ible + 1) then
            !------- if node falls right on LE point, fix curvature there
            W1(i) = 0.
            W2(i) = 1.0
            W3(i) = 0.
            W5(i) = cvle
        elseif (SB(i - 1)<SBLe .and. SB(i)>SBLe) then
            !------- modify equation at node just before LE point
            dsm = SB(i - 1) - SB(i - 2)
            dsp = SBLe - SB(i - 1)
            dso = 0.5 * (SBLe - SB(i - 2))
            !
            W1(i - 1) = smoosq * (-1.0 / dsm) / dso
            W2(i - 1) = smoosq * (1.0 / dsp + 1.0 / dsm) / dso + 1.0
            W3(i - 1) = 0.
            W5(i - 1) = W5(i - 1) + smoosq * cvle / (dsp * dso)
            !
            !------- modify equation at node just after LE point
            dsm = SB(i) - SBLe
            dsp = SB(i + 1) - SB(i)
            dso = 0.5 * (SB(i + 1) - SBLe)
            W1(i) = 0.
            W2(i) = smoosq * (1.0 / dsp + 1.0 / dsm) / dso + 1.0
            W3(i) = smoosq * (-1.0 / dsp) / dso
            W5(i) = W5(i) + smoosq * cvle / (dsm * dso)
            !
            exit
        endif
    enddo
    !
    !---- set artificial curvature at bunching points and fix it there
    do i = 2, NB - 1
        !------ chord-based x/c coordinate
        xoc = ((XB(i) - xble) * (xbte - xble) + (YB(i) - yble) * (ybte - yble)) / chbsq
        !
        if (SB(i)<SBLe) then
            !------- check if top side point is in refinement area
            if (xoc>XSRef1 .and. xoc<XSRef2) then
                W1(i) = 0.
                W2(i) = 1.0
                W3(i) = 0.
                W5(i) = cvle * CTRrat
            endif
            !------- check if bottom side point is in refinement area
        elseif (xoc>XPRef1 .and. xoc<XPRef2) then
            W1(i) = 0.
            W2(i) = 1.0
            W3(i) = 0.
            W5(i) = cvle * CTRrat
        endif
    enddo
    !
    !---- solve for smoothed curvature array W5
    if (ible==0) then
        call trisol(W2, W1, W3, W5, NB)
    else
        i = 1
        call trisol(W2(i), W1(i), W3(i), W5(i), ible)
        i = ible + 1
        call trisol(W2(i), W1(i), W3(i), W5(i), NB - ible)
    endif
    !
    !---- find max curvature
    cvmax = 0.
    do i = 1, NB
        cvmax = max(cvmax, abs(W5(i)))
    enddo
    !
    !---- normalize curvature array
    do i = 1, NB
        W5(i) = W5(i) / cvmax
    enddo
    !
    !---- spline curvature array
    call segspl(W5, W6, SB, NB)
    !
    !---- Set initial guess for node positions uniform in s.
    !     More nodes than specified (by factor of IPFAC) are
    !     temporarily used  for more reliable convergence.
    nn = ipfac * (N - 1) + 1
    !
    !---- ratio of lengths of panel at TE to one away from the TE
    rdste = 0.667
    rtf = (rdste - 1.0) * 2.0 + 1.0
    !
    if (ible==0) then
        !
        dsavg = (SB(NB) - SB(1)) / (float(nn - 3) + 2.0 * rtf)
        SNEw(1) = SB(1)
        do i = 2, nn - 1
            SNEw(i) = SB(1) + dsavg * (float(i - 2) + rtf)
        enddo
        SNEw(nn) = SB(NB)
        !
    else
        !
        nfrac1 = (N * ible) / NB
        !
        nn1 = ipfac * (nfrac1 - 1) + 1
        dsavg1 = (SBLe - SB(1)) / (float(nn1 - 2) + rtf)
        SNEw(1) = SB(1)
        do i = 2, nn1
            SNEw(i) = SB(1) + dsavg1 * (float(i - 2) + rtf)
        enddo
        !
        nn2 = nn - nn1 + 1
        dsavg2 = (SB(NB) - SBLe) / (float(nn2 - 2) + rtf)
        do i = 2, nn2 - 1
            SNEw(i - 1 + nn1) = SBLe + dsavg2 * (float(i - 2) + rtf)
        enddo
        SNEw(nn) = SB(NB)
        !
    endif
    !
    !---- Newton iteration loop for new node positions
    do iter = 1, 20
        !
        !------ set up tri-diagonal system for node position deltas
        cv1 = seval(SNEw(1), W5, W6, SB, NB)
        cv2 = seval(SNEw(2), W5, W6, SB, NB)
        cvs1 = deval(SNEw(1), W5, W6, SB, NB)
        cvs2 = deval(SNEw(2), W5, W6, SB, NB)
        !
        cavm = sqrt(cv1**2 + cv2**2)
        if (cavm==0.0) then
            cavm_s1 = 0.
            cavm_s2 = 0.
        else
            cavm_s1 = cvs1 * cv1 / cavm
            cavm_s2 = cvs2 * cv2 / cavm
        endif
        !
        do i = 2, nn - 1
            dsm = SNEw(i) - SNEw(i - 1)
            dsp = SNEw(i) - SNEw(i + 1)
            cv3 = seval(SNEw(i + 1), W5, W6, SB, NB)
            cvs3 = deval(SNEw(i + 1), W5, W6, SB, NB)
            !
            cavp = sqrt(cv3**2 + cv2**2)
            if (cavp==0.0) then
                cavp_s2 = 0.
                cavp_s3 = 0.
            else
                cavp_s2 = cvs2 * cv2 / cavp
                cavp_s3 = cvs3 * cv3 / cavp
            endif
            !
            fm = cc * cavm + 1.0
            fp = cc * cavp + 1.0
            !
            rez = dsp * fp + dsm * fm
            !
            !-------- lower, main, and upper diagonals
            W1(i) = -fm + cc * dsm * cavm_s1
            W2(i) = fp + fm + cc * (dsp * cavp_s2 + dsm * cavm_s2)
            W3(i) = -fp + cc * dsp * cavp_s3
            !
            !-------- residual, requiring that
            !         (1 + C*curv)*deltaS is equal on both sides of node i
            W4(i) = -rez
            !
            cv1 = cv2
            cv2 = cv3
            cvs1 = cvs2
            cvs2 = cvs3
            cavm = cavp
            cavm_s1 = cavp_s2
            cavm_s2 = cavp_s3
        enddo
        !
        !------ fix endpoints (at TE)
        W2(1) = 1.0
        W3(1) = 0.0
        W4(1) = 0.0
        W1(nn) = 0.0
        W2(nn) = 1.0
        W4(nn) = 0.0
        !
        if (rtf/=1.0) then
            !------- fudge equations adjacent to TE to get TE panel length ratio RTF
            !
            i = 2
            W4(i) = -((SNEw(i) - SNEw(i - 1)) + rtf * (SNEw(i) - SNEw(i + 1)))
            W1(i) = -1.0
            W2(i) = 1.0 + rtf
            W3(i) = -rtf
            !
            i = nn - 1
            W4(i) = -((SNEw(i) - SNEw(i + 1)) + rtf * (SNEw(i) - SNEw(i - 1)))
            W3(i) = -1.0
            W2(i) = 1.0 + rtf
            W1(i) = -rtf
        endif
        !
        !
        !------ fix sharp LE point
        if (ible/=0) then
            i = nn1
            W1(i) = 0.0
            W2(i) = 1.0
            W3(i) = 0.0
            W4(i) = SBLe - SNEw(i)
        endif
        !
        !------ solve for changes W4 in node position arc length values
        call trisol(W2, W1, W3, W4, nn)
        !
        !------ find under-relaxation factor to keep nodes from changing order
        RLX = 1.0
        dmax = 0.0
        do i = 1, nn - 1
            ds = SNEw(i + 1) - SNEw(i)
            dds = W4(i + 1) - W4(i)
            dsrat = 1.0 + RLX * dds / ds
            if (dsrat>4.0) RLX = (4.0 - 1.0) * ds / dds
            if (dsrat<0.2) RLX = (0.2 - 1.0) * ds / dds
            dmax = max(abs(W4(i)), dmax)
        enddo
        !
        !------ update node position
        do i = 2, nn - 1
            SNEw(i) = SNEw(i) + RLX * W4(i)
        enddo
        !
        !CC        IF(RLX.EQ.1.0) WRITE(*,*) DMAX
        !CC        IF(RLX.NE.1.0) WRITE(*,*) DMAX,'    RLX =',RLX
        if (abs(dmax)<1.E-3) goto 100
    enddo
    if (show_output) write (*, *) 'Paneling convergence failed.  Continuing anyway...'
    !
    !
    !---- set new panel node coordinates
    100  do i = 1, N
        ind = ipfac * (i - 1) + 1
        S(i) = SNEw(ind)
        X(i) = seval(SNEw(ind), XB, XBP, SB, NB)
        Y(i) = seval(SNEw(ind), YB, YBP, SB, NB)
    enddo
    !
    !
    !---- go over buffer airfoil again, checking for corners (double points)
    ncorn = 0
    do ib = 1, NB - 1
        if (SB(ib)==SB(ib + 1)) then
            !------- found one !
            !
            ncorn = ncorn + 1
            xbcorn = XB(ib)
            ybcorn = YB(ib)
            sbcorn = SB(ib)
            !
            !------- find current-airfoil panel which contains corner
            do i = 1, N
                !
                !--------- keep stepping until first node past corner
                if (S(i)>sbcorn) then
                    !
                    !---------- move remainder of panel nodes to make room for additional node
                    do j = N, i, -1
                        X(j + 1) = X(j)
                        Y(j + 1) = Y(j)
                        S(j + 1) = S(j)
                    enddo
                    N = N + 1
                    !
                    if (N>IQX - 1) stop 'PANEL: Too many panels. Increase IQX in XFOIL.INC'
                    !
                    X(i) = xbcorn
                    Y(i) = ybcorn
                    S(i) = sbcorn
                    !
                    !---------- shift nodes adjacent to corner to keep panel sizes comparable
                    if (i - 2>=1) then
                        S(i - 1) = 0.5 * (S(i) + S(i - 2))
                        X(i - 1) = seval(S(i - 1), XB, XBP, SB, NB)
                        Y(i - 1) = seval(S(i - 1), YB, YBP, SB, NB)
                    endif
                    !
                    if (i + 2<=N) then
                        S(i + 1) = 0.5 * (S(i) + S(i + 2))
                        X(i + 1) = seval(S(i + 1), XB, XBP, SB, NB)
                        Y(i + 1) = seval(S(i + 1), YB, YBP, SB, NB)
                    endif
                    !
                    !---------- go on to next input geometry point to check for corner
                    exit
                endif
                !
            enddo
        endif
    enddo
    !
    call scalc(X, Y, S, N)
    call segspl(X, XP, S, N)
    call segspl(Y, YP, S, N)
    call lefind(SLE, X, XP, Y, YP, S, N)
    !
    XLE = seval(SLE, X, XP, S, N)
    YLE = seval(SLE, Y, YP, S, N)
    XTE = 0.5 * (X(1) + X(N))
    YTE = 0.5 * (Y(1) + Y(N))
    CHOrd = sqrt((XTE - XLE)**2 + (YTE - YLE)**2)
    !
    !---- calculate panel size ratios (user info)
    dsmin = 1000.0
    dsmax = -1000.0
    do i = 1, N - 1
        ds = S(i + 1) - S(i)
        if (ds/=0.0) then
            dsmin = min(dsmin, ds)
            dsmax = max(dsmax, ds)
        endif
    enddo
    !
    dsmin = dsmin * float(N - 1) / S(N)
    dsmax = dsmax * float(N - 1) / S(N)
    !cc      WRITE(*,*) 'DSmin/DSavg = ',DSMIN,'     DSmax/DSavg = ',DSMAX
    !
    !---- set various flags for new airfoil
    LGAmu = .false.
    LQInu = .false.
    LWAke = .false.
    LQAij = .false.
    LADij = .false.
    LWDij = .false.
    LIPan = .false.
    LBLini = .false.
    LVConv = .false.
    LSCini = .false.
    LQSpec = .false.
    LGSame = .false.
    !
    if (LBFlap) then
        XOF = XBF
        YOF = YBF
        LFLap = .true.
    endif
    !
    !---- determine if TE is blunt or sharp, calculate TE geometry parameters
    call tecalc
    !
    !---- calculate normal vectors
    call ncalc(X, Y, S, N, NX, NY)
    !
    !---- calculate panel angles for panel routines
    call apcalc
    !
    if (SHArp) then
        if (show_output) write (*, 99002) 'Sharp trailing edge'
    else
        gap = sqrt((X(1) - X(N))**2 + (Y(1) - Y(N))**2)
        if (show_output) write (*, 99002) 'Blunt trailing edge.  Gap =', gap
    endif
    !
    if (Shopar .and. show_output) write (*, 99001) NPAn, CVPar, CTErat, CTRrat, XSRef1, XSRef2, XPRef1, XPRef2
    99001 format (/' Paneling parameters used...'/&
            '   Number of panel nodes      ', i4/'   Panel bunching parameter   ', &
            &f6.3/'   TE/LE panel density ratio  ', f6.3/'   Refined-area/LE panel density ratio   ', &
            &f6.3/'   Top    side refined area x/c limits ', 2F6.3/'   Bottom side refined area x/c limits ', 2F6.3)
    99002 format (/1x, a, f9.5)
    !
end subroutine pangen
!*==GETPAN.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! PANGEN



subroutine getpan
    use m_userio, only: getflt, askc, askr, aski, getint
    use m_xgeom, only: cang
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Local variables
    !
    real :: amax
    character(128) :: comarg
    logical :: error, lchange
    integer :: i, imax, ninput
    integer, dimension(20) :: iinput
    real, dimension(20) :: rinput
    character(4) :: var
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
    !
    !
    if (NB<=1) then
        if (show_output) write (*, *) 'GETPAN: Buffer airfoil not available.'
        return
    endif
    !
    100  lchange = .false.
    do
        !
        if (show_output) write (*, 99001) NPAn, CVPar, CTErat, CTRrat, XSRef1, XSRef2, XPRef1, XPRef2
        99001 format (/'    Present paneling parameters...'/'  N  i   Number of panel nodes      ', &
                &i4/'  P  r   Panel bunching parameter   ', f6.3/'  T  r   TE/LE panel density ratio  ', &
                &f6.3/'  R  r   Refined area/LE  panel density ratio  ', &
                &f6.3/'  XT rr  Top    side refined area x/c limits   ', &
                &2F6.3/'  XB rr  Bottom side refined area x/c limits   ', 2F6.3)
        do
            !
            call askc('Change what ? (<cr> if nothing else)^', var, comarg)
            !
            do i = 1, 20
                iinput(i) = 0
                rinput(i) = 0.0
            enddo
            ninput = 0
            call getint(comarg, iinput, ninput, error)
            ninput = 0
            call getflt(comarg, rinput, ninput, error)
            !
            if (var=='    ') then
                !
                if (lchange) then
                    !
                    !-------- set new panel distribution, and display max panel corner angle
                    call pangen(.false.)
                    if (N>0) call cang(X, Y, N, 1, imax, amax)
                    !
                    !-------- go back to paneling menu
                    goto 100
                endif
                return
                !
            elseif (var=='N   ' .or. var=='n   ') then
                !
                if (ninput>=1) then
                    NPAn = iinput(1)
                else
                    call aski('Enter number of panel nodes^', NPAn)
                endif
                if (NPAn>IQX - 6) then
                    NPAn = IQX - 6
                    if (show_output) write (*, 99002) NPAn
                    99002          format (1x, ' Number of panel nodes reduced to array limit:', i4)
                endif
                lchange = .true.
                !
            elseif (var=='P   ' .or. var=='p   ') then
                !
                if (ninput>=1) then
                    CVPar = rinput(1)
                else
                    call askr('Enter panel bunching parameter (0 to ~1)^', CVPar)
                endif
                lchange = .true.
                !
            elseif (var=='T   ' .or. var=='t   ') then
                !
                if (ninput>=1) then
                    CTErat = rinput(1)
                else
                    call askr('Enter TE/LE panel density ratio^', CTErat)
                endif
                lchange = .true.
                !
            elseif (var=='R   ' .or. var=='r   ') then
                !
                if (ninput>=1) then
                    CTRrat = rinput(1)
                else
                    call askr('Enter refined-area panel density ratio^', CTRrat)
                endif
                lchange = .true.
                !
            elseif (var=='XT  ' .or. var=='xt  ') then
                !
                if (ninput>=2) then
                    XSRef1 = rinput(1)
                    XSRef2 = rinput(2)
                else
                    call askr('Enter left   top   side refinement limit^', XSRef1)
                    call askr('Enter right  top   side refinement limit^', XSRef2)
                endif
                lchange = .true.
                !
            elseif (var=='XB  ' .or. var=='xb  ') then
                !
                if (ninput>=2) then
                    XPRef1 = rinput(1)
                    XPRef2 = rinput(2)
                else
                    call askr('Enter left  bottom side refinement limit^', XPRef1)
                    call askr('Enter right bottom side refinement limit^', XPRef2)
                endif
                lchange = .true.
                !
            else
                !
                if (show_output) then
                    write (*, *)
                    write (*, *) '***  Input not recognized  ***'
                endif
                goto 200
                !
                !
            endif
        enddo
        exit
    200  enddo
    !
end subroutine getpan
!*==TECALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! GETPAN





subroutine inte
    use m_xgeom, only: geopar, inter, lefind
    use m_userio, only: asks, askr, strip
    use m_aread, only: aread
    use m_spline, only: segspld, segspl, scalc
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Local variables
    !
    character(2) :: cair
    real :: frac
    integer :: i, iair, ip, itype, k, lu, npr
    character(80) :: isparst
    character(48), dimension(2) :: nameint
    integer, dimension(2) :: nint
    character(20) :: promptn
    real, dimension(IBX, 2) :: sint, xint, xpint, yint, ypint
    real, dimension(2) :: sleint
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
    !-----------------------------------------------------------
    !     Interpolates two airfoils into an intermediate shape.
    !     Extrapolation is also possible to a reasonable extent.
    !-----------------------------------------------------------
    !
    lu = 21
    !
    if (show_output) write (*, 99001) NAMe
    !
    99001 format (/'  F  disk file'/'  C  current airfoil  ', a)
    do ip = 1, NPOl
        if (NXYpol(ip)>0) then
            if (show_output) write (*, 99002) ip, NAMepol(ip)
            99002  format (1x, i2, '  polar airfoil    ', a)
        endif
    enddo
    if (NPOl==0) then
        promptn = '" ( F C ):  '
        npr = 12
    elseif (NPOl==1) then
        promptn = '" ( F C 1 ):  '
        npr = 14
    elseif (NPOl==2) then
        promptn = '" ( F C 1 2 ):  '
        npr = 16
    else
        promptn = '" ( F C 1 2.. ):  '
        npr = 18
    endif
    !
    do k = 1, 2
        iair = k - 1
        do
            if (show_output) write (*, 99003) iair, promptn(1:npr)
            !
            99003  format (/'  Select source of airfoil "', i1, a, $)
            read (*, 99004) cair
            !
            99004  format (a)
            !
            if (index('Ff', cair(1:1))/=0) then
                call asks('Enter filename^', FNAme)
                call aread(lu, FNAme, IBX, xint(1, k), yint(1, k), nint(k), nameint(k), isparst, itype, 0)
                if (itype==0) return
                !
            elseif (index('Cc', cair(1:1))/=0) then
                if (N<=1) then
                    if (show_output) write (*, *) 'No current airfoil available'
                    cycle
                endif
                !
                nint(k) = N
                do i = 1, N
                    xint(i, k) = X(i)
                    yint(i, k) = Y(i)
                enddo
                nameint(k) = NAMe
                !
            else
                read (cair, *, err = 100) ip
                if (ip<1 .or. ip>NPOl) goto 100
                if (NXYpol(ip)<=0) goto 100
                nint(k) = NXYpol(ip)
                do i = 1, nint(k)
                    xint(i, k) = CPOlxy(i, 1, ip)
                    yint(i, k) = CPOlxy(i, 2, ip)
                enddo
                nameint(k) = NAMepol(ip)
                !
            endif
            !
            call scalc(xint(1, k), yint(1, k), sint(1, k), nint(k))
            call segspld(xint(1, k), xpint(1, k), sint(1, k), nint(k), -999., -999.)
            call segspld(yint(1, k), ypint(1, k), sint(1, k), nint(k), -999., -999.)
            call lefind(sleint(k), xint(1, k), xpint(1, k), yint(1, k), ypint(1, k), sint(1, k), nint(k))
            exit
        enddo
    enddo
    !
    if (show_output) then
        write (*, *)
        write (*, *) 'airfoil "0":  ', nameint(1)
        write (*, *) 'airfoil "1":  ', nameint(2)
    endif
    frac = 0.5
    call askr('Specify interpolating fraction  0...1^', frac)
    !
    call inter(xint(1, 1), xpint(1, 1), yint(1, 1), ypint(1, 1), sint(1, 1), &
            nint(1), sleint(1), xint(1, 2), xpint(1, 2), yint(1, 2), &
            & ypint(1, 2), sint(1, 2), nint(2), sleint(2), XB, YB, NB, frac)
    !
    call scalc(XB, YB, SB, NB)
    call segspl(XB, XBP, SB, NB)
    call segspl(YB, YBP, SB, NB)
    !
    call geopar(XB, XBP, YB, YBP, SB, NB, W1, SBLe, CHOrdb, AREab, &
            RADble, ANGbte, EI11ba, EI22ba, APX1ba, APX2ba, EI11bt, EI22bt, APX1bt, &
            & APX2bt, THIckb, CAMbrb)
    !
    call asks('Enter new airfoil name^', NAMe)
    call strip(NAMe, NNAme)
    if (show_output) then
        write (*, *)
        write (*, *) 'Result has been placed in buffer airfoil'
        write (*, *) 'Execute PCOP or PANE to set new current airfoil'
    endif
    return
    !
    100  if (show_output) then
        write (*, *)
        write (*, *) 'Invalid response'
    end if
end subroutine inte
!*==INTX.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! INTE


subroutine intx
    use m_xgeom, only: geopar, interx, lefind
    use m_userio, only: asks, askr, strip
    use m_aread, only: aread
    use m_spline, only: segspld, segspl, scalc
    use i_xfoil
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! Local variables
    !
    character(2) :: cair
    real :: frac
    integer :: i, iair, ip, itype, k, lu, npr
    character(80) :: isparst
    character(48), dimension(2) :: nameint
    integer, dimension(2) :: nint
    character(20) :: promptn
    real, dimension(IBX, 2) :: sint, xint, xpint, yint, ypint
    real, dimension(2) :: sleint
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
    !-----------------------------------------------------------
    !     Interpolates two airfoils into an intermediate shape.
    !     Extrapolation is also possible to a reasonable extent.
    !-----------------------------------------------------------
    !
    lu = 21
    !
    if (show_output) write (*, 99001) NAMe
    !
    99001 format (/'  F  disk file'/'  C  current airfoil  ', a)
    do ip = 1, NPOl
        if (NXYpol(ip)>0) then
            if (show_output) write (*, 99002) ip, NAMepol(ip)
            99002  format (1x, i2, '  polar airfoil    ', a)
        endif
    enddo
    if (NPOl==0) then
        promptn = '" ( F C ):  '
        npr = 12
    elseif (NPOl==1) then
        promptn = '" ( F C 1 ):  '
        npr = 14
    elseif (NPOl==2) then
        promptn = '" ( F C 1 2 ):  '
        npr = 16
    else
        promptn = '" ( F C 1 2.. ):  '
        npr = 18
    endif
    !
    do k = 1, 2
        iair = k - 1
        do
            if (show_output) write (*, 99003) iair, promptn(1:npr)
            !
            99003  format (/'  Select source of airfoil "', i1, a, $)
            read (*, 99004, err = 100, end = 100) cair
            !
            99004  format (a)
            !
            if (cair==' ') goto 100
            !
            if (index('Ff', cair(1:1))/=0) then
                call asks('Enter filename^', FNAme)
                call aread(lu, FNAme, IBX, xint(1, k), yint(1, k), nint(k), nameint(k), isparst, itype, 0)
                if (itype==0) return
                !
            elseif (index('Cc', cair(1:1))/=0) then
                if (N<=1) then
                    if (show_output) write (*, *) 'No current airfoil available'
                    cycle
                endif
                !
                nint(k) = N
                do i = 1, N
                    xint(i, k) = X(i)
                    yint(i, k) = Y(i)
                enddo
                nameint(k) = NAMe
                !
            else
                read (cair, *, err = 100) ip
                if (ip<1 .or. ip>NPOl) goto 100
                if (NXYpol(ip)<=0) goto 100
                nint(k) = NXYpol(ip)
                do i = 1, N
                    xint(i, k) = CPOlxy(i, 1, ip)
                    yint(i, k) = CPOlxy(i, 2, ip)
                enddo
                nameint(k) = NAMepol(ip)
                !
            endif
            !
            call scalc(xint(1, k), yint(1, k), sint(1, k), nint(k))
            call segspld(xint(1, k), xpint(1, k), sint(1, k), nint(k), -999., -999.)
            call segspld(yint(1, k), ypint(1, k), sint(1, k), nint(k), -999., -999.)
            call lefind(sleint(k), xint(1, k), xpint(1, k), yint(1, k), ypint(1, k), sint(1, k), nint(k))
            exit
        enddo
    enddo
    !
    if (show_output) then
        write (*, *)
        write (*, *) 'airfoil "0":  ', nameint(1)
        write (*, *) 'airfoil "1":  ', nameint(2)
    endif
    frac = 0.5
    call askr('Specify interpolating fraction  0...1^', frac)
    !
    call interx(xint(1, 1), xpint(1, 1), yint(1, 1), ypint(1, 1), &
            sint(1, 1), nint(1), sleint(1), xint(1, 2), xpint(1, 2), yint(1, 2), &
            & ypint(1, 2), sint(1, 2), nint(2), sleint(2), XB, YB, NB, frac)
    !
    call scalc(XB, YB, SB, NB)
    call segspl(XB, XBP, SB, NB)
    call segspl(YB, YBP, SB, NB)
    !
    call geopar(XB, XBP, YB, YBP, SB, NB, W1, SBLe, CHOrdb, AREab, &
            RADble, ANGbte, EI11ba, EI22ba, APX1ba, APX2ba, EI11bt, EI22bt, APX1bt, &
            & APX2bt, THIckb, CAMbrb)
    !
    call asks('Enter new airfoil name^', NAMe)
    call strip(NAMe, NNAme)
    if (show_output) then
        write (*, *)
        write (*, *) 'Result has been placed in buffer airfoil'
        write (*, *) 'Execute PCOP or PANE to set new current airfoil'
    endif
    return
    !
    100  if (show_output) then
        write (*, *)
        write (*, *) 'Invalid response.  No action taken.'
    end if
end subroutine intx

end module m_xfoil