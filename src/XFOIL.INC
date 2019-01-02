!
!====  XFOIL code global INCLUDE file  =====
!
!------ Primary dimensioning limit parameters
! IQX   number of surface panel nodes + 6
! IWX   number of wake panel nodes
! IPX   number of Qspec(s) distributions
! ISX   number of airfoil sides
!
!------ Derived dimensioning limit parameters
! IBX   number of buffer airfoil nodes
! IMX   number of complex mapping coefficients  Cn
! IZX   number of panel nodes (airfoil + wake)
! IVX   number of nodes along BL on one side of airfoil and wake
! NAX   number of points in stored polar
! NPX   number of polars and reference polars
! NFX   number of points in one reference polar
! NTX   number of points in thickness/camber arrays
!
!---- include polar variable indexing parameters
INCLUDE 'PINDEX.INC'
!
PARAMETER (IQX = 370, IPX = 5, ISX = 2)
PARAMETER (IWX = IQX / 8 + 2)
PARAMETER (IBX = 4 * IQX)
PARAMETER (IZX = IQX + IWX)
PARAMETER (IVX = IQX / 2 + IWX + 50)
PARAMETER (NAX = 800, NPX = 12, NFX = 128)
PARAMETER (NTX = 2 * IBX)
CHARACTER*32 LABREF
CHARACTER*64 FNAME, PFNAME, PFNAMX, ONAME, PREFIX, OCNAME
CHARACTER*48 NAME, NAMEPOL, CODEPOL, NAMEREF
CHARACTER*80 ISPARS
LOGICAL OK, LIMAGE, &
        LGAMU, LQINU, SHARP, LVISC, LALFA, LWAKE, LPACC, &
        LBLINI, LIPAN, LQAIJ, LADIJ, LWDIJ, LCPXX, LQVDES, LQREFL, &
        LQSPEC, LVCONV, LCPREF, LCLOCK, LPFILE, LPFILX, LPPSHO, &
        LBFLAP, LFLAP, LEIW, LSCINI, LFOREF, LNORM, LGSAME, LDCPLOT, &
        LPLCAM, LQSYM, LGSYM, LQGRID, LGGRID, LGTICK, &
        LQSLOP, LGSLOP, LCSLOP, LQSPPL, LGEOPL, LGPARM, &
        LCPGRD, LBLGRD, LBLSYM, LCMINP, LHMOMP, LFREQP, &
        LCPINV
LOGICAL LPLOT, LSYM, LIQSET, LCLIP, LVLAB, LCURS, LLAND
LOGICAL LPGRID, LPCDW, LPLIST, LPLEGN, LAECEN, LPCDH, LPCMDOT
LOGICAL TFORCE
REAL NX, NY, MASS, MINF1, MINF, MINF_CL, MVISC, MACHP1
INTEGER RETYP, MATYP, AIJPIV
CHARACTER*1 VMXBL
!
!---- dimension temporary work and storage arrays (EQUIVALENCED below)
REAL W1(6 * IQX), W2(6 * IQX), W3(6 * IQX), W4(6 * IQX), &
        W5(6 * IQX), W6(6 * IQX), W7(6 * IQX), W8(6 * IQX)
REAL BIJ(IQX, IZX), CIJ(IWX, IQX)
!
COMMON/CR01/ VERSION
COMMON/CC01/ FNAME, &
        NAME, ISPARS, ONAME, PREFIX, OCNAME, &
        PFNAME(NPX), PFNAMX(NPX), &
        NAMEPOL(NPX), CODEPOL(NPX), &
        NAMEREF(NPX)
COMMON/QMAT/ Q(IQX, IQX), DQ(IQX), &
        DZDG(IQX), DZDN(IQX), DZDM(IZX), &
        DQDG(IQX), DQDM(IZX), QTAN1, QTAN2, &
        Z_QINF, Z_ALFA, Z_QDOF0, Z_QDOF1, Z_QDOF2, Z_QDOF3
COMMON/CR03/ AIJ(IQX, IQX), DIJ(IZX, IZX)
COMMON/CR04/ QINV(IZX), QVIS(IZX), CPI(IZX), CPV(IZX), &
        QINVU(IZX, 2), QINV_A(IZX)
COMMON/CR05/ X(IZX), Y(IZX), XP(IZX), YP(IZX), S(IZX), &
        SLE, XLE, YLE, XTE, YTE, CHORD, YIMAGE, &
        WGAP(IWX), WAKLEN
COMMON/CR06/ GAM(IQX), GAMU(IQX, 2), GAM_A(IQX), SIG(IZX), &
        NX(IZX), NY(IZX), APANEL(IZX), &
        SST, SST_GO, SST_GP, &
        GAMTE, GAMTE_A, &
        SIGTE, SIGTE_A, &
        DSTE, ANTE, ASTE
COMMON/CR07/ SSPLE, &
        SSPEC(IBX), XSPOC(IBX), YSPOC(IBX), &
        QGAMM(IBX), &
        QSPEC(IBX, IPX), QSPECP(IBX, IPX), &
        ALGAM, CLGAM, CMGAM, &
        ALQSP(IPX), CLQSP(IPX), CMQSP(IPX), &
        QF0(IQX), QF1(IQX), QF2(IQX), QF3(IQX), &
        QDOF0, QDOF1, QDOF2, QDOF3, CLSPEC, FFILT
COMMON/CI01/ IQ1, IQ2, NSP, NQSP, KQTARG, IACQSP, NC1, NNAME, NPREFIX
COMMON/CR09/ ADEG, ALFA, AWAKE, MVISC, AVISC, &
        XCMREF, YCMREF, &
        CL, CM, CD, CDP, CDF, CL_ALF, CL_MSQ, &
        PSIO, CIRC, COSA, SINA, QINF, &
        GAMMA, GAMM1, &
        MINF1, MINF, MINF_CL, TKLAM, TKL_MSQ, CPSTAR, QSTAR, &
        CPMN, CPMNI, CPMNV, XCPMNI, XCPMNV
COMMON/CI03/ NCPREF, NAPOL(NPX), NPOL, IPACT, NLREF, &
        ILINP(NPX), ICOLP(NPX), &
        ISYMR(NPX), ICOLR(NPX), &
        IMATYP(NPX), IRETYP(NPX), NXYPOL(NPX), &
        NPOLREF, NDREF(4, NPX), &
        IPOL(IPTOT), NIPOL, NIPOL0, &
        JPOL(JPTOT), NJPOL
COMMON/CR10/ XPREF(IQX), CPREF(IQX), VERSPOL(NPX), &
        CPOL(NAX, IPTOT, NPX), &
        CPOLSD(NAX, ISX, JPTOT, NPX), &
        CPOLXY(IQX, 2, NPX), &
        MACHP1(NPX), &
        REYNP1(NPX), &
        ACRITP(ISX, NPX), &
        PTRATP(NPX), &
        ETAPP(NPX), &
        XSTRIPP(ISX, NPX), &
        CPOLREF(NFX, 2, 4, NPX)
COMMON/CC02/ LABREF
!
COMMON/CR11/ PI, HOPI, QOPI, DTOR
COMMON/CR12/ CVPAR, CTERAT, CTRRAT, XSREF1, XSREF2, XPREF1, XPREF2
COMMON/CI04/ N, NB, NW, NPAN, IST, KIMAGE, KDELIM, &
        ITMAX, NSEQEX, RETYP, MATYP, AIJPIV(IQX), &
        IDEV, IDEVRP, IPSLU, NCOLOR, &
        ICOLS(ISX), NOVER, NCM, NTK
COMMON/CR13/ SIZE, SCRNFR, PLOTAR, PFAC, UFAC, QFAC, VFAC, &
        XWIND, YWIND, &
        XPAGE, YPAGE, XMARG, YMARG, &
        CH, CHG, CHQ, &
        XOFAIR, YOFAIR, FACAIR, XOFA, YOFA, FACA, UPRWT, &
        CPMIN, CPMAX, CPDEL, &
        UEMIN, UEMAX, UEDEL, &
        CPOLPLF(3, 4), &
        XCDWID, XALWID, XOCWID
COMMON/CL01/ OK, LIMAGE, SHARP, &
        LGAMU, LQINU, LVISC, LALFA, LWAKE, LPACC, &
        LBLINI, LIPAN, LQAIJ, LADIJ, LWDIJ, LCPXX, LQVDES, LQREFL, &
        LQSPEC, LVCONV, LCPREF, LCLOCK, LPFILE, LPFILX, LPPSHO, &
        LBFLAP, LFLAP, LEIW, LSCINI, LFOREF, LNORM, LGSAME, LDCPLOT, &
        LPLCAM, LQSYM, LGSYM, &
        LQGRID, LGGRID, LGTICK, &
        LQSLOP, LGSLOP, LCSLOP, LQSPPL, LGEOPL, LGPARM, &
        LCPGRD, LBLGRD, LBLSYM, &
        LPLOT, LSYM, LIQSET, LCLIP, LVLAB, LCURS, LLAND, &
        LPGRID, LPCDW, LPLIST, LPLEGN, LAECEN, LPCDH, LPCMDOT, &
        LCMINP, LHMOMP, LFREQP, &
        LCPINV
COMMON/CR14/ XB(IBX), YB(IBX), &
        XBP(IBX), YBP(IBX), SB(IBX), SNEW(5 * IBX), &
        XBF, YBF, XOF, YOF, HMOM, HFX, HFY, &
        XBMIN, XBMAX, YBMIN, YBMAX, &
        SBLE, CHORDB, AREAB, RADBLE, ANGBTE, &
        EI11BA, EI22BA, APX1BA, APX2BA, &
        EI11BT, EI22BT, APX1BT, APX2BT, &
        THICKB, CAMBRB, &
        XCM(2 * IBX), YCM(2 * IBX), SCM(2 * IBX), XCMP(2 * IBX), YCMP(2 * IBX), &
        XTK(2 * IBX), YTK(2 * IBX), STK(2 * IBX), XTKP(2 * IBX), YTKP(2 * IBX)
!
COMMON/CR15/ XSSI(IVX, ISX), UEDG(IVX, ISX), UINV(IVX, ISX), &
        MASS(IVX, ISX), THET(IVX, ISX), DSTR(IVX, ISX), &
        CTAU(IVX, ISX), DELT(IVX, ISX), TSTR(IVX, ISX), &
        USLP(IVX, ISX), GUXQ(IVX, ISX), GUXD(IVX, ISX), &
        TAU(IVX, ISX), DIS(IVX, ISX), CTQ(IVX, ISX), &
        VTI(IVX, ISX), &
        REINF1, REINF, REINF_CL, &
        ACRIT(ISX), &
        XSTRIP(ISX), XOCTR(ISX), YOCTR(ISX), XSSITR(ISX), &
        UINV_A(IVX, ISX), &
        TINDEX(ISX)
COMMON/CI05/ IXBLP, &
        IBLTE(ISX), NBL(ISX), IPAN(IVX, ISX), ISYS(IVX, ISX), NSYS, &
        ITRAN(ISX), IDAMP
COMMON/CL02/ TFORCE(ISX)
COMMON/CR17/ RMSBL, RMXBL, RLX, VACCEL
COMMON/CI06/ IMXBL, ISMXBL
COMMON/CC03/ VMXBL
COMMON/CR18/ XSF, YSF, XOFF, YOFF, &
        XGMIN, XGMAX, YGMIN, YGMAX, DXYG, &
        XCMIN, XCMAX, YCMIN, YCMAX, DXYC, DYOFFC, &
        XPMIN, XPMAX, YPMIN, YPMAX, DXYP, DYOFFP, &
        YSFP, GTICK
COMMON/CR19/&
        XCADD(NTX), YCADD(NTX), YCADDP(NTX), &
        XPADD(NTX), YPADD(NTX), YPADDP(NTX), &
        XCAM(NTX), &
        YCAM(NTX), YCAMP(NTX), &
        PCAM(NTX), PCAMP(NTX)
COMMON/CI19/ NCAM
!
COMMON/VMAT/ VA(3, 2, IZX), VB(3, 2, IZX), VDEL(3, 2, IZX), &
        VM(3, IZX, IZX), VZ(3, 2)
!
!
!---- save storage space
EQUIVALENCE (Q(1, 1), W1(1)), (Q(1, 7), W2(1)), &
        (Q(1, 13), W3(1)), (Q(1, 19), W4(1)), &
        (Q(1, 25), W5(1)), (Q(1, 31), W6(1)), &
        (Q(1, 37), W7(1)), (Q(1, 43), W8(1))
EQUIVALENCE (VM(1, 1, 1), BIJ(1, 1)), (VM(1, 1, IZX / 2), CIJ(1, 1))
!
!
!   VERSION     version number of this XFOIL implementation
!
!   FNAME       airfoil data filename
!   PFNAME(.)   polar append filename
!   PFNAMX(.)   polar append x/c dump filename
!   ONAME       default overlay airfoil filename
!   PREFIX      default filename prefix
!   OCNAME      default Cp(x) overlay filename
!   NAME        airfoil name
!
!   ISPARS      ISES domain parameters  (not used in XFOIL)
!
!   Q(..)       generic coefficient matrix
!   DQ(.)       generic matrix righthand side
!
!   DZDG(.)     dPsi/dGam
!   DZDN(.)     dPsi/dn
!   DZDM(.)     dPsi/dSig
!
!   DQDG(.)     dQtan/dGam
!   DQDM(.)     dQtan/dSig
!   QTAN1       Qtan at alpha =  0 deg.
!   QTAN2       Qtan at alpha = 90 deg.
!
!   Z_QINF      dPsi/dQinf
!   Z_ALFA      dPsi/dalfa
!   Z_QDOF0     dPsi/dQdof0
!   Z_QDOF1     dPsi/dQdof1
!   Z_QDOF2     dPsi/dQdof2
!   Z_QDOF3     dPsi/dQdof3
!
!   AIJ(..)     dPsi/dGam  influence coefficient matrix (factored if LQAIJ=t)
!   BIJ(..)     dGam/dSig  influence coefficient matrix
!   CIJ(..)     dQtan/dGam influence coefficient matrix
!   DIJ(..)     dQtan/dSig influence coefficient matrix
!   QINV(.)     tangential velocity due to surface vorticity
!   QVIS(.)     tangential velocity due to surface vorticity & mass sources
!   QINVU(..)   QINV for alpha = 0, 90 deg.
!   QINV_A(.)   dQINV/dalpha
!
!   X(.),Y(.)   airfoil (1<i<N) and wake (N+1<i<N+NW) coordinate arrays
!   XP(.),YP(.) dX/dS, dY/dS arrays for spline evaluation
!   S(.)        arc length along airfoil (spline parameter)
!   SLE         value of S at leading edge
!   XLE,YLE     leading  edge coordinates
!   XTE,YTE     trailing edge coordinates
!   WGAP(.)     thickness of "dead air" region inside wake just behind TE
!   WAKLEN      wake length to chord ratio
!
!   GAM(.)      surface vortex panel strength array
!   GAMU(.2)    surface vortex panel strength arrays for alpha = 0, 90 deg.
!   GAM_A(.)    dGAM/dALFA
!   SIG(.)      surface and wake mass defect array
!
!   NX(.),NY(.) normal unit vector components at airfoil and wake coordinates
!   APANEL(.)   surface and wake panel angle array (+ counterclockwise)
!
!   SST         S value at stagnation point
!   SST_GO      dSST/dGAM(IST)
!   SST_GP      dSST/dGAM(IST+1)
!
!   GAMTE       vortex panel strength across finite-thickness TE
!   SIGTE       source panel strength across finite-thickness TE
!   GAMTE_A     dGAMTE/dALFA
!   SIGTE_A     dSIGTE/dALFA
!   DSTE        TE panel length
!   ANTE,ASTE   projected TE thickness perp.,para. to TE bisector
!   SHARP       .TRUE.  if  DSTE.EQ.0.0 ,  .FALSE. otherwise
!
!   SSPEC(.)    normalized arc length around airfoil (QSPEC coordinate)
!   XSPOC(.)    x/c at SSPEC points
!   YSPOC(.)    y/c at SSPEC points
!   QSPEC(..)   specified surface velocity for inverse calculations
!   QSPECP(..)  dQSPEC/dSSPEC
!   QGAMM(.)    surface velocity for current airfoil geometry
!   SSPLE       SSPEC value at airfoil nose
!
!   IQ1,IQ2     target segment endpoint indices on Qspec(s) plot
!   NSP         number of points in QSPEC array
!   NQSP        number Qspec arrays
!   IACQSP      1:  ALQSP is prescribed for Qspec arrays
!               2:  CLQSP is prescribed for Qspec arrays
!   NC1         number of circle plane points, must be 2**n - 1
!
!   NNAME       number of characters in airfoil name
!   NPREFIX     number of characters in default filename prefix
!
!   ALQSP(.)    alpha,CL,CM corresponding to QSPEC distributions
!   CLQSP(.)    
!   CMQSP(.)    
!   ALGAM       alpha,CL,CM corresponding to QGAMM distribution
!   CLGAM
!   CMGAM
!
!   QF0(.)      shape function for QSPEC modification
!   QF1(.)        "
!   QF2(.)        "
!   QF3(.)        "
!   QDOF0       shape function weighting coefficient (inverse DOF)
!   QDOF1         "
!   QDOF2         "
!   QDOF3         "
!   CLSPEC      specified CL
!   FFILT       circle-plane mapping filter parameter
!
!   ADEG,ALFA   angle of attack in degrees, radians
!   AWAKE       angle of attack corresponding to wake geometry (radians)
!   AVISC       angle of attack corresponding to BL solution   (radians)
!   MVISC       Mach number corresponding to BL solution
!   CL,CM       current CL and CM calculated from GAM(.) distribution
!   CD          current CD from BL solution
!   CDF         current friction CD from BL solution
!   CL_ALF      dCL/dALFA
!   CL_MSQ      dCL/d(MINF^2)
!
!   PSIO        streamfunction inside airfoil
!   CIRC        circulation
!   COSA,SINA   cos(ALFA), sin(ALFA)
!   QINF        freestream speed    (defined as 1)
!   GAMMA,GAMM1 Gas constant Cp/Cv, Cp/Cv - 1
!   MINF1       freestream Mach number at CL=1
!   MINF        freestream Mach number at current CL
!   MINF_CL     dMINF/dCL
!   TKLAM       Karman-Tsien parameter Minf^2 / [1 + sqrt(1-Minf^2)]^2
!   TKL_MSQ     d(TKLAM)/d(MINF^2)
!   CPSTAR      sonic pressure coefficient
!   QSTAR       sonic speed
!
!   NCPREF      number of reference Cp vs x/c points
!   XPREF(.)    x/c array corresponding to reference Cp data array
!   CPREF(.)    reference Cp data array
!   LABREF      reference Cp data descriptor string
!
!   NLREF       number of characters in LABREF string
!   NAPOL(.)    number of points in each stored polar
!   NPOL        number of stored polars
!   IPACT       index of "active" polar being accumulated (0 if none are)
!   ILINP(.)    line style for each polar
!   ICOLP(.)    color for each polar
!   ISYMR(.)    symbol type for each reference polar
!   ICOLR(.)    color for each reference polar
!
!   NDREF(..)   number of points in each stored reference polar
!   NPOLREF     number of stored reference polars
!
!   VERSPOL(.)  version number of generating-code for each polar
!   CPOL(...)   CL,CD,and other parameters for each polar
!   CPOLXY(.1.) x,y coordinates of airfoil geometry which generated each polar
!   CPOLXY(.2.)
!   NXYPOL(.)   number of x,y points in CPOLXY array
!
!   PXTR(..)    transition locations for each polar
!   NAMEPOL(.)  airfoil names for each polar
!   CODEPOL(.)  generating-code names for each polar
!
!   NAMEREF(.)  name label of reference polar
!
!   PI          3.1415926...
!   HOPI,QOPI   1/(2 Pi) ,  1/(4 Pi)
!   DTOR        Pi / 180    (degrees to radians conversion factor)
!
!   CVPAR       curvature attraction parameter for airfoil paneling
!               0 = uniform panel node spacing around airfoil
!              ~1 = panel nodes strongly bunched in areas of large curvature
!   CTERAT      TE panel density / LE panel density ratio
!   CTRRAT      local refinement panel density / LE panel density ratio
!   XSREF1-2    suction  side local refinement x/c limits
!   XPREF1-2    pressure side local refinement x/c limits
!
!   N           number of points on airfoil
!   NB          number of points in buffer airfoil array
!   NW          number of points in wake
!   NPAN        default/specified number of points on airfoil
!
!   KDELIM      type of delimiter for coordinate file output
!                0 = spaces
!                1 = commas
!                2 = tabs
!
!   IST         stagnation point lies between S(IST), S(IST+1)
!   ITMAX       max number of Newton iterations
!   NSEQEX      max number of unconverged sequence points for early exit
!
!   RETYP       index giving type of Re variation with CL ...
!            ... 1  Re constant 
!            ... 2  Re ~ 1/sqrt(CL)    (fixed lift)
!            ... 3  Re ~ 1/CL          (fixed lift and dynamic pressure)
!
!   MATYP       index giving type of Ma variation with CL ...
!            ... 1  Ma constant 
!            ... 2  Ma ~ 1/sqrt(CL)    (fixed lift)
!
!   AIJPIV(.)   pivot index array for LU factoring routine
!
!   IDEV        "device" number for normal screen plotting
!   IDEVRP      "device" number for replotting (typically for hardcopy)
!   IPSLU       PostScript file specifier
!   NCOLOR      Number of defined colors in colormap
!   ICOLS(1)    color indices of top side
!   ICOLS(2)    color indices of bottom side
!
!   NOVER       number of airfoils overlaid on GDES geometry plot
!
!   SCRNFR      screen fraction taken up by initial plot window
!   SIZE        plot width (inches)
!   PLOTAR      plot aspect ratio
!   XWIND,YWIND window size in inches
!   XPAGE,YPAGE plot-page size in inches (for hardcopy)
!   XMARG,YMARG margin dimensions in inches
!   PFAC        scaling factor for  Cp
!   UFAC        scaling factor for  Ue
!   QFAC        scaling factor for  q  (surface speed)
!   VFAC        scaling factor for  Cp vectors
!   CH          character width / plot size  ratio
!   CHG         character width / plot size  ratio for geometry plot
!   CHQ         character width / plot size  ratio for Qspec(s) plot
!
!   XOFAIR      x offset for airfoil in  Cp vs x plots
!   YOFAIR      y offset for airfoil in  Cp vs x plots
!   FACAIR      scale factor for airfoil in  Cp vs x plots
!   XOFA        x offset for airfoil in  Cp vs x plots in airfoil units
!   YOFA        y offset for airfoil in  Cp vs x plots in airfoil units
!   FACA        scale factor for airfoil in  Cp vs x plots  in airfoil units
!   UPRWT       u/Qinf scale factor for profile plotting
!   CPMAX       max Cp  in  Cp vs x plots
!   CPMIN       min Cp  in  Cp vs x plots
!   CPDEL       delta Cp  in  Cp vs x plots
!   UEMAX       max Ue  in  Ue vs x plots
!   UEMIN       min Ue  in  Ue vs x plots
!   UEDEL       delta Ue  in  Ue vs x plots
!
!   CPOLPLF(1,ICD)  min CD in CD-CL polar plot
!   CPOLPLF(2,ICD)  max CD in CD-CL polar plot
!   CPOLPLF(3,ICD)  delta CD in CD-CL polar plot
!
!   XCDWID      width of CD   -CL polar plot
!   XALWID      width of alpha-CL polar plot
!   XOCWID      width of Xtr/c-CL polar plot
!
!   OK          user question response
!   LIMAGE      .TRUE. if image airfoil is present
!   LGAMU       .TRUE. if GAMU  arrays exist for current airfoil geometry
!   LQINU       .TRUE. if QINVU arrays exist for current airfoil geometry
!   LVISC       .TRUE. if viscous option is invoked
!   LALFA       .TRUE. if alpha is specifed, .FALSE. if CL is specified
!   LWAKE       .TRUE. if wake geometry has been calculated
!   LPACC       .TRUE. if each point calculated is to be saved
!   LBLINI      .TRUE. if BL has been initialized
!   LIPAN       .TRUE. if BL->panel pointers IPAN have been calculated
!   LQAIJ       .TRUE. if dPsi/dGam matrix has been computed and factored
!   LADIJ       .TRUE. if dQ/dSig matrix for the airfoil has been computed
!   LWDIJ       .TRUE. if dQ/dSig matrix for the wake has been computed
!   LQVDES      .TRUE. if viscous Ue is to be plotted in QDES routines
!   LQSPEC      .TRUE. if Qspec has been initialized
!   LQREFL      .TRUE. if reflected Qspec is to be plotted in QDES routines
!   LVCONV      .TRUE. if converged BL solution exists
!   LCPREF      .TRUE. if reference data is to be plotted on Cp vs x/c plots
!   LCLOCK      .TRUE. if source airfoil coordinates are clockwise
!   LPFILE      .TRUE. if polar file is ready to be appended to
!   LPFILX      .TRUE. if polar dump file is ready to be appended to
!   LPPSHO      .TRUE. if CL-CD polar is plotted during point sequence
!   LBFLAP      .TRUE. if buffer  airfoil flap parameters are defined
!   LFLAP       .TRUE. if current airfoil flap parameters are defined
!   LEIW        .TRUE. if unit circle complex number array is initialized
!   LSCINI      .TRUE. if old-airfoil circle-plane arc length s(w) exists
!   LFOREF      .TRUE. if CL,CD... data is to be plotted on Cp vs x/c plots
!   LNORM       .TRUE. if input buffer airfoil is to be normalized
!   LGSAME      .TRUE. if current and buffer airfoils are identical
!   LDCPLOT     .TRUE. if delta(Cp) plot is to be plotted in CAMB menu
!
!   LPLCAM      .TRUE. if thickness and camber are to be plotted
!   LQSYM       .TRUE. if symmetric Qspec will be enforced
!   LGSYM       .TRUE. if symmetric geometry will be enforced
!   LQGRID      .TRUE. if grid is to overlaid on Qspec(s) plot
!   LGGRID      .TRUE. if grid is to overlaid on buffer airfoil geometry plot
!   LGTICK      .TRUE. if node tick marks are to be plotted on buffer airfoil
!   LQSLOP      .TRUE. if modified Qspec(s) segment is to match slopes
!   LGSLOP      .TRUE. if modified geometry segment is to match slopes
!   LCSLOP      .TRUE. if modified camber line segment is to match slopes
!   LQSPPL      .TRUE. if current Qspec(s) in in plot
!   LGEOPL      .TRUE. if current geometry in in plot
!   LCPGRD      .TRUE. if grid is to be plotted on Cp plots
!   LBLGRD      .TRUE. if grid is to be plotted on BL variable plots
!   LBLSYM      .TRUE. if symbols are to be plotted on BL variable plots
!   LCMINP      .TRUE. if min Cp is to be written to polar file for cavitation
!   LHMOMP      .TRUE. if hinge moment is to be written to polar file
!   LFREQP      .TRUE. if individual TS-wave frequencies are to be plotted
!
!   LPGRID      .TRUE. if polar grid overlay is enabled
!   LPCDW       .TRUE. if polar CDwave is plotted
!   LPLIST      .TRUE. if polar listing lines (at top of plot) are enabled
!   LPLEGN      .TRUE. if polar legend is enabled
!   
!   LPLOT       .TRUE. if plot page is open
!   LSYM        .TRUE. if symbols are to be plotted in QDES routines
!   LIQSET      .TRUE. if inverse target segment is marked off in QDES
!   LCLIP       .TRUE. if line-plot clipping is to be performed
!   LVLAB       .TRUE. if label is to be plotted on viscous-variable plots
!   LCURS       .TRUE. if cursor input is to be used for blowups, etc.
!   LLAND       .TRUE. if Landscape orientation for PostScript is used
!
!
!   XB(.),YB(.) buffer airfoil coordinate arrays
!   XBP(.)      dXB/dSB
!   YBP(.)      dYB/dSB
!   SB(.)       spline parameter for buffer airfoil
!   SNEW(.)     new panel endpoint arc length array
!
!   XBF,YBF     buffer  airfoil flap hinge coordinates
!   XOF,YOF     current airfoil flap hinge coordinates
!   HMOM        moment of flap about hinge point
!   HFX         x-force of flap on hinge point
!   HFY         y-force of flap on hinge point
!
!~~~~~~~~~~~~~~ properties of current buffer airfoil
!
!   XBMIN,XBMAX  limits of XB array
!   YBMIN,YBMAX  limits of YB array
!   SBLE         LE tangency-point SB location
!   CHORDB       chord
!   AREAB        area
!   RADBLE       LE radius
!   ANGBTE       TE angle  (rad)
!   
!   EI11BA       bending inertia about axis 1    x^2 dx dy
!   EI22BA       bending inertia about axis 2    y^2 dx dy
!   APX1BA       principal axis 1 angle 
!   APX2BA       principal axis 2 angle 
!
!   EI11BT       bending inertia about axis 1    x^2 t ds
!   EI22BT       bending inertia about axis 2    y^2 t ds
!   APX1BT       principal axis 1 angle 
!   APX2BT       principal axis 2 angle 
!
!   THICKB       max thickness
!   CAMBRB       max camber
!
!~~~~~~~~~~~~~~
!
!   XSSI(..)    BL arc length coordinate array on each surface
!   UEDG(..)    BL edge velocity array
!   UINV(..)    BL edge velocity array without mass defect influence
!   MASS(..)    BL mass defect array  ( = UEDG*DSTR )
!   THET(..)    BL momentum thickness array
!   DSTR(..)    BL displacement thickness array
!   TSTR(..)    BL kin. energy  thickness array
!   CTAU(..)    sqrt(max shear coefficient) array
!               (in laminar regions, log of amplification ratio)
!
!   TAU(..)     wall shear stress array                 (for plotting only)
!   DIS(..)     dissipation array                       (for plotting only)
!   CTQ(..)     sqrt(equilibrium max shear coefficient) array (  "  )
!   VTI(..)     +/-1 conversion factor between panel and BL variables
!   UINV_A(..)  dUINV/dalfa array
! 
!   REINF1      Reynolds number  Vinf c / ve  for CL=1
!   REINF       Reynolds number for current CL
!   REINF_CL    dREINF/dCL
!
!   ACRIT       log (critical amplification ratio)
!   XSTRIP(.)   transition trip  x/c locations (if XTRIP > 0),
!               transition trip -s/s_side locations (if XTRIP < 0),
!   XOCTR(.)    actual transition x/c locations
!   YOCTR(.)    actual transition y/c locations
!   XSSITR(.)   actual transition xi locations
!
!   IXBLP   = 1  plot BL variables vs x
!           = 2  plot BL variables vs s
!   IBLTE(.)    BL array index at trailing edge
!   NBL(.)      max BL array index
!   IPAN(..)    panel index corresponding to BL location
!   ISYS(..)    BL Newton system line number corresponding to BL location
!   NSYS        total number of lines in BL Newton system
!   ITRAN(.)    BL array index of transition interval
!   TFORCE(.)   .TRUE. if transition is forced due to transition strip
!   TINDEX(.)   
!
!   IDAMP    = 0   use original enelope e^n f(H,Rtheta) for all profiles
!            = 1   use modified enelope e^n f(H,Rtheta) for separating profile
!
!   VA,VB(...)  diagonal and off-diagonal blocks in BL Newton system
!   VZ(..)      way-off-diagonal block at TE station line
!   VM(...)     mass-influence coefficient vectors in BL Newton system
!   VDEL(..)    residual and solution vectors in BL Newton system
!
!   RMSBL       rms change from BL Newton system solution
!   RMXBL       max change from BL Newton system solution
!   IMXBL       location of max change
!   ISMXBL      index of BL side containing max change
!   VMXBL       character identifying variable with max change
!   RLX         underrelaxation factor for Newton update
!   VACCEL      parameter for accelerating BL Newton system solution
!               (any off-diagonal element < VACCEL is not eliminated,
!                which speeds up each iteration, but MAY increase
!                iteration count)
!                Can be set to zero for unadulterated Newton method
!
!   XOFF,YOFF   x and y offsets for windowing in QDES,GDES routines
!   XSF ,YSF    x and y scaling factors for windowing in QDES,GDES routines
!
!   XGMIN       airfoil grid plot limits
!   XGMAX
!   YGMIN
!   YGMAX
!   DXYG        airfoil grid-plot annotation increment
!   GTICK       airfoil-plot tick marks size (as fraction of arc length)
