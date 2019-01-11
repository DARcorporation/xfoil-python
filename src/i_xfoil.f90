module i_xfoil
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
    use i_pindex
    !
    integer , parameter  ::  IQX = 370 , IPX = 5 , ISX = 2 , IWX = IQX/8 + 2 , IBX = 4*IQX , IZX = IQX + IWX ,        &
            &                         IVX = IQX/2 + IWX + 50 , NAX = 800 , NPX = 12 , NFX = 128 , NTX = 2*IBX
    !
    !---- dimension temporary work and storage arrays
    real , dimension(IQX,IZX)  ::  BIJ
    real , dimension(IWX,IQX)  ::  CIJ
    real , dimension(6*IQX)  ::  W1 , W2 , W3 , W4 , W5 , W6 , W7 , W8

    character(48) , dimension(NPX)  ::  CODepol , NAMepol , NAMeref
    character(64)  ::  FNAme , OCName , ONAme , PREfix
    character(80)  ::  ISPars
    character(48)  ::  NAMe
    character(64) , dimension(NPX)  ::  PFName , PFNamx
    character(32)  ::  LABref
    character(1)  ::  VMXbl

    integer  ::  IACqsp , IQ1 , IQ2 , KQTarg , NC1 , NNAme , NPRefix , NQSp , NSP

    integer , dimension(NPX)  ::  ICOlp , ICOlr , ILInp , IMAtyp , IREtyp , ISYmr , NAPol , NXYpol
    integer  ::  IPAct , NCPref , NIPol , NIPol0 , NJPol , NLRef , NPOl , NPOlref
    integer , dimension(IPTOT)  ::  IPOl
    integer , dimension(JPTOT)  ::  JPOl
    integer , dimension(4,NPX)  ::  NDRef

    integer , dimension(IQX)  ::  AIJpiv
    integer , dimension(ISX)  ::  ICOls
    integer  ::  IDEv , IDEvrp , IPSlu , IST , ITMax , KDElim , KIMage , MATyp , N , NB , NCM , NCOlor , NOVer ,      &
            &             NPAn , NSEqex , NTK , NW , RETyp

    integer , dimension(ISX)  ::  IBLte , ITRan , NBL
    integer  ::  IDAmp , IXBlp , NSYs
    integer , dimension(IVX,ISX)  ::  IPAn , ISYs

    integer  ::  IMXbl , ISMxbl
    integer  ::  NCAm

    logical  ::  LADij , LAEcen , LALfa , LBFlap , LBLgrd , LBLini , LBLsym , LCLip , LCLock , LCMinp , LCPgrd ,      &
            &             LCPinv , LCPref , LCPxx , LCSlop , LCUrs , LDCplot , LEIw , LFLap , LFOref , LFReqp , LGAmu ,        &
            &             LGEopl , LGGrid , LGParm , LGSame , LGSlop , LGSym , LGTick , LHMomp , LIMage , LIPan , LIQset ,     &
            &             LLAnd , LNOrm , LPAcc , LPCdh , LPCdw , LPCmdot , LPFile , LPFilx , LPGrid , LPLcam , LPLegn ,       &
            &             LPList , LPLot , LPPsho , LQAij , LQGrid , LQInu , LQRefl , LQSlop , LQSpec , LQSppl , LQSym ,       &
            &             LQVdes , LSCini , LSYm , LVConv , LVIsc , LVLab , LWAke , LWDij , OK , SHArp
    logical , dimension(ISX)  ::  TFOrce

    real  ::  VERsion
    real , dimension(IQX,IQX)  ::  AIJ
    real , dimension(IZX,IZX)  ::  DIJ
    real , dimension(IZX)  ::  CPI , CPV , QINv , QINv_a , QVIs
    real , dimension(IZX,2)  ::  QINvu
    real  ::  CHOrd , SLE , WAKlen , XLE , XTE , YIMage , YLE , YTE
    real , dimension(IZX)  ::  S , X , XP , Y , YP
    real , dimension(IWX)  ::  WGAp
    real  ::  ANTe , ASTe , DSTe , GAMte , GAMte_a , SIGte , SIGte_a , SST , SST_go , SST_gp
    real , dimension(IZX)  ::  APAnel , NX , NY , SIG
    real , dimension(IQX)  ::  GAM , GAM_a
    real , dimension(IQX,2)  ::  GAMu
    real  ::  ALGam , CLGam , CLSpec , CMGam , FFIlt , QDOf0 , QDOf1 , QDOf2 , QDOf3 , SSPle
    real , dimension(IPX)  ::  ALQsp , CLQsp , CMQsp
    real , dimension(IQX)  ::  QF0 , QF1 , QF2 , QF3
    real , dimension(IBX)  ::  QGAmm , SSPec , XSPoc , YSPoc
    real , dimension(IBX,IPX)  ::  QSPec , QSPecp
    real  ::  ADEg , ALFa , AVIsc , AWAke , CD , CDF , CDP , CIRc , CL , CL_alf , CL_msq , CM , COSa , CPMn , CPMni , &
            &          CPMnv , CPStar , GAMm1 , GAMma , MINf , MINf1 , MINf_cl , MVIsc , PSIo , QINf , QSTar , SINa , TKLam ,  &
            &          TKL_msq , XCMref , XCPmni , XCPmnv , YCMref
    real , dimension(ISX,NPX)  ::  ACRitp , XSTripp
    real , dimension(NAX,IPTOT,NPX)  ::  CPOl
    real , dimension(NFX,2,4,NPX)  ::  CPOlref
    real , dimension(NAX,ISX,JPTOT,NPX)  ::  CPOlsd
    real , dimension(IQX,2,NPX)  ::  CPOlxy
    real , dimension(IQX)  ::  CPRef , XPRef
    real , dimension(NPX)  ::  ETApp , MAChp1 , PTRatp , REYnp1 , VERspol
    real  ::  DTOr , HOPi , PI , QOPi
    real  ::  CTErat , CTRrat , CVPar , XPRef1 , XPRef2 , XSRef1 , XSRef2
    real  ::  CH , CHG , CHQ , CPDel , CPMax , CPMin , FACa , FACair , PFAc , PLOtar , QFAc , SCRnfr , SIZe , UEDel , &
            &          UEMax , UEMin , UFAc , UPRwt , VFAc , XALwid , XCDwid , XMArg , XOCwid , XOFa , XOFair , XPAge , XWInd ,&
            &          YMArg , YOFa , YOFair , YPAge , YWInd
    real , dimension(3,4)  ::  CPOlplf
    real  ::  ANGbte , APX1ba , APX1bt , APX2ba , APX2bt , AREab , CAMbrb , CHOrdb , EI11ba , EI11bt , EI22ba ,       &
            &          EI22bt , HFX , HFY , HMOm , RADble , SBLe , THIckb , XBF , XBMax , XBMin , XOF , YBF , YBMax , YBMin ,  &
            &          YOF
    real , dimension(IBX)  ::  SB , XB , XBP , YB , YBP
    real , dimension(2*IBX)  ::  SCM , STK , XCM , XCMp , XTK , XTKp , YCM , YCMp , YTK , YTKp
    real , dimension(5*IBX)  ::  SNEw
    real , dimension(ISX)  ::  ACRit , TINdex , XOCtr , XSSitr , XSTrip , YOCtr
    real , dimension(IVX,ISX)  ::  CTAu , CTQ , DELt , DIS , DSTr , GUXd , GUXq , MASs , TAU , THEt , TSTr , UEDg ,   &
            &                               UINv , UINv_a , USLp , VTI , XSSi
    real  ::  REInf , REInf1 , REInf_cl
    real  ::  RLX , RMSbl , RMXbl , VACcel
    real  ::  DXYc , DXYg , DXYp , DYOffc , DYOffp , GTIck , XCMax , XCMin , XGMax , XGMin , XOFf , XPMax , XPMin ,   &
            &          XSF , YCMax , YCMin , YGMax , YGMin , YOFf , YPMax , YPMin , YSF , YSFp
    real , dimension(NTX)  ::  PCAm , PCAmp , XCAdd , XCAm , XPAdd , YCAdd , YCAddp , YCAm , YCAmp , YPAdd , YPAddp

    real , dimension(IQX)  ::  DQ , DQDg , DZDg , DZDn
    real , dimension(IZX)  ::  DQDm , DZDm
    real , dimension(IQX,IQX)  ::  Q
    real  ::  QTAn1 , QTAn2 , Z_Alfa , Z_Qdof0 , Z_Qdof1 , Z_Qdof2 , Z_Qdof3 , Z_Qinf

    real , dimension(3,2,IZX)  ::  VA , VB , VDEl
    real , dimension(3,IZX,IZX)  ::  VM
    real , dimension(3,2)  ::  VZ
    !
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

end module i_xfoil