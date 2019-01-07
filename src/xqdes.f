C***********************************************************************
C    Module:  xqdes.f
C 
C    Copyright (C) 2000 Mark Drela 
C 
C    This program is free software; you can redistribute it and/or modify
C    it under the terms of the GNU General Public License as published by
C    the Free Software Foundation; either version 2 of the License, or
C    (at your option) any later version.
C
C    This program is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU General Public License for more details.
C
C    You should have received a copy of the GNU General Public License
C    along with this program; if not, write to the Free Software
C    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
C***********************************************************************
C
      SUBROUTINE QDES
C------------------------------------------------------
C     Mixed-Inverse design routine. Based on the 
C     same panel formulation as basic analysis method.
C------------------------------------------------------
      INCLUDE 'XFOIL.INC'
      CHARACTER*4 COMAND, COMOLD
      LOGICAL LRECALC
C
      CHARACTER*128 COMARG, ARGOLD
      CHARACTER*1 CHKEY
C
      REAL XBOX(2), YBOX(2)
      REAL XSP(IBX), YSP(IBX,IPX), YSPD(IBX,IPX)
C
      DIMENSION IINPUT(20)
      DIMENSION RINPUT(20)
      LOGICAL ERROR, LPLNEW
C
      SAVE COMOLD, ARGOLD
C
C---- statement function for compressible Karman-Tsien velocity
      QCOMP(G) = G*(1.0-TKLAM) / (1.0 - TKLAM*(G/QINF)**2)
C
C
      COMAND = '****'
      COMARG = ' '
      LRECALC = .FALSE.
C
      IF(N.EQ.0) THEN
       WRITE(*,*)
       WRITE(*,*) '***  No airfoil available  ***'
       RETURN
      ENDIF
C
      LSYM = .TRUE.
C
C---- number of sub-intervals for Qspec(s) plotting
      NTQSPL = 1
      IF(LQSLOP) NTQSPL = 8
C
C---- make sure a current solution exists
      CALL SPECAL
C
C---- see if current Qspec, if any, didn't come from Full-Inverse
      IF(NSP.NE.N) THEN
        LQSPEC = .FALSE.
        LIQSET = .FALSE.
      ENDIF
C
C---- set alpha, etc corresponding to Q
      ALGAM = ALFA
      CLGAM = CL
      CMGAM = CM
C
C---- set "old" speed distribution Q, arc length, and x/c,y/c arrays
      CHX = XTE - XLE
      CHY = YTE - YLE
      CHSQ = CHX**2 + CHY**2
      NSP = N
      DO I=1, NSP
        QGAMM(I) = GAM(I)
        SSPEC(I) = S(I)/S(N)
        XSPOC(I) = ((X(I)-XLE)*CHX + (Y(I)-YLE)*CHY)/CHSQ
        YSPOC(I) = ((Y(I)-YLE)*CHX - (X(I)-XLE)*CHY)/CHSQ
      ENDDO
      SSPLE = SLE/S(N)
C
      WRITE(*,1150) ALGAM/DTOR, CLGAM
 1150 FORMAT(/' Current Q operating condition:'
     &       /' alpha = ', F8.3, ' deg.      CL = ', F8.4 / )
C
      IF(.NOT.LQSPEC) THEN
C----- initialize Qspec to "old" solution and notify user
       NQSP = 1
       KQTARG = 1
       CALL GAMQSP(1)
       WRITE(*,1155)
       LQSPEC = .TRUE.
      ENDIF
C
C
C====================================================
C---- start of menu loop
 500  CONTINUE
      COMOLD = COMAND
      ARGOLD = COMARG
C
 501  CALL ASKC('.QDES^',COMAND,COMARG)
C
C--------------------------------------------------------
C---- process previous command ?
      IF(COMAND(1:1).EQ.'!') THEN
        IF(COMOLD.EQ.'****') THEN
          WRITE(*,*) 'Previous .QDES command not valid'
          GO TO 501
        ELSE
          COMAND = COMOLD
          COMARG = ARGOLD
          LRECALC = .TRUE.
        ENDIF
      ELSE
        LRECALC = .FALSE.
      ENDIF
C
      IF(COMAND.EQ.'    ') THEN
C----- just <return> was typed... clean up plotting and exit OPER
       RETURN
      ENDIF
C
C---- extract command line numeric arguments
      DO I=1, 20
        IINPUT(I) = 0
        RINPUT(I) = 0.0
      ENDDO
      NINPUT = 0
      CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
      NINPUT = 0
      CALL GETFLT(COMARG,RINPUT,NINPUT,ERROR)
C
C--------------------------------------------------------
      IF(COMAND.EQ.'?   ') THEN
       WRITE(*,1050)
 1050  FORMAT(
     &  /'   <cr>   Return to Top Level'
     & //'   QSET   Reset Qspec <== Q'
     &  /'   SMOO   Smooth Qspec inside target segment'
     &  /'   SLOP   Toggle modified-Qspec slope matching flag'
     &  /'   REST   Restore geometry from buffer airfoil'
     &  /'   CPXX   CPxx endpoint constraint toggle')
C
C--------------------------------------------------------
C---- re-initialize Qspec to Q
      ELSEIF(COMAND.EQ.'QSET') THEN
       CALL GAMQSP(1)
      GO TO 500
C
C--------------------------------------------------------
C---- smooth Qspec within target segment, or entire Qspec if not marked off
      ELSEIF(COMAND.EQ.'SMOO') THEN
C
       KQSP = 1
       CALL SMOOQ(IQ1,IQ2,KQSP)
       CALL SPLQSP(KQSP)
C
       CALL CLCALC(N,X,Y,QSPEC(1,KQSP),W1,ALFA,MINF,QINF, XCMREF,YCMREF, 
     &             CLQSP(KQSP),CMQSP(KQSP),CDPQ, CLQ_ALF,CLQ_MSQ)
        WRITE(*,1200) CL,CM,CLQSP(KQSP),CMQSP(KQSP)
      GO TO 500
C
C--------------------------------------------------------
C---- toggle Qspec endpoint slope matching
      ELSEIF(COMAND.EQ.'SLOP') THEN
       LQSLOP = .NOT.LQSLOP
       IF(LQSLOP) THEN
         WRITE(*,*)
     &    'Modified Qspec piece will be made tangent at endpoints'
       ELSE
         WRITE(*,*)
     &   'Modified Qspec piece will not be made tangent at endpoints'
       ENDIF
      GO TO 500
C
C--------------------------------------------------------
C---- toggle CPxx preservation constraints
      ELSEIF(COMAND.EQ.'CPXX') THEN
       LCPXX = .NOT.LCPXX
       IF(LCPXX) THEN
        WRITE(*,*) 'CPxx will be constrained'
       ELSE
        WRITE(*,*) 'CPxx will not be constrained'
       ENDIF
      GO TO 500
C
C--------------------------------------------------------
C---- restore and spline old airfoil
      ELSEIF(COMAND.EQ.'REST') THEN
       DO I=1, N
         X(I) = XB(I)
         Y(I) = YB(I)
       ENDDO
       CALL SCALC(X,Y,S,N)
       CALL SPLIND(X,XP,S,N,-999.0,-999.0)
       CALL SPLIND(Y,YP,S,N,-999.0,-999.0)
       CALL NCALC(X,Y,S,N,NX,NY)
       CALL LEFIND(SLE,X,XP,Y,YP,S,N)
       XLE = SEVAL(SLE,X,XP,S,N)
       YLE = SEVAL(SLE,Y,YP,S,N)
       CHORD  = SQRT( (0.5*(X(1)+X(N)) - XLE)**2
     &              + (0.5*(Y(1)+Y(N)) - YLE)**2 )
       CALL TECALC
       CALL APCALC
       LGAMU = .FALSE.
       LQINU = .FALSE.
       LGSAME = .TRUE.
C
cc       CALL NAMMOD(NAME,-1,1)
cc       CALL STRIP(NAME,NNAME)
C
C--------------------------------------------------------
      ELSE
       WRITE(*,1100) COMAND
 1100  FORMAT(' Command ',A4,' not recognized.  Type a " ? " for list.')
C
       COMAND = '****'
      ENDIF
C
      GO TO 500
C
C....................................................
C
 1155 FORMAT(/' Qspec initialized to current Q.'/ )
 1200 FORMAT(/' Q    :   CL =',F11.6, '    CM =',F11.6
     &       /' Qspec:   CL =',F11.6, '    CM =',F11.6 )
      END


      SUBROUTINE SPLQSP(KQSP)
C------------------------------------------------------
C     Splines Qspec(s).  The end intervals are treated
C     specially to avoid Gibbs-type problems from 
C     blindly splining to the stagnation point.
C------------------------------------------------------
      INCLUDE 'XFOIL.INC'
C
C---- usual spline with natural end BCs
      CALL SPLIND(QSPEC(2,KQSP),QSPECP(2,KQSP),SSPEC(2),NSP-2,
     &            -999.0,-999.0)
C
ccC---- pseudo-monotonic spline with simple secant slope calculation
cc      CALL SPLINA(QSPEC(2,KQSP),QSPECP(2,KQSP),SSPEC(2),NSP-2)
C
C---- end intervals are splined separately with natural BCs at
C     the trailing edge and matching slopes at the interior points
C
      I = 1
      CALL SPLIND(QSPEC(I,KQSP),QSPECP(I,KQSP),SSPEC(I),2,
     &            -999.0,QSPECP(I+1,KQSP))
C
      I = NSP-1
      CALL SPLIND(QSPEC(I,KQSP),QSPECP(I,KQSP),SSPEC(I),2,
     &            QSPECP(I,KQSP),-999.0)
C
      RETURN
      END


      SUBROUTINE SMOOQ(KQ1,KQ2,KQSP)
C--------------------------------------------
C     Smooths Qspec(s) inside target segment
C--------------------------------------------
      INCLUDE 'XFOIL.INC'
C
cC---- calculate smoothing coordinate
ccc      IF(NSP.EQ.NC1) THEN
cC
cC------ mapping inverse: use circle plane coordinate
c        I = 1
c        W8(I) = 0.0
c        DO 10 I=2, NSP
c          SINW = 2.0*SIN( 0.25*(WC(I)+WC(I-1)) )
c          SINWE = SINW**(1.0-AGTE)
cC
c          DSDW = SINWE * EXP( REAL(0.5*(PIQ(I)+PIQ(I-1)) ))
c          W8(I) = W8(I-1) + (WC(I)-WC(I-1))/DSDW
c   10   CONTINUE
c        DO 11 I=1, NSP
c          W8(I) = W8(I)/W8(NSP)
c 11     CONTINUE
cC
cC------ do not smooth first and last intervals in circle plane
c        KQ1 = MAX(IQ1,2)
c        KQ2 = MIN(IQ2,NSP-1)
cC
ccc      ELSE
C
C------ mixed inverse: use arc length coordinate
        DO 15 I=1, NSP
          W8(I) = SSPEC(I)
   15   CONTINUE
C
ccc      ENDIF
C
C
      IF(KQ2-KQ1 .LT. 2) THEN
       WRITE(*,*) 'Segment is too short.  No smoothing possible.'
       RETURN
      ENDIF
C
C---- set smoothing length ( ~ distance over which data is smeared )
      SMOOL = 0.002*(W8(NSP) - W8(1))
CCC   CALL ASKR('Enter Qspec smoothing length^',SMOOL)
C
C---- set up tri-diagonal system for smoothed Qspec
      SMOOSQ = SMOOL**2
      DO 20 I=KQ1+1, KQ2-1
        DSM = W8(I  ) - W8(I-1)
        DSP = W8(I+1) - W8(I  )
        DSO = 0.5*(W8(I+1) - W8(I-1))
C
        W1(I) =  SMOOSQ * (         - 1.0/DSM) / DSO
        W2(I) =  SMOOSQ * ( 1.0/DSP + 1.0/DSM) / DSO  +  1.0
        W3(I) =  SMOOSQ * (-1.0/DSP          ) / DSO
   20 CONTINUE
C
C---- set fixed-Qspec end conditions
      W2(KQ1) = 1.0
      W3(KQ1) = 0.0
C
      W1(KQ2) = 0.0
      W2(KQ2) = 1.0
C
      IF(LQSLOP) THEN
C----- also enforce slope matching at endpoints
       I = KQ1 + 1
       DSM = W8(I  ) - W8(I-1)
       DSP = W8(I+1) - W8(I  )
       DS  = W8(I+1) - W8(I-1)
       W1(I) = -1.0/DSM - (DSM/DS)/DSM
       W2(I) =  1.0/DSM + (DSM/DS)/DSM + (DSM/DS)/DSP
       W3(I) =                         - (DSM/DS)/DSP
       QSPP1 = W1(I)*QSPEC(I-1,KQSP)
     &       + W2(I)*QSPEC(I  ,KQSP)
     &       + W3(I)*QSPEC(I+1,KQSP)
C
       I = KQ2 - 1
       DSM = W8(I  ) - W8(I-1)
       DSP = W8(I+1) - W8(I  )
       DS  = W8(I+1) - W8(I-1)
       W1(I) =                           (DSP/DS)/DSM
       W2(I) = -1.0/DSP - (DSP/DS)/DSP - (DSP/DS)/DSM
       W3(I) =  1.0/DSP + (DSP/DS)/DSP
       QSPP2 = W1(I)*QSPEC(I-1,KQSP)
     &       + W2(I)*QSPEC(I  ,KQSP)
     &       + W3(I)*QSPEC(I+1,KQSP)
C
       QSPEC(KQ1+1,KQSP) = QSPP1
       QSPEC(KQ2-1,KQSP) = QSPP2
      ENDIF
C
C
C---- solve for smoothed Qspec array
      CALL TRISOL(W2(KQ1),W1(KQ1),W3(KQ1),QSPEC(KQ1,KQSP),(KQ2-KQ1+1))
C
C
cc      IF(LQSYM) THEN
cc        DO 40 I=KQ1+1, KQ2-1
cc          QSPEC(NSP-I+1,KQSP) = -QSPEC(I,KQSP)
cc 40     CONTINUE
cc      ENDIF
C
      RETURN
      END
 

      FUNCTION QINCOM(QC,QINF,TKLAM)
C-------------------------------------
C     Sets incompressible speed from
C     Karman-Tsien compressible speed
C-------------------------------------
C
      IF(TKLAM.LT.1.0E-4 .OR. ABS(QC).LT.1.0E-4) THEN
C----- for nearly incompressible case or very small speed, use asymptotic
C      expansion of singular quadratic formula to avoid numerical problems
       QINCOM = QC/(1.0 - TKLAM)
      ELSE
C----- use quadratic formula for typical case
       TMP = 0.5*(1.0 - TKLAM)*QINF/(QC*TKLAM)
       QINCOM = QINF*TMP*(SQRT(1.0 + 1.0/(TKLAM*TMP**2)) - 1.0)
      ENDIF
      RETURN
      END 

 
 
 
 
      SUBROUTINE GAMQSP(KQSP)
C------------------------------------------------
C     Sets Qspec(s,k) from current speed Q(s).
C------------------------------------------------
      INCLUDE 'XFOIL.INC'
C
      ALQSP(KQSP) = ALGAM
      CLQSP(KQSP) = CLGAM
      CMQSP(KQSP) = CMGAM
C
      DO 10 I=1, NSP
        QSPEC(I,KQSP) = QGAMM(I)
 10   CONTINUE
C
C---- zero out Qspec DOFs
      QDOF0 = 0.0
      QDOF1 = 0.0
      QDOF2 = 0.0
      QDOF3 = 0.0
C
      CALL SPLQSP(KQSP)
C
C---- reset target segment endpoints
      IF(.NOT.LIQSET) THEN
       IQ1 = 1
       IQ2 = NSP
      ENDIF
C
      RETURN
      END
