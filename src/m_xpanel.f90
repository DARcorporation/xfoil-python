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

!*==APCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module m_xpanel
contains
    subroutine apcalc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: i, ip
        real :: sx, sy
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
        !---- set angles of airfoil panels
        do i = 1, N - 1
            sx = X(i + 1) - X(i)
            sy = Y(i + 1) - Y(i)
            if (sx==0.0 .and. sy==0.0) then
                APAnel(i) = atan2(-NY(i), -NX(i))
            else
                APAnel(i) = atan2(sx, -sy)
            endif
        enddo
        !
        !---- TE panel
        i = N
        ip = 1
        if (SHArp) then
            APAnel(i) = PI
        else
            sx = X(ip) - X(i)
            sy = Y(ip) - Y(i)
            APAnel(i) = atan2(-sx, sy) + PI
        endif
        !
    end subroutine apcalc
    !*==NCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine ncalc(X, Y, S, N, Xn, Yn)
        use m_spline, only: segspl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: N
        real, dimension(N) :: S, X, Xn, Y, Yn
        intent (inout) Xn, Yn
        !
        ! Local variables
        !
        integer :: i
        real :: smod, sx, sy
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
        !---------------------------------------
        !     Calculates normal unit vector
        !     components at airfoil panel nodes
        !---------------------------------------
        !
        if (N<=1) return
        !
        call segspl(X, Xn, S, N)
        call segspl(Y, Yn, S, N)
        do i = 1, N
            sx = Yn(i)
            sy = -Xn(i)
            smod = sqrt(sx * sx + sy * sy)
            if (smod==0.0) then
                Xn(i) = -1.0
                Yn(i) = 0.0
            else
                Xn(i) = sx / smod
                Yn(i) = sy / smod
            endif
        enddo
        !
        !---- average normal vectors at corner points
        do i = 1, N - 1
            if (S(i)==S(i + 1)) then
                sx = 0.5 * (Xn(i) + Xn(i + 1))
                sy = 0.5 * (Yn(i) + Yn(i + 1))
                smod = sqrt(sx * sx + sy * sy)
                if (smod==0.0) then
                    Xn(i) = -1.0
                    Yn(i) = 0.0
                    Xn(i + 1) = -1.0
                    Yn(i + 1) = 0.0
                else
                    Xn(i) = sx / smod
                    Yn(i) = sy / smod
                    Xn(i + 1) = sx / smod
                    Yn(i + 1) = sy / smod
                endif
            endif
        enddo
        !
    end subroutine ncalc
    !*==PSILIN.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine psilin(I, Xi, Yi, Nxi, Nyi, Psi, Psi_ni, Geolin, Siglin)
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        logical :: Geolin, Siglin
        integer :: I
        real :: Nxi, Nyi, Psi, Psi_ni, Xi, Yi
        intent (in) Geolin, I, Nxi, Nyi, Siglin, Xi, Yi
        intent (inout) Psi, Psi_ni
        !
        ! Local variables
        !
        real :: apan, dsim, dsio, dsip, dsm, dso, dsp, dxinv, g0, g1, g2, gamte1, gamte2, gdif, gdif1, gdif2, &
                & gsum, gsum1, gsum2, nxo, nxp, nyo, nyp, pdif, pdni, pdx0, pdx1, pdx2, pdyy, pgam, pgamni, &
                & pgamx1, pgamx2, pgamyy, psid, psig, psigni, psigx1, psigx2, psigyy, psis, psni, psum, psx0, &
                & psx1, psx2, psyy, qtanm, rs0, rs1, rs2, rx1, rx2, ry1, ry2, scs, sdif, sds, seps, sgn, &
                & sigte1, sigte2, ssum, sx, sy, t0, t1, t2, x0, x1, x1i, x1o, x1p, x2, x2i, x2o, x2p, xjo, &
                & xjp, yjo
        integer :: io, jm, jo, jp, jq
        real :: yjp, yy, yyi, yyo, yyp
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
        !-----------------------------------------------------------------------
        !     Calculates current streamfunction Psi at panel node or wake node
        !     I due to freestream and all bound vorticity Gam on the airfoil.
        !     Sensitivities of Psi with respect to alpha (Z_ALFA) and inverse
        !     Qspec DOFs (Z_QDOF0,Z_QDOF1) which influence Gam in inverse cases.
        !     Also calculates the sensitivity vector dPsi/dGam (DZDG).
        !
        !     If SIGLIN=True, then Psi includes the effects of the viscous
        !     source distribution Sig and the sensitivity vector dPsi/dSig
        !     (DZDM) is calculated.
        !
        !     If GEOLIN=True, then the geometric sensitivity vector dPsi/dn
        !     is calculated, where n is the normal motion of the jth node.
        !
        !          Airfoil:  1   < I < N
        !          Wake:     N+1 < I < N+NW
        !-----------------------------------------------------------------------
        !
        !---- distance tolerance for determining if two points are the same
        seps = (S(N) - S(1)) * 1.0E-5
        !
        io = I
        !
        COSa = cos(ALFa)
        SINa = sin(ALFa)
        !
        do jo = 1, N
            DZDg(jo) = 0.0
            DZDn(jo) = 0.0
            DQDg(jo) = 0.0
        enddo
        !
        do jo = 1, N
            DZDm(jo) = 0.0
            DQDm(jo) = 0.0
        enddo
        !
        Z_Qinf = 0.
        Z_Alfa = 0.
        Z_Qdof0 = 0.
        Z_Qdof1 = 0.
        Z_Qdof2 = 0.
        Z_Qdof3 = 0.
        !
        Psi = 0.
        Psi_ni = 0.
        !
        QTAn1 = 0.
        QTAn2 = 0.
        qtanm = 0.
        !
        if (SHArp) then
            scs = 1.0
            sds = 0.0
        else
            scs = ANTe / DSTe
            sds = ASTe / DSTe
        endif
        !
        do jo = 1, N
            jp = jo + 1
            !
            jm = jo - 1
            jq = jp + 1
            !
            if (jo==1) then
                jm = jo
            elseif (jo==N - 1) then
                jq = jp
            elseif (jo==N) then
                jp = 1
                if ((X(jo) - X(jp))**2 + (Y(jo) - Y(jp))**2<seps**2) goto 100
            endif
            !
            dso = sqrt((X(jo) - X(jp))**2 + (Y(jo) - Y(jp))**2)
            !
            !------ skip null panel
            if (dso/=0.0) then
                !
                dsio = 1.0 / dso
                !
                apan = APAnel(jo)
                !
                rx1 = Xi - X(jo)
                ry1 = Yi - Y(jo)
                rx2 = Xi - X(jp)
                ry2 = Yi - Y(jp)
                !
                sx = (X(jp) - X(jo)) * dsio
                sy = (Y(jp) - Y(jo)) * dsio
                !
                x1 = sx * rx1 + sy * ry1
                x2 = sx * rx2 + sy * ry2
                yy = sx * ry1 - sy * rx1
                !
                rs1 = rx1 * rx1 + ry1 * ry1
                rs2 = rx2 * rx2 + ry2 * ry2
                !
                !------ set reflection flag SGN to avoid branch problems with arctan
                if (io>=1 .and. io<=N) then
                    !------- no problem on airfoil surface
                    sgn = 1.0
                else
                    !------- make sure arctan falls between  -/+  Pi/2
                    sgn = sign(1.0, yy)
                endif
                !
                !------ set log(r^2) and arctan(x/y), correcting for reflection if any
                if (io/=jo .and. rs1>0.0) then
                    g1 = log(rs1)
                    t1 = atan2(sgn * x1, sgn * yy) + (0.5 - 0.5 * sgn) * PI
                else
                    g1 = 0.0
                    t1 = 0.0
                endif
                !
                if (io/=jp .and. rs2>0.0) then
                    g2 = log(rs2)
                    t2 = atan2(sgn * x2, sgn * yy) + (0.5 - 0.5 * sgn) * PI
                else
                    g2 = 0.0
                    t2 = 0.0
                endif
                !
                x1i = sx * Nxi + sy * Nyi
                x2i = sx * Nxi + sy * Nyi
                yyi = sx * Nyi - sy * Nxi
                !
                if (Geolin) then
                    nxo = NX(jo)
                    nyo = NY(jo)
                    nxp = NX(jp)
                    nyp = NY(jp)
                    !
                    x1o = -((rx1 - x1 * sx) * nxo + (ry1 - x1 * sy) * nyo) * dsio - (sx * nxo + sy * nyo)
                    x1p = ((rx1 - x1 * sx) * nxp + (ry1 - x1 * sy) * nyp) * dsio
                    x2o = -((rx2 - x2 * sx) * nxo + (ry2 - x2 * sy) * nyo) * dsio
                    x2p = ((rx2 - x2 * sx) * nxp + (ry2 - x2 * sy) * nyp) * dsio - (sx * nxp + sy * nyp)
                    yyo = ((rx1 + x1 * sy) * nyo - (ry1 - x1 * sx) * nxo) * dsio - (sx * nyo - sy * nxo)
                    yyp = -((rx1 - x1 * sy) * nyp - (ry1 + x1 * sx) * nxp) * dsio
                endif
                !
                if (jo==N) exit
                !
                if (Siglin) then
                    !
                    !------- set up midpoint quantities
                    x0 = 0.5 * (x1 + x2)
                    rs0 = x0 * x0 + yy * yy
                    g0 = log(rs0)
                    t0 = atan2(sgn * x0, sgn * yy) + (0.5 - 0.5 * sgn) * PI
                    !
                    !------- calculate source contribution to Psi  for  1-0  half-panel
                    dxinv = 1.0 / (x1 - x0)
                    psum = x0 * (t0 - apan) - x1 * (t1 - apan) + 0.5 * yy * (g1 - g0)
                    pdif = ((x1 + x0) * psum + rs1 * (t1 - apan) - rs0 * (t0 - apan) + (x0 - x1) * yy) * dxinv
                    !
                    psx1 = -(t1 - apan)
                    psx0 = t0 - apan
                    psyy = 0.5 * (g1 - g0)
                    !
                    pdx1 = ((x1 + x0) * psx1 + psum + 2.0 * x1 * (t1 - apan) - pdif) * dxinv
                    pdx0 = ((x1 + x0) * psx0 + psum - 2.0 * x0 * (t0 - apan) + pdif) * dxinv
                    pdyy = ((x1 + x0) * psyy + 2.0 * (x0 - x1 + yy * (t1 - t0))) * dxinv
                    !
                    dsm = sqrt((X(jp) - X(jm))**2 + (Y(jp) - Y(jm))**2)
                    dsim = 1.0 / dsm
                    !
                    !CC      SIG0 = (SIG(JP) - SIG(JO))*DSIO
                    !CC      SIG1 = (SIG(JP) - SIG(JM))*DSIM
                    !CC      SSUM = SIG0 + SIG1
                    !CC      SDIF = SIG0 - SIG1
                    !
                    ssum = (SIG(jp) - SIG(jo)) * dsio + (SIG(jp) - SIG(jm)) * dsim
                    sdif = (SIG(jp) - SIG(jo)) * dsio - (SIG(jp) - SIG(jm)) * dsim
                    !
                    Psi = Psi + QOPi * (psum * ssum + pdif * sdif)
                    !
                    !------- dPsi/dm
                    DZDm(jm) = DZDm(jm) + QOPi * (-psum * dsim + pdif * dsim)
                    DZDm(jo) = DZDm(jo) + QOPi * (-psum * dsio - pdif * dsio)
                    DZDm(jp) = DZDm(jp) + QOPi * (psum * (dsio + dsim) + pdif * (dsio - dsim))
                    !
                    !------- dPsi/dni
                    psni = psx1 * x1i + psx0 * (x1i + x2i) * 0.5 + psyy * yyi
                    pdni = pdx1 * x1i + pdx0 * (x1i + x2i) * 0.5 + pdyy * yyi
                    Psi_ni = Psi_ni + QOPi * (psni * ssum + pdni * sdif)
                    !
                    qtanm = qtanm + QOPi * (psni * ssum + pdni * sdif)
                    !
                    DQDm(jm) = DQDm(jm) + QOPi * (-psni * dsim + pdni * dsim)
                    DQDm(jo) = DQDm(jo) + QOPi * (-psni * dsio - pdni * dsio)
                    DQDm(jp) = DQDm(jp) + QOPi * (psni * (dsio + dsim) + pdni * (dsio - dsim))
                    !
                    !
                    !------- calculate source contribution to Psi  for  0-2  half-panel
                    dxinv = 1.0 / (x0 - x2)
                    psum = x2 * (t2 - apan) - x0 * (t0 - apan) + 0.5 * yy * (g0 - g2)
                    pdif = ((x0 + x2) * psum + rs0 * (t0 - apan) - rs2 * (t2 - apan) + (x2 - x0) * yy) * dxinv
                    !
                    psx0 = -(t0 - apan)
                    psx2 = t2 - apan
                    psyy = 0.5 * (g0 - g2)
                    !
                    pdx0 = ((x0 + x2) * psx0 + psum + 2.0 * x0 * (t0 - apan) - pdif) * dxinv
                    pdx2 = ((x0 + x2) * psx2 + psum - 2.0 * x2 * (t2 - apan) + pdif) * dxinv
                    pdyy = ((x0 + x2) * psyy + 2.0 * (x2 - x0 + yy * (t0 - t2))) * dxinv
                    !
                    dsp = sqrt((X(jq) - X(jo))**2 + (Y(jq) - Y(jo))**2)
                    dsip = 1.0 / dsp
                    !
                    !CC         SIG2 = (SIG(JQ) - SIG(JO))*DSIP
                    !CC         SIG0 = (SIG(JP) - SIG(JO))*DSIO
                    !CC         SSUM = SIG2 + SIG0
                    !CC         SDIF = SIG2 - SIG0
                    !
                    ssum = (SIG(jq) - SIG(jo)) * dsip + (SIG(jp) - SIG(jo)) * dsio
                    sdif = (SIG(jq) - SIG(jo)) * dsip - (SIG(jp) - SIG(jo)) * dsio
                    !
                    Psi = Psi + QOPi * (psum * ssum + pdif * sdif)
                    !
                    !------- dPsi/dm
                    DZDm(jo) = DZDm(jo) + QOPi * (-psum * (dsip + dsio) - pdif * (dsip - dsio))
                    DZDm(jp) = DZDm(jp) + QOPi * (psum * dsio - pdif * dsio)
                    DZDm(jq) = DZDm(jq) + QOPi * (psum * dsip + pdif * dsip)
                    !
                    !------- dPsi/dni
                    psni = psx0 * (x1i + x2i) * 0.5 + psx2 * x2i + psyy * yyi
                    pdni = pdx0 * (x1i + x2i) * 0.5 + pdx2 * x2i + pdyy * yyi
                    Psi_ni = Psi_ni + QOPi * (psni * ssum + pdni * sdif)
                    !
                    qtanm = qtanm + QOPi * (psni * ssum + pdni * sdif)
                    !
                    DQDm(jo) = DQDm(jo) + QOPi * (-psni * (dsip + dsio) - pdni * (dsip - dsio))
                    DQDm(jp) = DQDm(jp) + QOPi * (psni * dsio - pdni * dsio)
                    DQDm(jq) = DQDm(jq) + QOPi * (psni * dsip + pdni * dsip)
                    !
                endif
                !
                !------ calculate vortex panel contribution to Psi
                dxinv = 1.0 / (x1 - x2)
                psis = 0.5 * x1 * g1 - 0.5 * x2 * g2 + x2 - x1 + yy * (t1 - t2)
                psid = ((x1 + x2) * psis + 0.5 * (rs2 * g2 - rs1 * g1 + x1 * x1 - x2 * x2)) * dxinv
                !
                psx1 = 0.5 * g1
                psx2 = -.5 * g2
                psyy = t1 - t2
                !
                pdx1 = ((x1 + x2) * psx1 + psis - x1 * g1 - psid) * dxinv
                pdx2 = ((x1 + x2) * psx2 + psis + x2 * g2 + psid) * dxinv
                pdyy = ((x1 + x2) * psyy - yy * (g1 - g2)) * dxinv
                !
                gsum1 = GAMu(jp, 1) + GAMu(jo, 1)
                gsum2 = GAMu(jp, 2) + GAMu(jo, 2)
                gdif1 = GAMu(jp, 1) - GAMu(jo, 1)
                gdif2 = GAMu(jp, 2) - GAMu(jo, 2)
                !
                gsum = GAM(jp) + GAM(jo)
                gdif = GAM(jp) - GAM(jo)
                !
                Psi = Psi + QOPi * (psis * gsum + psid * gdif)
                !
                !------ dPsi/dGam
                DZDg(jo) = DZDg(jo) + QOPi * (psis - psid)
                DZDg(jp) = DZDg(jp) + QOPi * (psis + psid)
                !
                !------ dPsi/dni
                psni = psx1 * x1i + psx2 * x2i + psyy * yyi
                pdni = pdx1 * x1i + pdx2 * x2i + pdyy * yyi
                Psi_ni = Psi_ni + QOPi * (gsum * psni + gdif * pdni)
                !
                QTAn1 = QTAn1 + QOPi * (gsum1 * psni + gdif1 * pdni)
                QTAn2 = QTAn2 + QOPi * (gsum2 * psni + gdif2 * pdni)
                !
                DQDg(jo) = DQDg(jo) + QOPi * (psni - pdni)
                DQDg(jp) = DQDg(jp) + QOPi * (psni + pdni)
                !
                if (Geolin) then
                    !
                    !------- dPsi/dn
                    DZDn(jo) = DZDn(jo) + QOPi * gsum * (psx1 * x1o + psx2 * x2o + psyy * yyo) &
                            + QOPi * gdif * (pdx1 * x1o + pdx2 * x2o + pdyy * yyo)
                    DZDn(jp) = DZDn(jp) + QOPi * gsum * (psx1 * x1p + psx2 * x2p + psyy * yyp) &
                            + QOPi * gdif * (pdx1 * x1p + pdx2 * x2p + pdyy * yyp)
                    !------- dPsi/dP
                    Z_Qdof0 = Z_Qdof0 + QOPi * ((psis - psid) * QF0(jo) + (psis + psid) * QF0(jp))
                    Z_Qdof1 = Z_Qdof1 + QOPi * ((psis - psid) * QF1(jo) + (psis + psid) * QF1(jp))
                    Z_Qdof2 = Z_Qdof2 + QOPi * ((psis - psid) * QF2(jo) + (psis + psid) * QF2(jp))
                    Z_Qdof3 = Z_Qdof3 + QOPi * ((psis - psid) * QF3(jo) + (psis + psid) * QF3(jp))
                endif
            endif
            !
            !
        enddo
        !
        psig = 0.5 * yy * (g1 - g2) + x2 * (t2 - apan) - x1 * (t1 - apan)
        pgam = 0.5 * x1 * g1 - 0.5 * x2 * g2 + x2 - x1 + yy * (t1 - t2)
        !
        psigx1 = -(t1 - apan)
        psigx2 = t2 - apan
        psigyy = 0.5 * (g1 - g2)
        pgamx1 = 0.5 * g1
        pgamx2 = -.5 * g2
        pgamyy = t1 - t2
        !
        psigni = psigx1 * x1i + psigx2 * x2i + psigyy * yyi
        pgamni = pgamx1 * x1i + pgamx2 * x2i + pgamyy * yyi
        !
        !---- TE panel source and vortex strengths
        sigte1 = 0.5 * scs * (GAMu(jp, 1) - GAMu(jo, 1))
        sigte2 = 0.5 * scs * (GAMu(jp, 2) - GAMu(jo, 2))
        gamte1 = -.5 * sds * (GAMu(jp, 1) - GAMu(jo, 1))
        gamte2 = -.5 * sds * (GAMu(jp, 2) - GAMu(jo, 2))
        !
        SIGte = 0.5 * scs * (GAM(jp) - GAM(jo))
        GAMte = -.5 * sds * (GAM(jp) - GAM(jo))
        !
        !---- TE panel contribution to Psi
        Psi = Psi + HOPi * (psig * SIGte + pgam * GAMte)
        !
        !---- dPsi/dGam
        DZDg(jo) = DZDg(jo) - HOPi * psig * scs * 0.5
        DZDg(jp) = DZDg(jp) + HOPi * psig * scs * 0.5
        !
        DZDg(jo) = DZDg(jo) + HOPi * pgam * sds * 0.5
        DZDg(jp) = DZDg(jp) - HOPi * pgam * sds * 0.5
        !
        !---- dPsi/dni
        Psi_ni = Psi_ni + HOPi * (psigni * SIGte + pgamni * GAMte)
        !
        QTAn1 = QTAn1 + HOPi * (psigni * sigte1 + pgamni * gamte1)
        QTAn2 = QTAn2 + HOPi * (psigni * sigte2 + pgamni * gamte2)
        !
        DQDg(jo) = DQDg(jo) - HOPi * (psigni * 0.5 * scs - pgamni * 0.5 * sds)
        DQDg(jp) = DQDg(jp) + HOPi * (psigni * 0.5 * scs - pgamni * 0.5 * sds)
        !
        if (Geolin) then
            !
            !----- dPsi/dn
            DZDn(jo) = DZDn(jo) + HOPi * (psigx1 * x1o + psigx2 * x2o + psigyy * yyo) * SIGte &
                    + HOPi * (pgamx1 * x1o + pgamx2 * x2o + pgamyy * yyo) * GAMte
            DZDn(jp) = DZDn(jp) + HOPi * (psigx1 * x1p + psigx2 * x2p + psigyy * yyp) * SIGte &
                    + HOPi * (pgamx1 * x1p + pgamx2 * x2p + pgamyy * yyp) * GAMte
            !
            !----- dPsi/dP
            Z_Qdof0 = Z_Qdof0 &
                    + HOPi * psig * 0.5 * (QF0(jp) - QF0(jo)) * scs - HOPi * pgam * 0.5 * (QF0(jp) - QF0(jo)) * sds
            Z_Qdof1 = Z_Qdof1 &
                    + HOPi * psig * 0.5 * (QF1(jp) - QF1(jo)) * scs - HOPi * pgam * 0.5 * (QF1(jp) - QF1(jo)) * sds
            Z_Qdof2 = Z_Qdof2 &
                    + HOPi * psig * 0.5 * (QF2(jp) - QF2(jo)) * scs - HOPi * pgam * 0.5 * (QF2(jp) - QF2(jo)) * sds
            Z_Qdof3 = Z_Qdof3 &
                    + HOPi * psig * 0.5 * (QF3(jp) - QF3(jo)) * scs - HOPi * pgam * 0.5 * (QF3(jp) - QF3(jo)) * sds
            !
        endif
        !
        !
        !**** Freestream terms
        100  Psi = Psi + QINf * (COSa * Yi - SINa * Xi)
        !
        !---- dPsi/dn
        Psi_ni = Psi_ni + QINf * (COSa * Nyi - SINa * Nxi)
        !
        QTAn1 = QTAn1 + QINf * Nyi
        QTAn2 = QTAn2 - QINf * Nxi
        !
        !---- dPsi/dQinf
        Z_Qinf = Z_Qinf + (COSa * Yi - SINa * Xi)
        !
        !---- dPsi/dalfa
        Z_Alfa = Z_Alfa - QINf * (SINa * Yi + COSa * Xi)
        !
        if (.not.LIMage) return
        !
        !
        !
        do jo = 1, N
            jp = jo + 1
            !
            jm = jo - 1
            jq = jp + 1
            !
            if (jo==1) then
                jm = jo
            elseif (jo==N - 1) then
                jq = jp
            elseif (jo==N) then
                jp = 1
                if ((X(jo) - X(jp))**2 + (Y(jo) - Y(jp))**2<seps**2) goto 99999
            endif
            !
            dso = sqrt((X(jo) - X(jp))**2 + (Y(jo) - Y(jp))**2)
            !
            !------ skip null panel
            if (dso/=0.0) then
                !
                dsio = 1.0 / dso
                !
                !cc     APAN = APANEL(JO)
                apan = PI - APAnel(jo) + 2.0 * ALFa
                !
                xjo = X(jo) + 2.0 * (YIMage + Y(jo)) * SINa
                yjo = Y(jo) - 2.0 * (YIMage + Y(jo)) * COSa
                xjp = X(jp) + 2.0 * (YIMage + Y(jp)) * SINa
                yjp = Y(jp) - 2.0 * (YIMage + Y(jp)) * COSa
                !
                rx1 = Xi - xjo
                ry1 = Yi - yjo
                rx2 = Xi - xjp
                ry2 = Yi - yjp
                !
                sx = (xjp - xjo) * dsio
                sy = (yjp - yjo) * dsio
                !
                x1 = sx * rx1 + sy * ry1
                x2 = sx * rx2 + sy * ry2
                yy = sx * ry1 - sy * rx1
                !
                rs1 = rx1 * rx1 + ry1 * ry1
                rs2 = rx2 * rx2 + ry2 * ry2
                !
                !------ set reflection flag SGN to avoid branch problems with arctan
                if (io>=1 .and. io<=N) then
                    !------- no problem on airfoil surface
                    sgn = 1.0
                else
                    !------- make sure arctan falls between  -/+  Pi/2
                    sgn = sign(1.0, yy)
                endif
                !
                !------ set log(r^2) and arctan(x/y), correcting for reflection if any
                g1 = log(rs1)
                t1 = atan2(sgn * x1, sgn * yy) + (0.5 - 0.5 * sgn) * PI
                !
                g2 = log(rs2)
                t2 = atan2(sgn * x2, sgn * yy) + (0.5 - 0.5 * sgn) * PI
                !
                x1i = sx * Nxi + sy * Nyi
                x2i = sx * Nxi + sy * Nyi
                yyi = sx * Nyi - sy * Nxi
                !
                if (Geolin) then
                    nxo = NX(jo)
                    nyo = NY(jo)
                    nxp = NX(jp)
                    nyp = NY(jp)
                    !
                    x1o = -((rx1 - x1 * sx) * nxo + (ry1 - x1 * sy) * nyo) * dsio - (sx * nxo + sy * nyo)
                    x1p = ((rx1 - x1 * sx) * nxp + (ry1 - x1 * sy) * nyp) * dsio
                    x2o = -((rx2 - x2 * sx) * nxo + (ry2 - x2 * sy) * nyo) * dsio
                    x2p = ((rx2 - x2 * sx) * nxp + (ry2 - x2 * sy) * nyp) * dsio - (sx * nxp + sy * nyp)
                    yyo = ((rx1 + x1 * sy) * nyo - (ry1 - x1 * sx) * nxo) * dsio - (sx * nyo - sy * nxo)
                    yyp = -((rx1 - x1 * sy) * nyp - (ry1 + x1 * sx) * nxp) * dsio
                endif
                !
                if (jo==N) exit
                !
                if (Siglin) then
                    !
                    !------- set up midpoint quantities
                    x0 = 0.5 * (x1 + x2)
                    rs0 = x0 * x0 + yy * yy
                    g0 = log(rs0)
                    t0 = atan2(sgn * x0, sgn * yy) + (0.5 - 0.5 * sgn) * PI
                    !
                    !------- calculate source contribution to Psi  for  1-0  half-panel
                    dxinv = 1.0 / (x1 - x0)
                    psum = x0 * (t0 - apan) - x1 * (t1 - apan) + 0.5 * yy * (g1 - g0)
                    pdif = ((x1 + x0) * psum + rs1 * (t1 - apan) - rs0 * (t0 - apan) + (x0 - x1) * yy) * dxinv
                    !
                    psx1 = -(t1 - apan)
                    psx0 = t0 - apan
                    psyy = 0.5 * (g1 - g0)
                    !
                    pdx1 = ((x1 + x0) * psx1 + psum + 2.0 * x1 * (t1 - apan) - pdif) * dxinv
                    pdx0 = ((x1 + x0) * psx0 + psum - 2.0 * x0 * (t0 - apan) + pdif) * dxinv
                    pdyy = ((x1 + x0) * psyy + 2.0 * (x0 - x1 + yy * (t1 - t0))) * dxinv
                    !
                    dsm = sqrt((X(jp) - X(jm))**2 + (Y(jp) - Y(jm))**2)
                    dsim = 1.0 / dsm
                    !
                    !CC      SIG0 = (SIG(JP) - SIG(JO))*DSIO
                    !CC      SIG1 = (SIG(JP) - SIG(JM))*DSIM
                    !CC      SSUM = SIG0 + SIG1
                    !CC      SDIF = SIG0 - SIG1
                    !
                    ssum = (SIG(jp) - SIG(jo)) * dsio + (SIG(jp) - SIG(jm)) * dsim
                    sdif = (SIG(jp) - SIG(jo)) * dsio - (SIG(jp) - SIG(jm)) * dsim
                    !
                    Psi = Psi + QOPi * (psum * ssum + pdif * sdif)
                    !
                    !------- dPsi/dm
                    DZDm(jm) = DZDm(jm) + QOPi * (-psum * dsim + pdif * dsim)
                    DZDm(jo) = DZDm(jo) + QOPi * (-psum * dsio - pdif * dsio)
                    DZDm(jp) = DZDm(jp) + QOPi * (psum * (dsio + dsim) + pdif * (dsio - dsim))
                    !
                    !------- dPsi/dni
                    psni = psx1 * x1i + psx0 * (x1i + x2i) * 0.5 + psyy * yyi
                    pdni = pdx1 * x1i + pdx0 * (x1i + x2i) * 0.5 + pdyy * yyi
                    Psi_ni = Psi_ni + QOPi * (psni * ssum + pdni * sdif)
                    !
                    qtanm = qtanm + QOPi * (psni * ssum + pdni * sdif)
                    !
                    DQDm(jm) = DQDm(jm) + QOPi * (-psni * dsim + pdni * dsim)
                    DQDm(jo) = DQDm(jo) + QOPi * (-psni * dsio - pdni * dsio)
                    DQDm(jp) = DQDm(jp) + QOPi * (psni * (dsio + dsim) + pdni * (dsio - dsim))
                    !
                    !
                    !------- calculate source contribution to Psi  for  0-2  half-panel
                    dxinv = 1.0 / (x0 - x2)
                    psum = x2 * (t2 - apan) - x0 * (t0 - apan) + 0.5 * yy * (g0 - g2)
                    pdif = ((x0 + x2) * psum + rs0 * (t0 - apan) - rs2 * (t2 - apan) + (x2 - x0) * yy) * dxinv
                    !
                    psx0 = -(t0 - apan)
                    psx2 = t2 - apan
                    psyy = 0.5 * (g0 - g2)
                    !
                    pdx0 = ((x0 + x2) * psx0 + psum + 2.0 * x0 * (t0 - apan) - pdif) * dxinv
                    pdx2 = ((x0 + x2) * psx2 + psum - 2.0 * x2 * (t2 - apan) + pdif) * dxinv
                    pdyy = ((x0 + x2) * psyy + 2.0 * (x2 - x0 + yy * (t0 - t2))) * dxinv
                    !
                    dsp = sqrt((X(jq) - X(jo))**2 + (Y(jq) - Y(jo))**2)
                    dsip = 1.0 / dsp
                    !
                    !CC         SIG2 = (SIG(JQ) - SIG(JO))*DSIP
                    !CC         SIG0 = (SIG(JP) - SIG(JO))*DSIO
                    !CC         SSUM = SIG2 + SIG0
                    !CC         SDIF = SIG2 - SIG0
                    !
                    ssum = (SIG(jq) - SIG(jo)) * dsip + (SIG(jp) - SIG(jo)) * dsio
                    sdif = (SIG(jq) - SIG(jo)) * dsip - (SIG(jp) - SIG(jo)) * dsio
                    !
                    Psi = Psi + QOPi * (psum * ssum + pdif * sdif)
                    !
                    !------- dPsi/dm
                    DZDm(jo) = DZDm(jo) + QOPi * (-psum * (dsip + dsio) - pdif * (dsip - dsio))
                    DZDm(jp) = DZDm(jp) + QOPi * (psum * dsio - pdif * dsio)
                    DZDm(jq) = DZDm(jq) + QOPi * (psum * dsip + pdif * dsip)
                    !
                    !------- dPsi/dni
                    psni = psx0 * (x1i + x2i) * 0.5 + psx2 * x2i + psyy * yyi
                    pdni = pdx0 * (x1i + x2i) * 0.5 + pdx2 * x2i + pdyy * yyi
                    Psi_ni = Psi_ni + QOPi * (psni * ssum + pdni * sdif)
                    !
                    qtanm = qtanm + QOPi * (psni * ssum + pdni * sdif)
                    !
                    DQDm(jo) = DQDm(jo) + QOPi * (-psni * (dsip + dsio) - pdni * (dsip - dsio))
                    DQDm(jp) = DQDm(jp) + QOPi * (psni * dsio - pdni * dsio)
                    DQDm(jq) = DQDm(jq) + QOPi * (psni * dsip + pdni * dsip)
                    !
                endif
                !
                !------ calculate vortex panel contribution to Psi
                dxinv = 1.0 / (x1 - x2)
                psis = 0.5 * x1 * g1 - 0.5 * x2 * g2 + x2 - x1 + yy * (t1 - t2)
                psid = ((x1 + x2) * psis + 0.5 * (rs2 * g2 - rs1 * g1 + x1 * x1 - x2 * x2)) * dxinv
                !
                psx1 = 0.5 * g1
                psx2 = -.5 * g2
                psyy = t1 - t2
                !
                pdx1 = ((x1 + x2) * psx1 + psis - x1 * g1 - psid) * dxinv
                pdx2 = ((x1 + x2) * psx2 + psis + x2 * g2 + psid) * dxinv
                pdyy = ((x1 + x2) * psyy - yy * (g1 - g2)) * dxinv
                !
                gsum1 = GAMu(jp, 1) + GAMu(jo, 1)
                gsum2 = GAMu(jp, 2) + GAMu(jo, 2)
                gdif1 = GAMu(jp, 1) - GAMu(jo, 1)
                gdif2 = GAMu(jp, 2) - GAMu(jo, 2)
                !
                gsum = GAM(jp) + GAM(jo)
                gdif = GAM(jp) - GAM(jo)
                !
                Psi = Psi - QOPi * (psis * gsum + psid * gdif)
                !
                !------ dPsi/dGam
                DZDg(jo) = DZDg(jo) - QOPi * (psis - psid)
                DZDg(jp) = DZDg(jp) - QOPi * (psis + psid)
                !
                !------ dPsi/dni
                psni = psx1 * x1i + psx2 * x2i + psyy * yyi
                pdni = pdx1 * x1i + pdx2 * x2i + pdyy * yyi
                Psi_ni = Psi_ni - QOPi * (gsum * psni + gdif * pdni)
                !
                QTAn1 = QTAn1 - QOPi * (gsum1 * psni + gdif1 * pdni)
                QTAn2 = QTAn2 - QOPi * (gsum2 * psni + gdif2 * pdni)
                !
                DQDg(jo) = DQDg(jo) - QOPi * (psni - pdni)
                DQDg(jp) = DQDg(jp) - QOPi * (psni + pdni)
                !
                if (Geolin) then
                    !
                    !------- dPsi/dn
                    DZDn(jo) = DZDn(jo) - QOPi * gsum * (psx1 * x1o + psx2 * x2o + psyy * yyo) &
                            - QOPi * gdif * (pdx1 * x1o + pdx2 * x2o + pdyy * yyo)
                    DZDn(jp) = DZDn(jp) - QOPi * gsum * (psx1 * x1p + psx2 * x2p + psyy * yyp) &
                            - QOPi * gdif * (pdx1 * x1p + pdx2 * x2p + pdyy * yyp)
                    !------- dPsi/dP
                    Z_Qdof0 = Z_Qdof0 - QOPi * ((psis - psid) * QF0(jo) + (psis + psid) * QF0(jp))
                    Z_Qdof1 = Z_Qdof1 - QOPi * ((psis - psid) * QF1(jo) + (psis + psid) * QF1(jp))
                    Z_Qdof2 = Z_Qdof2 - QOPi * ((psis - psid) * QF2(jo) + (psis + psid) * QF2(jp))
                    Z_Qdof3 = Z_Qdof3 - QOPi * ((psis - psid) * QF3(jo) + (psis + psid) * QF3(jp))
                endif
            endif
            !
            !
        enddo
        !
        psig = 0.5 * yy * (g1 - g2) + x2 * (t2 - apan) - x1 * (t1 - apan)
        pgam = 0.5 * x1 * g1 - 0.5 * x2 * g2 + x2 - x1 + yy * (t1 - t2)
        !
        psigx1 = -(t1 - apan)
        psigx2 = t2 - apan
        psigyy = 0.5 * (g1 - g2)
        pgamx1 = 0.5 * g1
        pgamx2 = -.5 * g2
        pgamyy = t1 - t2
        !
        psigni = psigx1 * x1i + psigx2 * x2i + psigyy * yyi
        pgamni = pgamx1 * x1i + pgamx2 * x2i + pgamyy * yyi
        !
        !---- TE panel source and vortex strengths
        sigte1 = 0.5 * scs * (GAMu(jp, 1) - GAMu(jo, 1))
        sigte2 = 0.5 * scs * (GAMu(jp, 2) - GAMu(jo, 2))
        gamte1 = -.5 * sds * (GAMu(jp, 1) - GAMu(jo, 1))
        gamte2 = -.5 * sds * (GAMu(jp, 2) - GAMu(jo, 2))
        !
        SIGte = 0.5 * scs * (GAM(jp) - GAM(jo))
        GAMte = -.5 * sds * (GAM(jp) - GAM(jo))
        !
        !---- TE panel contribution to Psi
        Psi = Psi + HOPi * (psig * SIGte - pgam * GAMte)
        !
        !---- dPsi/dGam
        DZDg(jo) = DZDg(jo) - HOPi * psig * scs * 0.5
        DZDg(jp) = DZDg(jp) + HOPi * psig * scs * 0.5
        !
        DZDg(jo) = DZDg(jo) - HOPi * pgam * sds * 0.5
        DZDg(jp) = DZDg(jp) + HOPi * pgam * sds * 0.5
        !
        !---- dPsi/dni
        Psi_ni = Psi_ni + HOPi * (psigni * SIGte - pgamni * GAMte)
        !
        QTAn1 = QTAn1 + HOPi * (psigni * sigte1 - pgamni * gamte1)
        QTAn2 = QTAn2 + HOPi * (psigni * sigte2 - pgamni * gamte2)
        !
        DQDg(jo) = DQDg(jo) - HOPi * (psigni * 0.5 * scs + pgamni * 0.5 * sds)
        DQDg(jp) = DQDg(jp) + HOPi * (psigni * 0.5 * scs + pgamni * 0.5 * sds)
        !
        if (Geolin) then
            !
            !----- dPsi/dn
            DZDn(jo) = DZDn(jo) + HOPi * (psigx1 * x1o + psigx2 * x2o + psigyy * yyo) * SIGte &
                    - HOPi * (pgamx1 * x1o + pgamx2 * x2o + pgamyy * yyo) * GAMte
            DZDn(jp) = DZDn(jp) + HOPi * (psigx1 * x1p + psigx2 * x2p + psigyy * yyp) * SIGte &
                    - HOPi * (pgamx1 * x1p + pgamx2 * x2p + pgamyy * yyp) * GAMte
            !
            !----- dPsi/dP
            Z_Qdof0 = Z_Qdof0 &
                    + HOPi * psig * 0.5 * (QF0(jp) - QF0(jo)) * scs + HOPi * pgam * 0.5 * (QF0(jp) - QF0(jo)) * sds
            Z_Qdof1 = Z_Qdof1 &
                    + HOPi * psig * 0.5 * (QF1(jp) - QF1(jo)) * scs + HOPi * pgam * 0.5 * (QF1(jp) - QF1(jo)) * sds
            Z_Qdof2 = Z_Qdof2 &
                    + HOPi * psig * 0.5 * (QF2(jp) - QF2(jo)) * scs + HOPi * pgam * 0.5 * (QF2(jp) - QF2(jo)) * sds
            Z_Qdof3 = Z_Qdof3 &
                    + HOPi * psig * 0.5 * (QF3(jp) - QF3(jo)) * scs + HOPi * pgam * 0.5 * (QF3(jp) - QF3(jo)) * sds
            !
        endif
        !
        !
    99999 end subroutine psilin
    !*==PSWLIN.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine pswlin(I, Xi, Yi, Nxi, Nyi, Psi, Psi_ni)
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        integer :: I
        real :: Nxi, Nyi, Psi, Psi_ni, Xi, Yi
        intent (in) I, Nxi, Nyi, Xi, Yi
        intent (inout) Psi, Psi_ni
        !
        ! Local variables
        !
        real :: apan, dsim, dsio, dsip, dsm, dso, dsp, dxinv, g0, g1, g2, pdif, pdni, pdx0, pdx1, pdx2, pdyy, &
                & psni, psum, psx0, psx1, psx2, psyy, rs0, rs1, rs2, rx1, rx2, ry1, ry2, sdif, sgn, ssum, sx, &
                & sy, t0, t1, t2, x0, x1, x1i, x2, x2i, yy, yyi
        integer :: io, jm, jo, jp, jq
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
        !--------------------------------------------------------------------
        !     Calculates current streamfunction Psi and tangential velocity
        !     Qtan at panel node or wake node I due to freestream and wake
        !     sources Sig.  Also calculates sensitivity vectors dPsi/dSig
        !     (DZDM) and dQtan/dSig (DQDM).
        !
        !          Airfoil:  1   < I < N
        !          Wake:     N+1 < I < N+NW
        !--------------------------------------------------------------------
        !
        io = I
        !
        COSa = cos(ALFa)
        SINa = sin(ALFa)
        !
        do jo = N + 1, N + NW
            DZDm(jo) = 0.0
            DQDm(jo) = 0.0
        enddo
        !
        Psi = 0.
        Psi_ni = 0.
        !
        do jo = N + 1, N + NW - 1
            !
            jp = jo + 1
            !
            jm = jo - 1
            jq = jp + 1
            if (jo==N + 1) then
                jm = jo
            elseif (jo==N + NW - 1) then
                jq = jp
            endif
            !
            dso = sqrt((X(jo) - X(jp))**2 + (Y(jo) - Y(jp))**2)
            dsio = 1.0 / dso
            !
            apan = APAnel(jo)
            !
            rx1 = Xi - X(jo)
            ry1 = Yi - Y(jo)
            rx2 = Xi - X(jp)
            ry2 = Yi - Y(jp)
            !
            sx = (X(jp) - X(jo)) * dsio
            sy = (Y(jp) - Y(jo)) * dsio
            !
            x1 = sx * rx1 + sy * ry1
            x2 = sx * rx2 + sy * ry2
            yy = sx * ry1 - sy * rx1
            !
            rs1 = rx1 * rx1 + ry1 * ry1
            rs2 = rx2 * rx2 + ry2 * ry2
            !
            if (io>=N + 1 .and. io<=N + NW) then
                sgn = 1.0
            else
                sgn = sign(1.0, yy)
            endif
            !
            if (io/=jo .and. rs1>0.0) then
                g1 = log(rs1)
                t1 = atan2(sgn * x1, sgn * yy) - (0.5 - 0.5 * sgn) * PI
            else
                g1 = 0.0
                t1 = 0.0
            endif
            !
            if (io/=jp .and. rs2>0.0) then
                g2 = log(rs2)
                t2 = atan2(sgn * x2, sgn * yy) - (0.5 - 0.5 * sgn) * PI
            else
                g2 = 0.0
                t2 = 0.0
            endif
            !
            x1i = sx * Nxi + sy * Nyi
            x2i = sx * Nxi + sy * Nyi
            yyi = sx * Nyi - sy * Nxi
            !
            !------- set up midpoint quantities
            x0 = 0.5 * (x1 + x2)
            rs0 = x0 * x0 + yy * yy
            g0 = log(rs0)
            t0 = atan2(sgn * x0, sgn * yy) - (0.5 - 0.5 * sgn) * PI
            !
            !------- calculate source contribution to Psi  for  1-0  half-panel
            dxinv = 1.0 / (x1 - x0)
            psum = x0 * (t0 - apan) - x1 * (t1 - apan) + 0.5 * yy * (g1 - g0)
            pdif = ((x1 + x0) * psum + rs1 * (t1 - apan) - rs0 * (t0 - apan) + (x0 - x1) * yy) * dxinv
            !
            psx1 = -(t1 - apan)
            psx0 = t0 - apan
            psyy = 0.5 * (g1 - g0)
            !
            pdx1 = ((x1 + x0) * psx1 + psum + 2.0 * x1 * (t1 - apan) - pdif) * dxinv
            pdx0 = ((x1 + x0) * psx0 + psum - 2.0 * x0 * (t0 - apan) + pdif) * dxinv
            pdyy = ((x1 + x0) * psyy + 2.0 * (x0 - x1 + yy * (t1 - t0))) * dxinv
            !
            dsm = sqrt((X(jp) - X(jm))**2 + (Y(jp) - Y(jm))**2)
            dsim = 1.0 / dsm
            !
            !CC         SIG0 = (SIG(JP) - SIG(JO))*DSIO
            !CC         SIG1 = (SIG(JP) - SIG(JM))*DSIM
            !CC         SSUM = SIG0 + SIG1
            !CC         SDIF = SIG0 - SIG1
            !
            ssum = (SIG(jp) - SIG(jo)) * dsio + (SIG(jp) - SIG(jm)) * dsim
            sdif = (SIG(jp) - SIG(jo)) * dsio - (SIG(jp) - SIG(jm)) * dsim
            !
            Psi = Psi + QOPi * (psum * ssum + pdif * sdif)
            !
            !------- dPsi/dm
            DZDm(jm) = DZDm(jm) + QOPi * (-psum * dsim + pdif * dsim)
            DZDm(jo) = DZDm(jo) + QOPi * (-psum * dsio - pdif * dsio)
            DZDm(jp) = DZDm(jp) + QOPi * (psum * (dsio + dsim) + pdif * (dsio - dsim))
            !
            !------- dPsi/dni
            psni = psx1 * x1i + psx0 * (x1i + x2i) * 0.5 + psyy * yyi
            pdni = pdx1 * x1i + pdx0 * (x1i + x2i) * 0.5 + pdyy * yyi
            Psi_ni = Psi_ni + QOPi * (psni * ssum + pdni * sdif)
            !
            DQDm(jm) = DQDm(jm) + QOPi * (-psni * dsim + pdni * dsim)
            DQDm(jo) = DQDm(jo) + QOPi * (-psni * dsio - pdni * dsio)
            DQDm(jp) = DQDm(jp) + QOPi * (psni * (dsio + dsim) + pdni * (dsio - dsim))
            !
            !
            !------- calculate source contribution to Psi  for  0-2  half-panel
            dxinv = 1.0 / (x0 - x2)
            psum = x2 * (t2 - apan) - x0 * (t0 - apan) + 0.5 * yy * (g0 - g2)
            pdif = ((x0 + x2) * psum + rs0 * (t0 - apan) - rs2 * (t2 - apan) + (x2 - x0) * yy) * dxinv
            !
            psx0 = -(t0 - apan)
            psx2 = t2 - apan
            psyy = 0.5 * (g0 - g2)
            !
            pdx0 = ((x0 + x2) * psx0 + psum + 2.0 * x0 * (t0 - apan) - pdif) * dxinv
            pdx2 = ((x0 + x2) * psx2 + psum - 2.0 * x2 * (t2 - apan) + pdif) * dxinv
            pdyy = ((x0 + x2) * psyy + 2.0 * (x2 - x0 + yy * (t0 - t2))) * dxinv
            !
            dsp = sqrt((X(jq) - X(jo))**2 + (Y(jq) - Y(jo))**2)
            dsip = 1.0 / dsp
            !
            !CC         SIG2 = (SIG(JQ) - SIG(JO))*DSIP
            !CC         SIG0 = (SIG(JP) - SIG(JO))*DSIO
            !CC         SSUM = SIG2 + SIG0
            !CC         SDIF = SIG2 - SIG0
            !
            ssum = (SIG(jq) - SIG(jo)) * dsip + (SIG(jp) - SIG(jo)) * dsio
            sdif = (SIG(jq) - SIG(jo)) * dsip - (SIG(jp) - SIG(jo)) * dsio
            !
            Psi = Psi + QOPi * (psum * ssum + pdif * sdif)
            !
            !------- dPsi/dm
            DZDm(jo) = DZDm(jo) + QOPi * (-psum * (dsip + dsio) - pdif * (dsip - dsio))
            DZDm(jp) = DZDm(jp) + QOPi * (psum * dsio - pdif * dsio)
            DZDm(jq) = DZDm(jq) + QOPi * (psum * dsip + pdif * dsip)
            !
            !------- dPsi/dni
            psni = psx0 * (x1i + x2i) * 0.5 + psx2 * x2i + psyy * yyi
            pdni = pdx0 * (x1i + x2i) * 0.5 + pdx2 * x2i + pdyy * yyi
            Psi_ni = Psi_ni + QOPi * (psni * ssum + pdni * sdif)
            !
            DQDm(jo) = DQDm(jo) + QOPi * (-psni * (dsip + dsio) - pdni * (dsip - dsio))
            DQDm(jp) = DQDm(jp) + QOPi * (psni * dsio - pdni * dsio)
            DQDm(jq) = DQDm(jq) + QOPi * (psni * dsip + pdni * dsip)
            !
        enddo
        !
    end subroutine pswlin
    !*==GGCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine ggcalc
        use m_xsolve, only: ludcmp, baksub
        use m_xutils, only: atanc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: abis, ag1, ag2, bwt, cbis, ds1, ds2, dsmin, psi, psiinf, psi_n, qbis, res, res1, res2, sbis, &
                & xbis, ybis
        integer :: i, j
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
        !--------------------------------------------------------------
        !     Calculates two surface vorticity (gamma) distributions
        !     for alpha = 0, 90  degrees.  These are superimposed
        !     in SPECAL or SPECCL for specified alpha or CL.
        !--------------------------------------------------------------
        !
        !---- distance of internal control point ahead of sharp TE
        !-    (fraction of smaller panel length adjacent to TE)
        bwt = 0.1
        !
        if (show_output) write (*, *) 'Calculating unit vorticity distributions ...'
        !
        do i = 1, N
            GAM(i) = 0.
            GAMu(i, 1) = 0.
            GAMu(i, 2) = 0.
        enddo
        PSIo = 0.
        !
        !---- Set up matrix system for  Psi = Psio  on airfoil surface.
        !-    The unknowns are (dGamma)i and dPsio.
        do i = 1, N
            !
            !------ calculate Psi and dPsi/dGamma array for current node
            call psilin(i, X(i), Y(i), NX(i), NY(i), psi, psi_n, .false., .true.)
            !
            psiinf = QINf * (cos(ALFa) * Y(i) - sin(ALFa) * X(i))
            !
            !------ RES1 = PSI( 0) - PSIO
            !------ RES2 = PSI(90) - PSIO
            res1 = QINf * Y(i)
            res2 = -QINf * X(i)
            !
            !------ dRes/dGamma
            do j = 1, N
                AIJ(i, j) = DZDg(j)
            enddo
            !
            do j = 1, N
                BIJ(i, j) = -DZDm(j)
            enddo
            !
            !------ dRes/dPsio
            AIJ(i, N + 1) = -1.0
            !
            GAMu(i, 1) = -res1
            GAMu(i, 2) = -res2
            !
        enddo
        !
        !---- set Kutta condition
        !-    RES = GAM(1) + GAM(N)
        res = 0.
        !
        do j = 1, N + 1
            AIJ(N + 1, j) = 0.0
        enddo
        !
        AIJ(N + 1, 1) = 1.0
        AIJ(N + 1, N) = 1.0
        !
        GAMu(N + 1, 1) = -res
        GAMu(N + 1, 2) = -res
        !
        !---- set up Kutta condition (no direct source influence)
        do j = 1, N
            BIJ(N + 1, j) = 0.
        enddo
        !
        if (SHArp) then
            !----- set zero internal velocity in TE corner
            !
            !----- set TE bisector angle
            ag1 = atan2(-YP(1), -XP(1))
            ag2 = atanc(YP(N), XP(N), ag1)
            abis = 0.5 * (ag1 + ag2)
            cbis = cos(abis)
            sbis = sin(abis)
            !
            !----- minimum panel length adjacent to TE
            ds1 = sqrt((X(1) - X(2))**2 + (Y(1) - Y(2))**2)
            ds2 = sqrt((X(N) - X(N - 1))**2 + (Y(N) - Y(N - 1))**2)
            dsmin = min(ds1, ds2)
            !
            !----- control point on bisector just ahead of TE point
            xbis = XTE - bwt * dsmin * cbis
            ybis = YTE - bwt * dsmin * sbis
            !cc       write(*,*) xbis, ybis
            !
            !----- set velocity component along bisector line
            call psilin(0, xbis, ybis, -sbis, cbis, psi, qbis, .false., .true.)
            !
            !CC--- RES = DQDGj*Gammaj + DQDMj*Massj + QINF*(COSA*CBIS + SINA*SBIS)
            res = qbis
            !
            !----- dRes/dGamma
            do j = 1, N
                AIJ(N, j) = DQDg(j)
            enddo
            !
            !----- -dRes/dMass
            do j = 1, N
                BIJ(N, j) = -DQDm(j)
            enddo
            !
            !----- dRes/dPsio
            AIJ(N, N + 1) = 0.
            !
            !----- -dRes/dUinf
            GAMu(N, 1) = -cbis
            !
            !----- -dRes/dVinf
            GAMu(N, 2) = -sbis
            !
        endif
        !
        !---- LU-factor coefficient matrix AIJ
        call ludcmp(IQX, N + 1, AIJ, AIJpiv)
        LQAij = .true.
        !
        !---- solve system for the two vorticity distributions
        call baksub(IQX, N + 1, AIJ, AIJpiv, GAMu(1, 1))
        call baksub(IQX, N + 1, AIJ, AIJpiv, GAMu(1, 2))
        !
        !---- set inviscid alpha=0,90 surface speeds for this geometry
        do i = 1, N
            QINvu(i, 1) = GAMu(i, 1)
            QINvu(i, 2) = GAMu(i, 2)
        enddo
        !
        LGAmu = .true.
        !
    end subroutine ggcalc
    !*==QWCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine qwcalc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: i
        real :: psi, psi_ni
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
        !---------------------------------------------------------------
        !     Sets inviscid tangential velocity for alpha = 0, 90
        !     on wake due to freestream and airfoil surface vorticity.
        !---------------------------------------------------------------
        !
        !---- first wake point (same as TE)
        QINvu(N + 1, 1) = QINvu(N, 1)
        QINvu(N + 1, 2) = QINvu(N, 2)
        !
        !---- rest of wake
        do i = N + 2, N + NW
            call psilin(i, X(i), Y(i), NX(i), NY(i), psi, psi_ni, .false., .false.)
            QINvu(i, 1) = QTAn1
            QINvu(i, 2) = QTAn2
        enddo
        !
    end subroutine qwcalc
    !*==QDCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine qdcalc
        use m_xsolve, only: baksub
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: i, iw, j, k
        real :: psi, psi_n, sum
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
        !-----------------------------------------------------
        !     Calculates source panel influence coefficient
        !     matrix for current airfoil and wake geometry.
        !-----------------------------------------------------
        !
        if (show_output) write (*, *) 'Calculating source influence matrix ...'
        !
        if (.not.LADij) then
            !
            !----- calculate source influence matrix for airfoil surface if it doesn't exist
            do j = 1, N
                !
                !------- multiply each dPsi/Sig vector by inverse of factored dPsi/dGam matrix
                call baksub(IQX, N + 1, AIJ, AIJpiv, BIJ(1, j))
                !
                !------- store resulting dGam/dSig = dQtan/dSig vector
                do i = 1, N
                    DIJ(i, j) = BIJ(i, j)
                enddo
                !
            enddo
            LADij = .true.
            !
        endif
        !
        !---- set up coefficient matrix of dPsi/dm on airfoil surface
        do i = 1, N
            call pswlin(i, X(i), Y(i), NX(i), NY(i), psi, psi_n)
            do j = N + 1, N + NW
                BIJ(i, j) = -DZDm(j)
            enddo
        enddo
        !
        !---- set up Kutta condition (no direct source influence)
        do j = N + 1, N + NW
            BIJ(N + 1, j) = 0.
        enddo
        !
        !---- sharp TE gamma extrapolation also has no source influence
        if (SHArp) then
            do j = N + 1, N + NW
                BIJ(N, j) = 0.
            enddo
        endif
        !
        !---- multiply by inverse of factored dPsi/dGam matrix
        do j = N + 1, N + NW
            call baksub(IQX, N + 1, AIJ, AIJpiv, BIJ(1, j))
        enddo
        !
        !---- set the source influence matrix for the wake sources
        do i = 1, N
            do j = N + 1, N + NW
                DIJ(i, j) = BIJ(i, j)
            enddo
        enddo
        !
        !**** Now we need to calculate the influence of sources on the wake velocities
        !
        !---- calculcate dQtan/dGam and dQtan/dSig at the wake points
        do i = N + 1, N + NW
            !
            iw = i - N
            if (iw > iwx .or. iw <= 0) then
                exit
            end if
            !
            !------ airfoil contribution at wake panel node
            call psilin(i, X(i), Y(i), NX(i), NY(i), psi, psi_n, .false., .true.)
            !
            do j = 1, N
                CIJ(iw, j) = DQDg(j)
            enddo
            !
            do j = 1, N
                DIJ(i, j) = DQDm(j)
            enddo
            !
            !------ wake contribution
            call pswlin(i, X(i), Y(i), NX(i), NY(i), psi, psi_n)
            !
            do j = N + 1, N + NW
                DIJ(i, j) = DQDm(j)
            enddo
            !
        enddo
        !
        !---- add on effect of all sources on airfoil vorticity which effects wake Qtan
        do i = N + 1, N + NW
            iw = i - N
            !
            !------ airfoil surface source contribution first
            do j = 1, N
                sum = 0.
                do k = 1, N
                    sum = sum + CIJ(iw, k) * DIJ(k, j)
                enddo
                DIJ(i, j) = DIJ(i, j) + sum
            enddo
            !
            !------ wake source contribution next
            do j = N + 1, N + NW
                sum = 0.
                do k = 1, N
                    sum = sum + CIJ(iw, k) * BIJ(k, j)
                enddo
                DIJ(i, j) = DIJ(i, j) + sum
            enddo
            !
        enddo
        !
        !---- make sure first wake point has same velocity as trailing edge
        do j = 1, N + NW
            DIJ(N + 1, j) = DIJ(N, j)
        enddo
        !
        LWDij = .true.
        !
    end subroutine qdcalc
    !*==XYWAKE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine xywake
        use m_xutils, only: setexp
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: ds, ds1, psi, psi_x, psi_y, smod, sx, sy
        integer :: i
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
        !-----------------------------------------------------
        !     Sets wake coordinate array for current surface
        !     vorticity and/or mass source distributions.
        !-----------------------------------------------------
        !
        if (show_output) write (*, *) 'Calculating wake trajectory ...'
        !
        !---- number of wake points
        NW = N / 12 + 10 * int(WAKlen)
        if (NW>IWX) then
            if (show_output) write (*, *) 'Array size (IWX) too small.  Last wake point index reduced.'
            NW = IWX
        endif
        !
        ds1 = 0.5 * (S(2) - S(1) + S(N) - S(N - 1))
        call setexp(SNEw(N + 1), ds1, WAKlen * CHOrd, NW)
        !

        !      write(*,*) waklen, chord, waklen*chord
        !      write(*,*) ds1
        !      do i = n+1, n+nw
        !        write(*,*) i-n, snew(i)
        !      enddo

        XTE = 0.5 * (X(1) + X(N))
        YTE = 0.5 * (Y(1) + Y(N))
        !
        !---- set first wake point a tiny distance behind TE
        i = N + 1
        sx = 0.5 * (YP(N) - YP(1))
        sy = 0.5 * (XP(1) - XP(N))
        smod = sqrt(sx**2 + sy**2)
        NX(i) = sx / smod
        NY(i) = sy / smod
        X(i) = XTE - 0.0001 * NY(i)
        Y(i) = YTE + 0.0001 * NX(i)
        S(i) = S(N)
        !
        !---- calculate streamfunction gradient components at first point
        call psilin(i, X(i), Y(i), 1.0, 0.0, psi, psi_x, .false., .false.)
        call psilin(i, X(i), Y(i), 0.0, 1.0, psi, psi_y, .false., .false.)
        !
        !---- set unit vector normal to wake at first point
        NX(i + 1) = -psi_x / sqrt(psi_x**2 + psi_y**2)
        NY(i + 1) = -psi_y / sqrt(psi_x**2 + psi_y**2)
        !
        !---- set angle of wake panel normal
        APAnel(i) = atan2(psi_y, psi_x)
        !
        !---- set rest of wake points
        do i = N + 2, N + NW
            ds = SNEw(i) - SNEw(i - 1)
            !
            !------ set new point DS downstream of last point
            X(i) = X(i - 1) - ds * NY(i)
            Y(i) = Y(i - 1) + ds * NX(i)
            S(i) = S(i - 1) + ds
            !
            if (i/=N + NW) then
                !
                !------- calculate normal vector for next point
                call psilin(i, X(i), Y(i), 1.0, 0.0, psi, psi_x, .false., .false.)
                call psilin(i, X(i), Y(i), 0.0, 1.0, psi, psi_y, .false., .false.)
                !
                NX(i + 1) = -psi_x / sqrt(psi_x**2 + psi_y**2)
                NY(i + 1) = -psi_y / sqrt(psi_x**2 + psi_y**2)
                !
                !------- set angle of wake panel normal
                APAnel(i) = atan2(psi_y, psi_x)
            endif
            !
        enddo
        !
        !---- set wake presence flag and corresponding alpha
        LWAke = .true.
        AWAke = ALFa
        !
        !---- old source influence matrix is invalid for the new wake geometry
        LWDij = .false.
        !
    end subroutine xywake
    !*==STFIND.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine stfind
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: dgam, ds
        integer :: i
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
        !-----------------------------------------
        !     Locates stagnation point arc length
        !     location SST and panel index IST.
        !-----------------------------------------
        !
        do i = 1, N - 1
            if (GAM(i)>=0.0 .and. GAM(i + 1)<0.0) goto 100
        enddo
        !
        if (show_output) write (*, *) 'STFIND: Stagnation point not found. Continuing ...'
        i = N / 2
        !
        !
        100  IST = i
        dgam = GAM(i + 1) - GAM(i)
        ds = S(i + 1) - S(i)
        !
        !---- evaluate so as to minimize roundoff for very small GAM(I) or GAM(I+1)
        if (GAM(i)<-GAM(i + 1)) then
            SST = S(i) - ds * (GAM(i) / dgam)
        else
            SST = S(i + 1) - ds * (GAM(i + 1) / dgam)
        endif
        !
        !---- tweak stagnation point if it falls right on a node (very unlikely)
        if (SST<=S(i)) SST = S(i) + 1.0E-7
        if (SST>=S(i + 1)) SST = S(i + 1) - 1.0E-7
        !
        SST_go = (SST - S(i + 1)) / dgam
        SST_gp = (S(i) - SST) / dgam
        !
    end subroutine stfind
    !*==IBLPAN.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine iblpan
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: i, ibl, iblmax, is, iw
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
        !-------------------------------------------------------------
        !     Sets  BL location -> panel location  pointer array IPAN
        !-------------------------------------------------------------
        !
        !---- top surface first
        is = 1
        !
        ibl = 1
        do i = IST, 1, -1
            ibl = ibl + 1
            IPAn(ibl, is) = i
            VTI(ibl, is) = 1.0
        enddo
        !
        IBLte(is) = ibl
        NBL(is) = ibl
        !
        !---- bottom surface next
        is = 2
        !
        ibl = 1
        do i = IST + 1, N
            ibl = ibl + 1
            IPAn(ibl, is) = i
            VTI(ibl, is) = -1.0
        enddo
        !
        !---- wake
        IBLte(is) = ibl
        !
        do iw = 1, NW
            i = N + iw
            ibl = IBLte(is) + iw
            IPAn(ibl, is) = i
            VTI(ibl, is) = -1.0
        enddo
        !
        NBL(is) = IBLte(is) + NW
        !
        !---- upper wake pointers (for plotting only)
        do iw = 1, NW
            IPAn(IBLte(1) + iw, 1) = IPAn(IBLte(2) + iw, 2)
            VTI(IBLte(1) + iw, 1) = 1.0
        enddo
        !
        !
        iblmax = max(IBLte(1), IBLte(2)) + NW
        if (iblmax>IVX) then
            if (show_output) then
                write (*, *) ' ***  BL array overflow.'
                write (*, *) ' ***  Increase IVX to at least', iblmax
            endif
            stop
        endif
        !
        LIPan = .true.
    end subroutine iblpan
    !*==XICALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine xicalc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: aa, bb, crosp, dwdxte, dxssi, telrat, xeps, zn
        integer :: i, ibl, ibl1, ibl2, is, is1, is2, iw
        real, save :: xfeps
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
        !-------------------------------------------------------------
        !     Sets BL arc length array on each airfoil side and wake
        !-------------------------------------------------------------
        data xfeps/1.0E-7/
        !
        !---- minimum xi node arc length near stagnation point
        xeps = xfeps * (S(N) - S(1))
        !
        is = 1
        !
        XSSi(1, is) = 0.
        !
        do ibl = 2, IBLte(is)
            i = IPAn(ibl, is)
            XSSi(ibl, is) = max(SST - S(i), xeps)
        enddo
        !
        !
        is = 2
        !
        XSSi(1, is) = 0.
        !
        do ibl = 2, IBLte(is)
            i = IPAn(ibl, is)
            XSSi(ibl, is) = max(S(i) - SST, xeps)
        enddo
        !
        !
        is1 = 1
        is2 = 2
        !
        ibl1 = IBLte(is1) + 1
        XSSi(ibl1, is1) = XSSi(ibl1 - 1, is1)
        !
        ibl2 = IBLte(is2) + 1
        XSSi(ibl2, is2) = XSSi(ibl2 - 1, is2)
        !
        do ibl = IBLte(is) + 2, NBL(is)
            i = IPAn(ibl, is)
            dxssi = sqrt((X(i) - X(i - 1))**2 + (Y(i) - Y(i - 1))**2)
            !
            ibl1 = IBLte(is1) + ibl - IBLte(is)
            ibl2 = IBLte(is2) + ibl - IBLte(is)
            XSSi(ibl1, is1) = XSSi(ibl1 - 1, is1) + dxssi
            XSSi(ibl2, is2) = XSSi(ibl2 - 1, is2) + dxssi
        enddo
        !
        !---- trailing edge flap length to TE gap ratio
        telrat = 2.50
        !
        !---- set up parameters for TE flap cubics
        !
        !cc   DWDXTE = YP(1)/XP(1) + YP(N)/XP(N)    !!! BUG  2/2/95
        !
        crosp = (XP(1) * YP(N) - YP(1) * XP(N)) / sqrt((XP(1)**2 + YP(1)**2) * (XP(N)**2 + YP(N)**2))
        dwdxte = crosp / sqrt(1.0 - crosp**2)
        !
        !---- limit cubic to avoid absurd TE gap widths
        dwdxte = max(dwdxte, -3.0 / telrat)
        dwdxte = min(dwdxte, 3.0 / telrat)
        !
        aa = 3.0 + telrat * dwdxte
        bb = -2.0 - telrat * dwdxte
        !
        if (SHArp) then
            do iw = 1, NW
                WGAp(iw) = 0.
            enddo
        else
            !----- set TE flap (wake gap) array
            is = 2
            do iw = 1, NW
                ibl = IBLte(is) + iw
                zn = 1.0 - (XSSi(ibl, is) - XSSi(IBLte(is), is)) / (telrat * ANTe)
                WGAp(iw) = 0.
                if (zn>=0.0) WGAp(iw) = ANTe * (aa + bb * zn) * zn**2
            enddo
        endif
        !
    end subroutine xicalc
    !*==UICALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine uicalc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: i, ibl, is
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
        !--------------------------------------------------------------
        !     Sets inviscid Ue from panel inviscid tangential velocity
        !--------------------------------------------------------------
        !
        do is = 1, 2
            UINv(1, is) = 0.
            UINv_a(1, is) = 0.
            do ibl = 2, NBL(is)
                i = IPAn(ibl, is)
                UINv(ibl, is) = VTI(ibl, is) * QINv(i)
                UINv_a(ibl, is) = VTI(ibl, is) * QINv_a(i)
            enddo
        enddo
        !
    end subroutine uicalc
    !*==UECALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine uecalc
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: i, ibl, is
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
        !--------------------------------------------------------------
        !     Sets viscous Ue from panel viscous tangential velocity
        !--------------------------------------------------------------
        !
        do is = 1, 2
            UEDg(1, is) = 0.
            do ibl = 2, NBL(is)
                i = IPAn(ibl, is)
                UEDg(ibl, is) = VTI(ibl, is) * QVIs(i)
            enddo
        enddo
        !
    end subroutine uecalc
    !*==QVFUE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine qvfue
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: i, ibl, is
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
        !--------------------------------------------------------------
        !     Sets panel viscous tangential velocity from viscous Ue
        !--------------------------------------------------------------
        !
        do is = 1, 2
            do ibl = 2, NBL(is)
                i = IPAn(ibl, is)
                QVIs(i) = VTI(ibl, is) * UEDg(ibl, is)
            enddo
        enddo
        !
    end subroutine qvfue
    !*==QISET.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine qiset
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: i
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
        !-------------------------------------------------------
        !     Sets inviscid panel tangential velocity for
        !     current alpha.
        !-------------------------------------------------------
        !
        COSa = cos(ALFa)
        SINa = sin(ALFa)
        !
        do i = 1, N + NW
            QINv(i) = COSa * QINvu(i, 1) + SINa * QINvu(i, 2)
            QINv_a(i) = -SINa * QINvu(i, 1) + COSa * QINvu(i, 2)
        enddo
        !
    end subroutine qiset
    !*==GAMQV.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine gamqv
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: i
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
        do i = 1, N
            GAM(i) = QVIs(i)
            GAM_a(i) = QINv_a(i)
        enddo
        !
    end subroutine gamqv
    !*==STMOVE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine stmove
        use s_xbl, only: iblsys
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: dudx, ueps
        integer :: i, ibl, idif, is, istold
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
        !---------------------------------------------------
        !     Moves stagnation point location to new panel.
        !---------------------------------------------------
        !
        !---- locate new stagnation point arc length SST from GAM distribution
        istold = IST
        call stfind
        !
        if (istold==IST) then
            !
            !----- recalculate new arc length array
            call xicalc
            !
        else
            !
            !CC       WRITE(*,*) 'STMOVE: Resetting stagnation point'
            !
            !----- set new BL position -> panel position  pointers
            call iblpan
            !
            !----- set new inviscid BL edge velocity UINV from QINV
            call uicalc
            !
            !----- recalculate new arc length array
            call xicalc
            !
            !----- set  BL position -> system line  pointers
            call iblsys
            !
            if (IST>istold) then
                !------ increase in number of points on top side (IS=1)
                idif = IST - istold
                !
                ITRan(1) = ITRan(1) + idif
                ITRan(2) = ITRan(2) - idif
                !
                !------ move top side BL variables downstream
                do ibl = NBL(1), idif + 2, -1
                    CTAu(ibl, 1) = CTAu(ibl - idif, 1)
                    THEt(ibl, 1) = THEt(ibl - idif, 1)
                    DSTr(ibl, 1) = DSTr(ibl - idif, 1)
                    UEDg(ibl, 1) = UEDg(ibl - idif, 1)
                enddo
                !
                !------ set BL variables between old and new stagnation point
                dudx = UEDg(idif + 2, 1) / XSSi(idif + 2, 1)
                do ibl = idif + 1, 2, -1
                    CTAu(ibl, 1) = CTAu(idif + 2, 1)
                    THEt(ibl, 1) = THEt(idif + 2, 1)
                    DSTr(ibl, 1) = DSTr(idif + 2, 1)
                    UEDg(ibl, 1) = dudx * XSSi(ibl, 1)
                enddo
                !
                !------ move bottom side BL variables upstream
                do ibl = 2, NBL(2)
                    CTAu(ibl, 2) = CTAu(ibl + idif, 2)
                    THEt(ibl, 2) = THEt(ibl + idif, 2)
                    DSTr(ibl, 2) = DSTr(ibl + idif, 2)
                    UEDg(ibl, 2) = UEDg(ibl + idif, 2)
                enddo
                !
            else
                !------ increase in number of points on bottom side (IS=2)
                idif = istold - IST
                !
                ITRan(1) = ITRan(1) - idif
                ITRan(2) = ITRan(2) + idif
                !
                !------ move bottom side BL variables downstream
                do ibl = NBL(2), idif + 2, -1
                    CTAu(ibl, 2) = CTAu(ibl - idif, 2)
                    THEt(ibl, 2) = THEt(ibl - idif, 2)
                    DSTr(ibl, 2) = DSTr(ibl - idif, 2)
                    UEDg(ibl, 2) = UEDg(ibl - idif, 2)
                enddo
                !
                !------ set BL variables between old and new stagnation point
                dudx = UEDg(idif + 2, 2) / XSSi(idif + 2, 2)


                !        write(*,*) 'idif Ue xi dudx',
                !     &    idif, UEDG(idif+2,2), xssi(idif+2,2), dudx

                do ibl = idif + 1, 2, -1
                    CTAu(ibl, 2) = CTAu(idif + 2, 2)
                    THEt(ibl, 2) = THEt(idif + 2, 2)
                    DSTr(ibl, 2) = DSTr(idif + 2, 2)
                    UEDg(ibl, 2) = dudx * XSSi(ibl, 2)
                enddo

                !        write(*,*) 'Uenew xinew', idif+1, uedg(idif+1,2), xssi(idif+1,2)

                !
                !------ move top side BL variables upstream
                do ibl = 2, NBL(1)
                    CTAu(ibl, 1) = CTAu(ibl + idif, 1)
                    THEt(ibl, 1) = THEt(ibl + idif, 1)
                    DSTr(ibl, 1) = DSTr(ibl + idif, 1)
                    UEDg(ibl, 1) = UEDg(ibl + idif, 1)
                enddo
            endif
            !
            !----- tweak Ue so it's not zero, in case stag. point is right on node
            ueps = 1.0E-7
            do is = 1, 2
                do ibl = 2, NBL(is)
                    i = IPAn(ibl, is)
                    if (UEDg(ibl, is)<=ueps) then
                        UEDg(ibl, is) = ueps
                        QVIs(i) = VTI(ibl, is) * ueps
                        GAM(i) = VTI(ibl, is) * ueps
                    endif
                enddo
            enddo
            !
        endif
        !
        !---- set new mass array since Ue has been tweaked
        do is = 1, 2
            do ibl = 2, NBL(is)
                MASs(ibl, is) = DSTr(ibl, is) * UEDg(ibl, is)
            enddo
        enddo
        !
    end subroutine stmove
    !*==UESET.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine ueset
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: dui, ue_m
        integer :: i, ibl, is, j, jbl, js
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
        !---------------------------------------------------------
        !     Sets Ue from inviscid Ue plus all source influence
        !---------------------------------------------------------
        !
        do is = 1, 2
            do ibl = 2, NBL(is)
                i = IPAn(ibl, is)
                !
                dui = 0.
                do js = 1, 2
                    do jbl = 2, NBL(js)
                        j = IPAn(jbl, js)
                        ue_m = -VTI(ibl, is) * VTI(jbl, js) * DIJ(i, j)
                        dui = dui + ue_m * MASs(jbl, js)
                    enddo
                enddo
                !
                UEDg(ibl, is) = UINv(ibl, is) + dui
                !
            enddo
        enddo
        !
    end subroutine ueset
    !*==DSSET.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019


    subroutine dsset
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: ibl, is
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
        do is = 1, 2
            do ibl = 2, NBL(is)
                DSTr(ibl, is) = MASs(ibl, is) / UEDg(ibl, is)
            enddo
        enddo
        !
    end subroutine dsset

end module m_xpanel