!
!----- CIRCLE.INC include file for circle-plane operations
!
!                                                              n
! ICX   number of circle-plane points for complex mapping   ( 2  + 1 )
! IMX   number of complex mapping coefficients  Cn

PARAMETER (ICX = 257)
PARAMETER (IMX = (ICX - 1) / 4)
!
COMPLEX ZCOLDW, DZTE, CHORDZ, ZLEOLD, ZC, ZC_CN, PIQ, CN, EIW
!
COMMON/CPI01/ NC, MC, MCT
!
COMMON/CPR01/ PI, AGTE, AG0, QIM0, QIMOLD, &
        DWC, WC(ICX), SC(ICX), &
        SCOLD(ICX), XCOLD(ICX), YCOLD(ICX)
!
COMMON/CPC01/ DZTE, CHORDZ, ZLEOLD, ZCOLDW(ICX), &
        ZC(ICX), ZC_CN(ICX, IMX / 4), &
        PIQ(ICX), CN(0:IMX), EIW(ICX, 0:IMX)
!

!   NC         number of circle plane points, must be 2**n + 1
!   MC         number of Fourier harmonics of P(w) + iQ(w)
!   MCT        number of Fourier harmonics for which dZC/dCN are calculated
!
!   PI         3.1415926
!   AGTE       trailing edge angle/pi
!   AG0        angle of airfoil surface at first point
!   QIM0       Q(w) offset   = Q(0)
!   QIMOLD     Q(w) offset for old airfoil
!   DWC        increment of circle-plane coordinate w,  DWC = 2 pi/(NC-1)
!   WC(.)      circle plane coordinate w for Fourier operations
!   SC(.)      normalized arc length array s(w)
!   SCOLD(.)   normalized arc length s(w) of old airfoil
!   XCOLD(.)   x coordinate x(w) of old airfoil
!   YCOLD(.)   y coordinate y(w) of old airfoil
!
!   DZTE       trailing edge gap specified in the complex plane
!   CHORDZ     airfoil chord specified in the complex plane
!   ZLEOLD     leading edge of old airfoil
!   ZCOLDW(.)  d(x+iy)/dw of old airfoil
!   ZC(.)      complex airfoil coordinates derived from P(w) + iQ(w)
!   ZC_CN(..)  sensitivities dZC/dCN for driving geometry constraints
!   PIQ(.)     complex harmonic function P(w) + iQ(w)
!   CN(.)      Fourier coefficients of P(w) + iQ(w)
!   EIW(..)    complex number  exp(inw)  array on the unit circle 