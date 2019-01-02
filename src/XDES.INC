!     INCLUDE file for XFOIL design routines...
!
!---- Statement functions used to offset and scale all plots with blowups
XMOD(XTMP) = XSF * (XTMP - XOFF)
YMOD(YTMP) = YSF * (YTMP - YOFF)
!
YMODP(YTMP) = YSFP * (YTMP - YOFF)
!