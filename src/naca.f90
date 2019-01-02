!***********************************************************************
!    Module:  naca.f
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

SUBROUTINE NACA4(IDES, XX, YT, YC, NSIDE, XB, YB, NB, NAME)
    REAL XX(NSIDE), YT(NSIDE), YC(NSIDE)
    REAL XB(2 * NSIDE), YB(2 * NSIDE)
    REAL M
    CHARACTER*(*) NAME
    !
    CHARACTER*10 DIGITS
    DATA DIGITS / '0123456789' /
    !
    !---- TE point bunching parameter
    DATA AN / 1.5 /
    !
    N4 = IDES / 1000
    N3 = (IDES - N4 * 1000) / 100
    N2 = (IDES - N4 * 1000 - N3 * 100) / 10
    N1 = (IDES - N4 * 1000 - N3 * 100 - N2 * 10)
    !
    M = FLOAT(N4) / 100.0
    P = FLOAT(N3) / 10.0
    T = FLOAT(N2 * 10 + N1) / 100.0
    !
    ANP = AN + 1.0
    DO I = 1, NSIDE
        FRAC = FLOAT(I - 1) / FLOAT(NSIDE - 1)
        IF(I == NSIDE) THEN
            XX(I) = 1.0
        ELSE
            XX(I) = 1.0 - ANP * FRAC * (1.0 - FRAC)**AN - (1.0 - FRAC)**ANP
        ENDIF
        YT(I) = (0.29690 * SQRT(XX(I))&
                - 0.12600 * XX(I)&
                - 0.35160 * XX(I)**2&
                + 0.28430 * XX(I)**3&
                - 0.10150 * XX(I)**4) * T / 0.20
        IF(XX(I) < P) THEN
            YC(I) = M / P**2 * (2.0 * P * XX(I) - XX(I)**2)
        ELSE
            YC(I) = M / (1.0 - P)**2 * ((1.0 - 2.0 * P) + 2.0 * P * XX(I) - XX(I)**2)
        ENDIF
    end do
    !
    IB = 0
    DO I = NSIDE, 1, -1
        IB = IB + 1
        XB(IB) = XX(I)
        YB(IB) = YC(I) + YT(I)
    end do
    DO I = 2, NSIDE
        IB = IB + 1
        XB(IB) = XX(I)
        YB(IB) = YC(I) - YT(I)
    end do
    NB = IB
    !
    NAME = 'NACA'
    NAME(6:9) = DIGITS(N4 + 1:N4 + 1)&
            // DIGITS(N3 + 1:N3 + 1)&
            // DIGITS(N2 + 1:N2 + 1)&
            // DIGITS(N1 + 1:N1 + 1)
    !
    RETURN
END


SUBROUTINE NACA5(IDES, XX, YT, YC, NSIDE, XB, YB, NB, NAME)
    REAL XX(NSIDE), YT(NSIDE), YC(NSIDE)
    REAL XB(2 * NSIDE), YB(2 * NSIDE)
    REAL M
    !
    CHARACTER*(*) NAME
    !
    CHARACTER*10 DIGITS
    DATA DIGITS / '0123456789' /
    !
    !---- TE point bunching parameter
    DATA AN / 1.5 /
    !
    N5 = IDES / 10000
    N4 = (IDES - N5 * 10000) / 1000
    N3 = (IDES - N5 * 10000 - N4 * 1000) / 100
    N2 = (IDES - N5 * 10000 - N4 * 1000 - N3 * 100) / 10
    N1 = (IDES - N5 * 10000 - N4 * 1000 - N3 * 100 - N2 * 10)
    !
    N543 = 100 * N5 + 10 * N4 + N3
    !
    IF      (N543 == 210) THEN
        !c     P = 0.05
        M = 0.0580
        C = 361.4
    ELSE IF (N543 == 220) THEN
        !c     P = 0.10
        M = 0.1260
        C = 51.64
    ELSE IF (N543 == 230) THEN
        !c     P = 0.15
        M = 0.2025
        C = 15.957
    ELSE IF (N543 == 240) THEN
        !c     P = 0.20
        M = 0.2900
        C = 6.643
    ELSE IF (N543 == 250) THEN
        !c     P = 0.25
        M = 0.3910
        C = 3.230
    ELSE
        WRITE(*, *) 'Illegal 5-digit designation'
        WRITE(*, *) 'First three digits must be 210, 220, ... 250'
        IDES = 0
        RETURN
    ENDIF
    !
    !
    T = FLOAT(N2 * 10 + N1) / 100.0
    !
    ANP = AN + 1.0
    DO I = 1, NSIDE
        FRAC = FLOAT(I - 1) / FLOAT(NSIDE - 1)
        IF(I == NSIDE) THEN
            XX(I) = 1.0
        ELSE
            XX(I) = 1.0 - ANP * FRAC * (1.0 - FRAC)**AN - (1.0 - FRAC)**ANP
        ENDIF
        !
        YT(I) = (0.29690 * SQRT(XX(I))&
                - 0.12600 * XX(I)&
                - 0.35160 * XX(I)**2&
                + 0.28430 * XX(I)**3&
                - 0.10150 * XX(I)**4) * T / 0.20
        IF(XX(I) < M) THEN
            YC(I) = (C / 6.0) * (XX(I)**3 - 3.0 * M * XX(I)**2&
                    + M * M * (3.0 - M) * XX(I))
        ELSE
            YC(I) = (C / 6.0) * M**3 * (1.0 - XX(I))
        ENDIF
    end do
    !
    IB = 0
    DO I = NSIDE, 1, -1
        IB = IB + 1
        XB(IB) = XX(I)
        YB(IB) = YC(I) + YT(I)
    end do
    DO I = 2, NSIDE
        IB = IB + 1
        XB(IB) = XX(I)
        YB(IB) = YC(I) - YT(I)
    end do
    NB = IB
    !
    NAME = 'NACA'
    NAME(6:10) = DIGITS(N5 + 1:N5 + 1)&
            // DIGITS(N4 + 1:N4 + 1)&
            // DIGITS(N3 + 1:N3 + 1)&
            // DIGITS(N2 + 1:N2 + 1)&
            // DIGITS(N1 + 1:N1 + 1)
    !
    RETURN
END