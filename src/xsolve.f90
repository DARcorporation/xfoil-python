!***********************************************************************
!    Module:  xsolve.f
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


SUBROUTINE GAUSS(NSIZ, NN, Z, R, NRHS)
    !     *******************************************************
    !     *                                                     *
    !     *   Solves general NxN system in NN unknowns          *
    !     *    with arbitrary number (NRHS) of righthand sides. *
    !     *   Assumes system is invertible...                   *
    !     *    ...if it isn't, a divide by zero will result.    *
    !     *                                                     *
    !     *   Z is the coefficient matrix...                    *
    !     *     ...destroyed during solution process.           *
    !     *   R is the righthand side(s)...                     *
    !     *     ...replaced by the solution vector(s).          *
    !     *                                                     *
    !     *                              Mark Drela  1984       *
    !     *******************************************************
    !
    DIMENSION Z(NSIZ, NSIZ), R(NSIZ, NRHS)
    !
    DO 1 NP = 1, NN - 1
        NP1 = NP + 1
        !
        !------ find max pivot index NX
        NX = NP
        DO 11 N = NP1, NN
            IF(ABS(Z(N, NP)) - ABS(Z(NX, NP))) 11, 11, 111
            111      NX = N
        11   CONTINUE
        !
        PIVOT = 1.0 / Z(NX, NP)
        !
        !------ switch pivots
        Z(NX, NP) = Z(NP, NP)
        !
        !------ switch rows & normalize pivot row
        DO 12 L = NP1, NN
            TEMP = Z(NX, L) * PIVOT
            Z(NX, L) = Z(NP, L)
            Z(NP, L) = TEMP
        12   CONTINUE
        !
        DO 13 L = 1, NRHS
            TEMP = R(NX, L) * PIVOT
            R(NX, L) = R(NP, L)
            R(NP, L) = TEMP
        13   CONTINUE
        !
        !------ forward eliminate everything
        DO 15 K = NP1, NN
            ZTMP = Z(K, NP)
            !
            !          IF(ZTMP == 0.0) GO TO 15
            !
            DO 151 L = NP1, NN
                Z(K, L) = Z(K, L) - ZTMP * Z(NP, L)
            151     CONTINUE
            DO 152 L = 1, NRHS
                R(K, L) = R(K, L) - ZTMP * R(NP, L)
            152     CONTINUE
        15   CONTINUE
        !
    1 CONTINUE
    !
    !---- solve for last row
    DO 2 L = 1, NRHS
        R(NN, L) = R(NN, L) / Z(NN, NN)
    2 CONTINUE
    !
    !---- back substitute everything
    DO 3 NP = NN - 1, 1, -1
        NP1 = NP + 1
        DO 31 L = 1, NRHS
            DO 310 K = NP1, NN
                R(NP, L) = R(NP, L) - Z(NP, K) * R(K, L)
            310     CONTINUE
        31   CONTINUE
    3 CONTINUE
    !
    RETURN
END
! GAUSS


SUBROUTINE CGAUSS(NSIZ, NN, Z, R, NRHS)
    !********************************************
    !     Solves general complex linear systems.
    !********************************************
    COMPLEX Z(NSIZ, NSIZ), R(NSIZ, NRHS)
    COMPLEX PIVOT, TEMP, ZTMP
    !
    DO 1 NP = 1, NN - 1
        NP1 = NP + 1
        !
        !------ find max pivot index NX
        NX = NP
        DO 11 N = NP1, NN
            IF(ABS(Z(N, NP)) - ABS(Z(NX, NP))) 11, 11, 111
            111      NX = N
        11   CONTINUE
        !
        PIVOT = (1.0, 0.0) / Z(NX, NP)
        !
        !------ switch pivots
        Z(NX, NP) = Z(NP, NP)
        !
        !------ switch rows & normalize pivot row
        DO 12 L = NP1, NN
            TEMP = Z(NX, L) * PIVOT
            Z(NX, L) = Z(NP, L)
            Z(NP, L) = TEMP
        12   CONTINUE
        !
        DO 13 L = 1, NRHS
            TEMP = R(NX, L) * PIVOT
            R(NX, L) = R(NP, L)
            R(NP, L) = TEMP
        13   CONTINUE
        !
        !------ forward eliminate everything
        DO 15 K = NP1, NN
            ZTMP = Z(K, NP)
            !
            !          IF(ZTMP == 0.0) GO TO 15
            !
            DO 151 L = NP1, NN
                Z(K, L) = Z(K, L) - ZTMP * Z(NP, L)
            151     CONTINUE
            DO 152 L = 1, NRHS
                R(K, L) = R(K, L) - ZTMP * R(NP, L)
            152     CONTINUE
        15   CONTINUE
        !
    1 CONTINUE
    !
    !---- solve for last row
    DO 2 L = 1, NRHS
        R(NN, L) = R(NN, L) / Z(NN, NN)
    2 CONTINUE
    !
    !---- back substitute everything
    DO 3 NP = NN - 1, 1, -1
        NP1 = NP + 1
        DO 31 L = 1, NRHS
            DO 310 K = NP1, NN
                R(NP, L) = R(NP, L) - Z(NP, K) * R(K, L)
            310     CONTINUE
        31   CONTINUE
    3 CONTINUE
    !
    RETURN
END
! CGAUSS



SUBROUTINE LUDCMP(NSIZ, N, A, INDX)
    !     *******************************************************
    !     *                                                     *
    !     *   Factors a full NxN matrix into an LU form.        *
    !     *   Subr. BAKSUB can back-substitute it with some RHS.*
    !     *   Assumes matrix is non-singular...                 *
    !     *    ...if it isn't, a divide by zero will result.    *
    !     *                                                     *
    !     *   A is the matrix...                                *
    !     *     ...replaced with its LU factors.                *
    !     *                                                     *
    !     *                              Mark Drela  1988       *
    !     *******************************************************
    !
    DIMENSION A(NSIZ, NSIZ), INDX(NSIZ)
    !
    PARAMETER (NVX = 500)
    DIMENSION VV(NVX)
    !
    IF(N > NVX) STOP 'LUDCMP: Array overflow. Increase NVX.'
    !
    DO 12 I = 1, N
        AAMAX = 0.
        DO 11 J = 1, N
            AAMAX = MAX(ABS(A(I, J)), AAMAX)
        11   CONTINUE
        VV(I) = 1.0 / AAMAX
    12 CONTINUE
    !
    DO 19 J = 1, N
        DO 14 I = 1, J - 1
            SUM = A(I, J)
            DO 13 K = 1, I - 1
                SUM = SUM - A(I, K) * A(K, J)
            13     CONTINUE
            A(I, J) = SUM
        14   CONTINUE
        !
        AAMAX = 0.
        DO 16 I = J, N
            SUM = A(I, J)
            DO 15 K = 1, J - 1
                SUM = SUM - A(I, K) * A(K, J)
            15     CONTINUE
            A(I, J) = SUM
            !
            DUM = VV(I) * ABS(SUM)
            IF(DUM >= AAMAX) THEN
                IMAX = I
                AAMAX = DUM
            ENDIF
        16   CONTINUE
        !
        IF(J /= IMAX) THEN
            DO 17 K = 1, N
                DUM = A(IMAX, K)
                A(IMAX, K) = A(J, K)
                A(J, K) = DUM
            17    CONTINUE
            VV(IMAX) = VV(J)
        ENDIF
        !
        INDX(J) = IMAX
        IF(J /= N) THEN
            DUM = 1.0 / A(J, J)
            DO 18 I = J + 1, N
                A(I, J) = A(I, J) * DUM
            18    CONTINUE
        ENDIF
        !
    19 CONTINUE
    !
    RETURN
END
! LUDCMP


SUBROUTINE BAKSUB(NSIZ, N, A, INDX, B)
    DIMENSION A(NSIZ, NSIZ), B(NSIZ), INDX(NSIZ)
    !
    II = 0
    DO 12 I = 1, N
        LL = INDX(I)
        SUM = B(LL)
        B(LL) = B(I)
        IF(II /= 0) THEN
            DO 11 J = II, I - 1
                SUM = SUM - A(I, J) * B(J)
            11    CONTINUE
        ELSE IF(SUM /= 0.0) THEN
            II = I
        ENDIF
        B(I) = SUM
    12 CONTINUE
    !
    DO 14 I = N, 1, -1
        SUM = B(I)
        IF(I < N) THEN
            DO 13 J = I + 1, N
                SUM = SUM - A(I, J) * B(J)
            13    CONTINUE
        ENDIF
        B(I) = SUM / A(I, I)
    14 CONTINUE
    !
    RETURN
END
! BAKSUB



SUBROUTINE BLSOLV
    !-----------------------------------------------------------------
    !      Custom solver for coupled viscous-inviscid Newton system:
    !
    !        A  |  |  .  |  |  .  |    d       R       S
    !        B  A  |  .  |  |  .  |    d       R       S
    !        |  B  A  .  |  |  .  |    d       R       S
    !        .  .  .  .  |  |  .  |    d   =   R - dRe S
    !        |  |  |  B  A  |  .  |    d       R       S
    !        |  Z  |  |  B  A  .  |    d       R       S
    !        .  .  .  .  .  .  .  |    d       R       S
    !        |  |  |  |  |  |  B  A    d       R       S
    !
    !       A, B, Z  3x3  blocks containing linearized BL equation coefficients
    !       |        3x1  vectors containing mass defect influence 
    !                     coefficients on Ue
    !       d        3x1  unknown vectors (Newton deltas for Ctau, Theta, m)
    !       R        3x1  residual vectors
    !       S        3x1  Re influence vectors
    !-----------------------------------------------------------------
    INCLUDE 'XFOIL.INC'
    !
    IVTE1 = ISYS(IBLTE(1), 1)
    !
    VACC1 = VACCEL
    VACC2 = VACCEL * 2.0 / (S(N) - S(1))
    VACC3 = VACCEL * 2.0 / (S(N) - S(1))
    !
    DO 1000 IV = 1, NSYS
        !
        IVP = IV + 1
        !
        !====== Invert VA(IV) block
        !
        !------ normalize first row
        PIVOT = 1.0 / VA(1, 1, IV)
        VA(1, 2, IV) = VA(1, 2, IV) * PIVOT
        DO 10 L = IV, NSYS
            VM(1, L, IV) = VM(1, L, IV) * PIVOT
        10   CONTINUE
        VDEL(1, 1, IV) = VDEL(1, 1, IV) * PIVOT
        VDEL(1, 2, IV) = VDEL(1, 2, IV) * PIVOT
        !
        !------ eliminate lower first column in VA block
        DO 15 K = 2, 3
            VTMP = VA(K, 1, IV)
            VA(K, 2, IV) = VA(K, 2, IV) - VTMP * VA(1, 2, IV)
            DO 150 L = IV, NSYS
                VM(K, L, IV) = VM(K, L, IV) - VTMP * VM(1, L, IV)
            150     CONTINUE
            VDEL(K, 1, IV) = VDEL(K, 1, IV) - VTMP * VDEL(1, 1, IV)
            VDEL(K, 2, IV) = VDEL(K, 2, IV) - VTMP * VDEL(1, 2, IV)
        15   CONTINUE
        !
        !
        !------ normalize second row
        PIVOT = 1.0 / VA(2, 2, IV)
        DO 20 L = IV, NSYS
            VM(2, L, IV) = VM(2, L, IV) * PIVOT
        20   CONTINUE
        VDEL(2, 1, IV) = VDEL(2, 1, IV) * PIVOT
        VDEL(2, 2, IV) = VDEL(2, 2, IV) * PIVOT
        !
        !------ eliminate lower second column in VA block
        K = 3
        VTMP = VA(K, 2, IV)
        DO 250 L = IV, NSYS
            VM(K, L, IV) = VM(K, L, IV) - VTMP * VM(2, L, IV)
        250   CONTINUE
        VDEL(K, 1, IV) = VDEL(K, 1, IV) - VTMP * VDEL(2, 1, IV)
        VDEL(K, 2, IV) = VDEL(K, 2, IV) - VTMP * VDEL(2, 2, IV)
        !
        !
        !------ normalize third row
        PIVOT = 1.0 / VM(3, IV, IV)
        DO 350 L = IVP, NSYS
            VM(3, L, IV) = VM(3, L, IV) * PIVOT
        350   CONTINUE
        VDEL(3, 1, IV) = VDEL(3, 1, IV) * PIVOT
        VDEL(3, 2, IV) = VDEL(3, 2, IV) * PIVOT
        !
        !
        !------ eliminate upper third column in VA block
        VTMP1 = VM(1, IV, IV)
        VTMP2 = VM(2, IV, IV)
        DO 450 L = IVP, NSYS
            VM(1, L, IV) = VM(1, L, IV) - VTMP1 * VM(3, L, IV)
            VM(2, L, IV) = VM(2, L, IV) - VTMP2 * VM(3, L, IV)
        450   CONTINUE
        VDEL(1, 1, IV) = VDEL(1, 1, IV) - VTMP1 * VDEL(3, 1, IV)
        VDEL(2, 1, IV) = VDEL(2, 1, IV) - VTMP2 * VDEL(3, 1, IV)
        VDEL(1, 2, IV) = VDEL(1, 2, IV) - VTMP1 * VDEL(3, 2, IV)
        VDEL(2, 2, IV) = VDEL(2, 2, IV) - VTMP2 * VDEL(3, 2, IV)
        !
        !------ eliminate upper second column in VA block
        VTMP = VA(1, 2, IV)
        DO 460 L = IVP, NSYS
            VM(1, L, IV) = VM(1, L, IV) - VTMP * VM(2, L, IV)
        460   CONTINUE
        VDEL(1, 1, IV) = VDEL(1, 1, IV) - VTMP * VDEL(2, 1, IV)
        VDEL(1, 2, IV) = VDEL(1, 2, IV) - VTMP * VDEL(2, 2, IV)
        !
        !
        IF(IV == NSYS) GO TO 1000
        !
        !====== Eliminate VB(IV+1) block, rows  1 -> 3
        DO 50 K = 1, 3
            VTMP1 = VB(K, 1, IVP)
            VTMP2 = VB(K, 2, IVP)
            VTMP3 = VM(K, IV, IVP)
            DO 510 L = IVP, NSYS
                VM(K, L, IVP) = VM(K, L, IVP)&
                        - (VTMP1 * VM(1, L, IV)&
                                + VTMP2 * VM(2, L, IV)&
                                + VTMP3 * VM(3, L, IV))
            510     CONTINUE
            VDEL(K, 1, IVP) = VDEL(K, 1, IVP)&
                    - (VTMP1 * VDEL(1, 1, IV)&
                            + VTMP2 * VDEL(2, 1, IV)&
                            + VTMP3 * VDEL(3, 1, IV))
            VDEL(K, 2, IVP) = VDEL(K, 2, IVP)&
                    - (VTMP1 * VDEL(1, 2, IV)&
                            + VTMP2 * VDEL(2, 2, IV)&
                            + VTMP3 * VDEL(3, 2, IV))
        50   CONTINUE
        !
        IF(IV == IVTE1) THEN
            !------- eliminate VZ block
            IVZ = ISYS(IBLTE(2) + 1, 2)
            !
            DO 55 K = 1, 3
                VTMP1 = VZ(K, 1)
                VTMP2 = VZ(K, 2)
                DO 515 L = IVP, NSYS
                    VM(K, L, IVZ) = VM(K, L, IVZ)&
                            - (VTMP1 * VM(1, L, IV)&
                                    + VTMP2 * VM(2, L, IV))
                515      CONTINUE
                VDEL(K, 1, IVZ) = VDEL(K, 1, IVZ)&
                        - (VTMP1 * VDEL(1, 1, IV)&
                                + VTMP2 * VDEL(2, 1, IV))
                VDEL(K, 2, IVZ) = VDEL(K, 2, IVZ)&
                        - (VTMP1 * VDEL(1, 2, IV)&
                                + VTMP2 * VDEL(2, 2, IV))
            55    CONTINUE
        ENDIF
        !
        IF(IVP == NSYS) GO TO 1000
        !
        !====== Eliminate lower VM column
        DO 60 KV = IV + 2, NSYS
            VTMP1 = VM(1, IV, KV)
            VTMP2 = VM(2, IV, KV)
            VTMP3 = VM(3, IV, KV)
            !
            IF(ABS(VTMP1) > VACC1) THEN
                DO 610 L = IVP, NSYS
                    VM(1, L, KV) = VM(1, L, KV) - VTMP1 * VM(3, L, IV)
                610     CONTINUE
                VDEL(1, 1, KV) = VDEL(1, 1, KV) - VTMP1 * VDEL(3, 1, IV)
                VDEL(1, 2, KV) = VDEL(1, 2, KV) - VTMP1 * VDEL(3, 2, IV)
            ENDIF
            !
            IF(ABS(VTMP2) > VACC2) THEN
                DO 620 L = IVP, NSYS
                    VM(2, L, KV) = VM(2, L, KV) - VTMP2 * VM(3, L, IV)
                620     CONTINUE
                VDEL(2, 1, KV) = VDEL(2, 1, KV) - VTMP2 * VDEL(3, 1, IV)
                VDEL(2, 2, KV) = VDEL(2, 2, KV) - VTMP2 * VDEL(3, 2, IV)
            ENDIF
            !
            IF(ABS(VTMP3) > VACC3) THEN
                DO 630 L = IVP, NSYS
                    VM(3, L, KV) = VM(3, L, KV) - VTMP3 * VM(3, L, IV)
                630     CONTINUE
                VDEL(3, 1, KV) = VDEL(3, 1, KV) - VTMP3 * VDEL(3, 1, IV)
                VDEL(3, 2, KV) = VDEL(3, 2, KV) - VTMP3 * VDEL(3, 2, IV)
            ENDIF
            !
        60   CONTINUE
        !
    1000 CONTINUE
    !
    !
    !
    DO 2000 IV = NSYS, 2, -1
        !
        !------ eliminate upper VM columns
        VTMP = VDEL(3, 1, IV)
        DO 81 KV = IV - 1, 1, -1
            VDEL(1, 1, KV) = VDEL(1, 1, KV) - VM(1, IV, KV) * VTMP
            VDEL(2, 1, KV) = VDEL(2, 1, KV) - VM(2, IV, KV) * VTMP
            VDEL(3, 1, KV) = VDEL(3, 1, KV) - VM(3, IV, KV) * VTMP
        81   CONTINUE
        !
        VTMP = VDEL(3, 2, IV)
        DO 82 KV = IV - 1, 1, -1
            VDEL(1, 2, KV) = VDEL(1, 2, KV) - VM(1, IV, KV) * VTMP
            VDEL(2, 2, KV) = VDEL(2, 2, KV) - VM(2, IV, KV) * VTMP
            VDEL(3, 2, KV) = VDEL(3, 2, KV) - VM(3, IV, KV) * VTMP
        82   CONTINUE
        !
    2000 CONTINUE
    !
    RETURN
END