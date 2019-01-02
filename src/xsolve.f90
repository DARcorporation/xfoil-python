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
    DO NP = 1, NN - 1
        NP1 = NP + 1
        !
        !------ find max pivot index NX
        NX = NP
        DO N = NP1, NN
            IF(ABS(Z(N, NP)) - ABS(Z(NX, NP))) 11, 11, 111
            111      NX = N
        11   CONTINUE
        end do
        !
        PIVOT = 1.0 / Z(NX, NP)
        !
        !------ switch pivots
        Z(NX, NP) = Z(NP, NP)
        !
        !------ switch rows & normalize pivot row
        DO L = NP1, NN
            TEMP = Z(NX, L) * PIVOT
            Z(NX, L) = Z(NP, L)
            Z(NP, L) = TEMP
        end do
        !
        DO L = 1, NRHS
            TEMP = R(NX, L) * PIVOT
            R(NX, L) = R(NP, L)
            R(NP, L) = TEMP
        end do
        !
        !------ forward eliminate everything
        DO K = NP1, NN
            ZTMP = Z(K, NP)
            !
            !          IF(ZTMP == 0.0) GO TO 15
            !
            DO L = NP1, NN
                Z(K, L) = Z(K, L) - ZTMP * Z(NP, L)
            end do
            DO L = 1, NRHS
                R(K, L) = R(K, L) - ZTMP * R(NP, L)
            end do
        end do
        !
    end do
    !
    !---- solve for last row
    DO L = 1, NRHS
        R(NN, L) = R(NN, L) / Z(NN, NN)
    end do
    !
    !---- back substitute everything
    DO NP = NN - 1, 1, -1
        NP1 = NP + 1
        DO L = 1, NRHS
            DO K = NP1, NN
                R(NP, L) = R(NP, L) - Z(NP, K) * R(K, L)
            end do
        end do
    end do
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
    DO NP = 1, NN - 1
        NP1 = NP + 1
        !
        !------ find max pivot index NX
        NX = NP
        DO N = NP1, NN
            IF(ABS(Z(N, NP)) - ABS(Z(NX, NP))) 11, 11, 111
            111      NX = N
        11   CONTINUE
        end do
        !
        PIVOT = (1.0, 0.0) / Z(NX, NP)
        !
        !------ switch pivots
        Z(NX, NP) = Z(NP, NP)
        !
        !------ switch rows & normalize pivot row
        DO L = NP1, NN
            TEMP = Z(NX, L) * PIVOT
            Z(NX, L) = Z(NP, L)
            Z(NP, L) = TEMP
        end do
        !
        DO L = 1, NRHS
            TEMP = R(NX, L) * PIVOT
            R(NX, L) = R(NP, L)
            R(NP, L) = TEMP
        end do
        !
        !------ forward eliminate everything
        DO K = NP1, NN
            ZTMP = Z(K, NP)
            !
            !          IF(ZTMP == 0.0) GO TO 15
            !
            DO L = NP1, NN
                Z(K, L) = Z(K, L) - ZTMP * Z(NP, L)
            end do
            DO L = 1, NRHS
                R(K, L) = R(K, L) - ZTMP * R(NP, L)
            end do
        end do
        !
    end do
    !
    !---- solve for last row
    DO L = 1, NRHS
        R(NN, L) = R(NN, L) / Z(NN, NN)
    end do
    !
    !---- back substitute everything
    DO NP = NN - 1, 1, -1
        NP1 = NP + 1
        DO L = 1, NRHS
            DO K = NP1, NN
                R(NP, L) = R(NP, L) - Z(NP, K) * R(K, L)
            end do
        end do
    end do
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
    DO I = 1, N
        AAMAX = 0.
        DO J = 1, N
            AAMAX = MAX(ABS(A(I, J)), AAMAX)
        end do
        VV(I) = 1.0 / AAMAX
    end do
    !
    DO J = 1, N
        DO I = 1, J - 1
            SUM = A(I, J)
            DO K = 1, I - 1
                SUM = SUM - A(I, K) * A(K, J)
            end do
            A(I, J) = SUM
        end do
        !
        AAMAX = 0.
        DO I = J, N
            SUM = A(I, J)
            DO K = 1, J - 1
                SUM = SUM - A(I, K) * A(K, J)
            end do
            A(I, J) = SUM
            !
            DUM = VV(I) * ABS(SUM)
            IF(DUM >= AAMAX) THEN
                IMAX = I
                AAMAX = DUM
            ENDIF
        end do
        !
        IF(J /= IMAX) THEN
            DO K = 1, N
                DUM = A(IMAX, K)
                A(IMAX, K) = A(J, K)
                A(J, K) = DUM
            end do
            VV(IMAX) = VV(J)
        ENDIF
        !
        INDX(J) = IMAX
        IF(J /= N) THEN
            DUM = 1.0 / A(J, J)
            DO I = J + 1, N
                A(I, J) = A(I, J) * DUM
            end do
        ENDIF
        !
    end do
    !
    RETURN
END
! LUDCMP


SUBROUTINE BAKSUB(NSIZ, N, A, INDX, B)
    DIMENSION A(NSIZ, NSIZ), B(NSIZ), INDX(NSIZ)
    !
    II = 0
    DO I = 1, N
        LL = INDX(I)
        SUM = B(LL)
        B(LL) = B(I)
        IF(II /= 0) THEN
            DO J = II, I - 1
                SUM = SUM - A(I, J) * B(J)
            end do
        ELSE IF(SUM /= 0.0) THEN
            II = I
        ENDIF
        B(I) = SUM
    end do
    !
    DO I = N, 1, -1
        SUM = B(I)
        IF(I < N) THEN
            DO J = I + 1, N
                SUM = SUM - A(I, J) * B(J)
            end do
        ENDIF
        B(I) = SUM / A(I, I)
    end do
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
    DO IV = 1, NSYS
        !
        IVP = IV + 1
        !
        !====== Invert VA(IV) block
        !
        !------ normalize first row
        PIVOT = 1.0 / VA(1, 1, IV)
        VA(1, 2, IV) = VA(1, 2, IV) * PIVOT
        DO L = IV, NSYS
            VM(1, L, IV) = VM(1, L, IV) * PIVOT
        end do
        VDEL(1, 1, IV) = VDEL(1, 1, IV) * PIVOT
        VDEL(1, 2, IV) = VDEL(1, 2, IV) * PIVOT
        !
        !------ eliminate lower first column in VA block
        DO K = 2, 3
            VTMP = VA(K, 1, IV)
            VA(K, 2, IV) = VA(K, 2, IV) - VTMP * VA(1, 2, IV)
            DO L = IV, NSYS
                VM(K, L, IV) = VM(K, L, IV) - VTMP * VM(1, L, IV)
            end do
            VDEL(K, 1, IV) = VDEL(K, 1, IV) - VTMP * VDEL(1, 1, IV)
            VDEL(K, 2, IV) = VDEL(K, 2, IV) - VTMP * VDEL(1, 2, IV)
        end do
        !
        !
        !------ normalize second row
        PIVOT = 1.0 / VA(2, 2, IV)
        DO L = IV, NSYS
            VM(2, L, IV) = VM(2, L, IV) * PIVOT
        end do
        VDEL(2, 1, IV) = VDEL(2, 1, IV) * PIVOT
        VDEL(2, 2, IV) = VDEL(2, 2, IV) * PIVOT
        !
        !------ eliminate lower second column in VA block
        K = 3
        VTMP = VA(K, 2, IV)
        DO L = IV, NSYS
            VM(K, L, IV) = VM(K, L, IV) - VTMP * VM(2, L, IV)
        end do
        VDEL(K, 1, IV) = VDEL(K, 1, IV) - VTMP * VDEL(2, 1, IV)
        VDEL(K, 2, IV) = VDEL(K, 2, IV) - VTMP * VDEL(2, 2, IV)
        !
        !
        !------ normalize third row
        PIVOT = 1.0 / VM(3, IV, IV)
        DO L = IVP, NSYS
            VM(3, L, IV) = VM(3, L, IV) * PIVOT
        end do
        VDEL(3, 1, IV) = VDEL(3, 1, IV) * PIVOT
        VDEL(3, 2, IV) = VDEL(3, 2, IV) * PIVOT
        !
        !
        !------ eliminate upper third column in VA block
        VTMP1 = VM(1, IV, IV)
        VTMP2 = VM(2, IV, IV)
        DO L = IVP, NSYS
            VM(1, L, IV) = VM(1, L, IV) - VTMP1 * VM(3, L, IV)
            VM(2, L, IV) = VM(2, L, IV) - VTMP2 * VM(3, L, IV)
        end do
        VDEL(1, 1, IV) = VDEL(1, 1, IV) - VTMP1 * VDEL(3, 1, IV)
        VDEL(2, 1, IV) = VDEL(2, 1, IV) - VTMP2 * VDEL(3, 1, IV)
        VDEL(1, 2, IV) = VDEL(1, 2, IV) - VTMP1 * VDEL(3, 2, IV)
        VDEL(2, 2, IV) = VDEL(2, 2, IV) - VTMP2 * VDEL(3, 2, IV)
        !
        !------ eliminate upper second column in VA block
        VTMP = VA(1, 2, IV)
        DO L = IVP, NSYS
            VM(1, L, IV) = VM(1, L, IV) - VTMP * VM(2, L, IV)
        end do
        VDEL(1, 1, IV) = VDEL(1, 1, IV) - VTMP * VDEL(2, 1, IV)
        VDEL(1, 2, IV) = VDEL(1, 2, IV) - VTMP * VDEL(2, 2, IV)
        !
        !
        IF(IV == NSYS) GO TO 1000
        !
        !====== Eliminate VB(IV+1) block, rows  1 -> 3
        DO K = 1, 3
            VTMP1 = VB(K, 1, IVP)
            VTMP2 = VB(K, 2, IVP)
            VTMP3 = VM(K, IV, IVP)
            DO L = IVP, NSYS
                VM(K, L, IVP) = VM(K, L, IVP)&
                        - (VTMP1 * VM(1, L, IV)&
                                + VTMP2 * VM(2, L, IV)&
                                + VTMP3 * VM(3, L, IV))
            end do
            VDEL(K, 1, IVP) = VDEL(K, 1, IVP)&
                    - (VTMP1 * VDEL(1, 1, IV)&
                            + VTMP2 * VDEL(2, 1, IV)&
                            + VTMP3 * VDEL(3, 1, IV))
            VDEL(K, 2, IVP) = VDEL(K, 2, IVP)&
                    - (VTMP1 * VDEL(1, 2, IV)&
                            + VTMP2 * VDEL(2, 2, IV)&
                            + VTMP3 * VDEL(3, 2, IV))
        end do
        !
        IF(IV == IVTE1) THEN
            !------- eliminate VZ block
            IVZ = ISYS(IBLTE(2) + 1, 2)
            !
            DO K = 1, 3
                VTMP1 = VZ(K, 1)
                VTMP2 = VZ(K, 2)
                DO L = IVP, NSYS
                    VM(K, L, IVZ) = VM(K, L, IVZ)&
                            - (VTMP1 * VM(1, L, IV)&
                                    + VTMP2 * VM(2, L, IV))
                end do
                VDEL(K, 1, IVZ) = VDEL(K, 1, IVZ)&
                        - (VTMP1 * VDEL(1, 1, IV)&
                                + VTMP2 * VDEL(2, 1, IV))
                VDEL(K, 2, IVZ) = VDEL(K, 2, IVZ)&
                        - (VTMP1 * VDEL(1, 2, IV)&
                                + VTMP2 * VDEL(2, 2, IV))
            end do
        ENDIF
        !
        IF(IVP == NSYS) GO TO 1000
        !
        !====== Eliminate lower VM column
        DO KV = IV + 2, NSYS
            VTMP1 = VM(1, IV, KV)
            VTMP2 = VM(2, IV, KV)
            VTMP3 = VM(3, IV, KV)
            !
            IF(ABS(VTMP1) > VACC1) THEN
                DO L = IVP, NSYS
                    VM(1, L, KV) = VM(1, L, KV) - VTMP1 * VM(3, L, IV)
                end do
                VDEL(1, 1, KV) = VDEL(1, 1, KV) - VTMP1 * VDEL(3, 1, IV)
                VDEL(1, 2, KV) = VDEL(1, 2, KV) - VTMP1 * VDEL(3, 2, IV)
            ENDIF
            !
            IF(ABS(VTMP2) > VACC2) THEN
                DO L = IVP, NSYS
                    VM(2, L, KV) = VM(2, L, KV) - VTMP2 * VM(3, L, IV)
                end do
                VDEL(2, 1, KV) = VDEL(2, 1, KV) - VTMP2 * VDEL(3, 1, IV)
                VDEL(2, 2, KV) = VDEL(2, 2, KV) - VTMP2 * VDEL(3, 2, IV)
            ENDIF
            !
            IF(ABS(VTMP3) > VACC3) THEN
                DO L = IVP, NSYS
                    VM(3, L, KV) = VM(3, L, KV) - VTMP3 * VM(3, L, IV)
                end do
                VDEL(3, 1, KV) = VDEL(3, 1, KV) - VTMP3 * VDEL(3, 1, IV)
                VDEL(3, 2, KV) = VDEL(3, 2, KV) - VTMP3 * VDEL(3, 2, IV)
            ENDIF
            !
        end do
        !
    1000 CONTINUE
    end do
    !
    !
    !
    DO IV = NSYS, 2, -1
        !
        !------ eliminate upper VM columns
        VTMP = VDEL(3, 1, IV)
        DO KV = IV - 1, 1, -1
            VDEL(1, 1, KV) = VDEL(1, 1, KV) - VM(1, IV, KV) * VTMP
            VDEL(2, 1, KV) = VDEL(2, 1, KV) - VM(2, IV, KV) * VTMP
            VDEL(3, 1, KV) = VDEL(3, 1, KV) - VM(3, IV, KV) * VTMP
        end do
        !
        VTMP = VDEL(3, 2, IV)
        DO KV = IV - 1, 1, -1
            VDEL(1, 2, KV) = VDEL(1, 2, KV) - VM(1, IV, KV) * VTMP
            VDEL(2, 2, KV) = VDEL(2, 2, KV) - VM(2, IV, KV) * VTMP
            VDEL(3, 2, KV) = VDEL(3, 2, KV) - VM(3, IV, KV) * VTMP
        end do
        !
    end do
    !
    RETURN
END