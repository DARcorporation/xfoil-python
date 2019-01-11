!
!---- Pointers for referencing polar force coefficients
!     First 4 pointers must be main polar plot variables.
!
PARAMETER (&
        IAL = 1, &      ! alpha
        ICL = 2, &      ! CL
        ICD = 3, &      ! CD
        ICM = 4, &      ! Cm
        ICW = 5, &      ! CDwave
        ICV = 6, &      ! CDvisc
        ICP = 7, &      ! CDpres
        IMA = 8, &      ! Mach
        IRE = 9, &      ! Re
        ICH = 10, &     ! Hinge moment
        IMC = 11, &     ! Minimum Cp on surface
        ICDH = 12, &    ! CDh  (engine thrust coeff.)
        ICMDOT = 13)    ! Cm_dot
PARAMETER (IPTOT = 13)
!
!
!---------------------
!  Pointers for referencing polar airfoil-side quantities
!
PARAMETER (&
        JNC = 1, &      ! Ncrit
        JTP = 2, &      ! trip
        JTN = 3, &      ! transition
        JTI = 4)        ! transition index
PARAMETER (JPTOT = 4)