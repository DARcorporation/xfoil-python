!*==I_PINDEX.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
! AREAD
module i_pindex
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! PARAMETER definitions
    !
    integer, parameter :: IAL = 1, ICL = 2, ICD = 3, ICM = 4, ICW = 5, ICV = 6, ICP = 7, IMA = 8, IRE = 9, &
            & ICH = 10, IMC = 11, ICDH = 12, ICMDOT = 13, IPTOT = 13, JNC = 1, JTP = 2, JTN = 3, &
            & JTI = 4, JPTOT = 4
    !
    ! Local variables
    !
    character(6), dimension(IPTOT), save :: cpolform
    character(10), dimension(IPTOT), save :: cpolname
    character(6), dimension(JPTOT), save :: cpolsform
    character(5), dimension(JPTOT), save :: cpolsname
    !
    !*** End of declarations rewritten by SPAG
    !
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! PARAMETER definitions
    !
    !
    ! Local variables
    !
    !
    !*** End of declarations rewritten by SPAG
    !
    !
    !---- Pointers for referencing polar force coefficients
    !     First 4 pointers must be main polar plot variables.
    !
    !      ! alpha
    !      ! CL
    !      ! CD
    !      ! Cm
    !      ! CDwave
    !      ! CDvisc
    !      ! CDpres
    !      ! Mach
    !      ! Re
    !     ! Hinge moment
    !     ! Minimum Cp on surface
    !    ! CDh  (engine thrust coeff.)
    !
    !
    !---------------------
    !  Pointers for referencing polar airfoil-side quantities
    !
    ! Cm_dot
    !      ! Ncrit
    !      ! trip
    !      ! transition

    !
    data cpolname/'alpha     ', 'CL        ', 'CD        ', 'CM        ', 'CDw       ', 'CDv       ', 'CDp       ', &
            &'Mach      ', 'Re        ', 'Chinge    ', 'Cpmin     ', 'CDh       ', 'Cmdot     '/
    ! transition index
    !     !    alpha
    !     !     CL
    !     !     CD
    !     !     CM
    !     !     CDw
    !     !     CDv
    !     !     CDp
    !     !     Mach
    !     !     Re
    !     !    Chinge
    !     !    Cpmin
    !     !     CDh
    data cpolform/'F7.3  ', 'F9.4  ', 'F10.5 ', 'F9.4  ', 'F10.5 ', 'F10.5 ', 'F10.5 ', 'F8.4  ', 'E11.3 ', &
            &'F10.5 ', 'F9.4  ', 'F11.5 ', 'F9.5  '/
    !     Cmdot

    data cpolsname/'Ncrit', 'Xtrip', 'Xtr  ', 'Itr  '/
    !     !    Ncrit
    !     !    Xtrip
    !     !    Xtr
    data cpolsform/'F7.3  ', 'F9.4  ', 'F9.4  ', 'F9.4  '/
    !    Itr
end module i_pindex
