module s_xbl
contains
    ! iblsys was originally in xbl between setbl and mrchue.
    ! It was moved here to avoid a circular use dependency.
    subroutine iblsys
        use i_xfoil
        use i_xbl
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        integer :: ibl, is, iv
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
        !---------------------------------------------
        !     Sets the BL Newton system line number
        !     corresponding to each BL station.
        !---------------------------------------------
        iv = 0
        do is = 1, 2
            do ibl = 2, NBL(is)
                iv = iv + 1
                ISYs(ibl, is) = iv
            enddo
        enddo
        !
        NSYs = iv
        if (NSYs>2 * IVX) stop '*** IBLSYS: BL system array overflow. ***'
        !
    end subroutine iblsys
    !*==MRCHUE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
end module s_xbl