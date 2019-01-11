module s_xfoil
contains
    subroutine mrcl(Cls, M_cls, R_cls)
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Dummy arguments
        !
        real :: Cls, M_cls, R_cls
        intent (in) Cls
        intent (out) M_cls, R_cls
        !
        ! Local variables
        !
        real :: cla, rrat
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
        !-------------------------------------------
        !     Sets actual Mach, Reynolds numbers
        !     from unit-CL values and specified CLS
        !     depending on MATYP,RETYP flags.
        !-------------------------------------------
        !
        cla = max(Cls, 0.000001)
        !
        if (RETyp<1 .or. RETyp>3) then
            write (*, *) 'MRCL:  Illegal Re(CL) dependence trigger.'
            write (*, *) '       Setting fixed Re.'
            RETyp = 1
        endif
        if (MATyp<1 .or. MATyp>3) then
            write (*, *) 'MRCL:  Illegal Mach(CL) dependence trigger.'
            write (*, *) '       Setting fixed Mach.'
            MATyp = 1
        endif
        !
        !
        if (MATyp==1) then
            !
            MINf = MINf1
            M_cls = 0.
            !
        elseif (MATyp==2) then
            !
            MINf = MINf1 / sqrt(cla)
            M_cls = -0.5 * MINf / cla
            !
        elseif (MATyp==3) then
            !
            MINf = MINf1
            M_cls = 0.
            !
        endif
        !
        !
        if (RETyp==1) then
            !
            REInf = REInf1
            R_cls = 0.
            !
        elseif (RETyp==2) then
            !
            REInf = REInf1 / sqrt(cla)
            R_cls = -0.5 * REInf / cla
            !
        elseif (RETyp==3) then
            !
            REInf = REInf1 / cla
            R_cls = -REInf / cla
            !
        endif
        !
        !
        if (MINf>=0.99) then
            write (*, *)
            write (*, *) 'MRCL: CL too low for chosen Mach(CL) dependence'
            write (*, *) '      Aritificially limiting Mach to  0.99'
            MINf = 0.99
            M_cls = 0.
        endif
        !
        rrat = 1.0
        if (REInf1>0.0) rrat = REInf / REInf1
        !
        if (rrat>100.0) then
            write (*, *)
            write (*, *) 'MRCL: CL too low for chosen Re(CL) dependence'
            write (*, *) '      Aritificially limiting Re to ', REInf1 * 100.0
            REInf = REInf1 * 100.0
            R_cls = 0.
        endif
        !
    end subroutine mrcl
    !*==GETDEF.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! MRCL


    subroutine comset
        use i_xfoil
        implicit none
        !
        !*** Start of declarations rewritten by SPAG
        !
        ! Local variables
        !
        real :: beta, beta_msq
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
        !---- set Karman-Tsien parameter TKLAM
        beta = sqrt(1.0 - MINf**2)
        beta_msq = -0.5 / beta
        !
        TKLam = MINf**2 / (1.0 + beta)**2
        TKL_msq = 1.0 / (1.0 + beta)**2 - 2.0 * TKLam / (1.0 + beta) * beta_msq
        !
        !---- set sonic Pressure coefficient and speed
        if (MINf==0.0) then
            CPStar = -999.0
            QSTar = 999.0
        else
            CPStar = 2.0 / (GAMma * MINf**2) &
                    * (((1.0 + 0.5 * GAMm1 * MINf**2) / (1.0 + 0.5 * GAMm1))**(GAMma / GAMm1) - 1.0)
            QSTar = QINf / MINf * sqrt((1.0 + 0.5 * GAMm1 * MINf**2) / (1.0 + 0.5 * GAMm1))
        endif
        !
    end subroutine comset
    !*==CPCALC.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
    ! COMSET

end module s_xfoil