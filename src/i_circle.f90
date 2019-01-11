!*==I_CIRCLE.f90  processed by SPAG 7.21DC at 11:25 on 11 Jan 2019
module i_circle
    implicit none
    !
    !*** Start of declarations rewritten by SPAG
    !
    ! PARAMETER definitions
    !
    integer, parameter :: ICX = 257, IMX = (ICX - 1) / 4
    !
    ! Local variables
    !
    real :: ag0, agte, dwc, pi, qim0, qimold
    complex :: chordz, dzte, zleold
    complex, dimension(0:IMX) :: cn
    complex, dimension(ICX, 0:IMX) :: eiw
    integer :: mc, mct, nc
    complex, dimension(ICX) :: piq, zc, zcoldw
    real, dimension(ICX) :: sc, scold, wc, xcold, ycold
    complex, dimension(ICX, IMX / 4) :: zc_cn
    !
    !*** End of declarations rewritten by SPAG
    !
    !
    ! PARAMETER definitions
    !
    !
    ! COMMON /CPC01/
    !
    !
    ! COMMON /CPI01/
    !
    !
    ! COMMON /CPR01/
    !
end module i_circle
