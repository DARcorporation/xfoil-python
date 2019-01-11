module i_iopol
    include 'PINDEX.INC'
    CHARACTER*10 CPOLNAME(IPTOT)
    CHARACTER*5 CPOLSNAME(JPTOT)
    CHARACTER*6 CPOLFORM(IPTOT), CPOLSFORM(JPTOT)
    !
    DATA CPOLNAME  /&
            'alpha     ', &
            'CL        ', &
            'CD        ', &
            'CM        ', &
            'CDw       ', &
            'CDv       ', &
            'CDp       ', &
            'Mach      ', &
            'Re        ', &
            'Chinge    ', &
            'Cpmin     ', &
            'CDh       ', &
            'Cmdot     '  /
    DATA CPOLFORM  /&
            'F7.3  ', &     !    alpha
            'F9.4  ', &     !     CL
            'F10.5 ', &     !     CD
            'F9.4  ', &     !     CM
            'F10.5 ', &     !     CDw
            'F10.5 ', &     !     CDv
            'F10.5 ', &     !     CDp
            'F8.4  ', &     !     Mach
            'E11.3 ', &     !     Re
            'F10.5 ', &     !    Chinge
            'F9.4  ', &     !    Cpmin
            'F11.5 ', &     !     CDh
            'F9.5  ' /      !     Cmdot

    DATA CPOLSNAME /&
            'Ncrit', &
            'Xtrip', &
            'Xtr  ', &
            'Itr  ' /
    DATA CPOLSFORM  /&
            'F7.3  ', &     !    Ncrit
            'F9.4  ', &     !    Xtrip
            'F9.4  ', &     !    Xtr
            'F9.4  ' /      !    Itr
end module i_iopol