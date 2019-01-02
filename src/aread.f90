SUBROUTINE AREAD(LU, FNAME, NMAX, X, Y, N, NAME, ISPARS, ITYPE, INFO)
    DIMENSION X(NMAX), Y(NMAX)
    CHARACTER*(*) FNAME
    CHARACTER*(*) NAME
    CHARACTER*(*) ISPARS
    !--------------------------------------------------------
    !     Reads in several types of airfoil coordinate file.
    !
    !  Input:
    !       LU      logical unit to use for reading
    !       FNAME   name of coordinate file to be read,
    !               if FNAME(1:1) == ' ', unit LU is assumed
    !               to be already open
    !       INFO   0 keep quiet
    !              1 print info on airfoil
    !  Output:
    !       X,Y     coordinates
    !       N       number of X,Y coordinates
    !       NAME    character name string        (if ITYPE > 1)
    !       ISPARS  ISES/MSES domain-size string (if ITYPE > 2)
    !       ITYPE returns type of file:
    !           0  None.  Read error occurred.
    !           1  Generic.
    !           2  Labeled generic.
    !           3  MSES single element.
    !           4  MSES multi-element.
    !--------------------------------------------------------
    CHARACTER*80 LINE1, LINE2, LINE
    LOGICAL LOPEN, ERROR
    DIMENSION A(10)
    !
    IEL = 0
    NEL = 0
    !
    !---- assume read error will occur
    ITYPE = 0
    !
    LOPEN = FNAME(1:1) /= ' '
    IF(LOPEN) OPEN(LU, FILE = FNAME, STATUS = 'OLD', ERR = 98)
    !
    11   READ(LU, 1000, END = 99, ERR = 98) LINE1
    IF(INDEX('#!', LINE1(1:1)) /= 0) GO TO 11
    !
    12   READ(LU, 1000, END = 99) LINE2
    IF(INDEX('#!', LINE2(1:1)) /= 0) GO TO 12
    !
    I = 1
    !
    !---- try to read two numbers from first line
    NA = 2
    CALL GETFLT(LINE1, A, NA, ERROR)
    IF(ERROR .OR. NA < 2) THEN
        !------ must be a name string
        NAME = LINE1
    ELSE
        !------ no name, just two valid numbers... must be plain airfoil file
        NAME = ' '
        IF(INFO > 0) THEN
            WRITE(*, *)
            WRITE(*, *) 'Plain airfoil file'
        ENDIF
        ITYPE = 1
        REWIND(LU)
        GO TO 50
    ENDIF
    !
    !---- if we got here, there's a name line,
    !-    so now try to read four MSES domain numbers from second line
    NA = 4
    CALL GETFLT(LINE2, A, NA, ERROR)
    IF(ERROR .OR. NA < 2) THEN
        !------ less than two valid numbers... not a valid format
        GO TO 99
        !
    ELSEIF(NA < 4) THEN
        !------ less than four numbers... usual .dat labeled file
        NAME = LINE1
        IF(INFO > 0) THEN
            WRITE(*, *)
            WRITE(*, *) 'Labeled airfoil file.  Name:  ', NAME
        ENDIF
        ITYPE = 2
        REWIND(LU)
        READ(LU, 1000, END = 99) LINE1
        GO TO 50
        !
    ELSE
        !------ four or more numbers... MSES or MISES file
        IF(INFO > 0) THEN
            WRITE(*, *)
            WRITE(*, *) 'MSES airfoil file.  Name:  ', NAME
        ENDIF
        ITYPE = 3
        ISPARS = LINE2
    ENDIF
    !
    !---- read each element until 999.0 or end of file is encountered
    50 NEL = NEL + 1
    DO 55 I = 1, NMAX
        51     READ(LU, 1000, END = 60) LINE
        !
        !------ skip comment line
        IF(INDEX('#!', LINE(1:1)) /= 0) GO TO 51
        !
        NA = 2
        CALL GETFLT(LINE, A, NA, ERROR)
        IF(ERROR) GO TO 99
        !
        !------ skip line without at least two numbers
        IF(NA < 2) GO TO 51
        !
        X(I) = A(1)
        Y(I) = A(2)
        !
        IF (X(I) == 999.0 .AND. Y(I) == 999.0) THEN
            !-------- if this is the element we want, just exit
            IF(IEL == NEL) GO TO 60
            !
            IF(IEL == 0) THEN
                CALL ASKI('Enter element number^', IEL)
                ITYPE = 4
            ENDIF
            !
            !-------- if this is the specified element, exit.
            IF(IEL == NEL) GO TO 60
            GO TO 50
        ENDIF
    55 CONTINUE
    WRITE(*, 5030) NMAX
    WRITE(*, 5900)
    IF(LOPEN) CLOSE(LU)
    ITYPE = 0
    RETURN
    !
    60 N = I - 1
    IF(LOPEN) CLOSE(LU)
    RETURN
    !
    98 CONTINUE
    NFN = INDEX(FNAME, ' ') + 1
    WRITE(*, 5050) FNAME(1:NFN)
    WRITE(*, 5900)
    ITYPE = 0
    RETURN
    !
    99 CONTINUE
    IF(LOPEN) CLOSE(LU)
    WRITE(*, 5100)
    WRITE(*, 5900)
    ITYPE = 0
    RETURN
    !...............................................................
    1000 FORMAT(A)
    5030 FORMAT(/' Buffer array size exceeded'&
            /' Maximum number of points: ', I4)
    5050 FORMAT(/' File OPEN error.  Nonexistent file:  ', A)
    5100 FORMAT(/' File READ error.  Unrecognizable file format')
    5900 FORMAT(' *** LOAD NOT COMPLETED ***')
END
! AREAD