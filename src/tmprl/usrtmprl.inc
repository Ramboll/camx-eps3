c*** USRTMPRL.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for user input parameters and variables for TMPRL
c    program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Day of week parameters:
c-----------------------------------------------------------------------
c
c   ***      C   standard 3 character abbreviations of the days of the
c                week
c
      character*3 MON
      character*3 TUE
      character*3 WED
      character*3 THU
      character*3 FRI
      character*3 SAT
      character*3 SUN
c
      parameter ( MON = 'MON' )
      parameter ( TUE = 'TUE' )
      parameter ( WED = 'WED' )
      parameter ( THU = 'THU' )
      parameter ( FRI = 'FRI' )
      parameter ( SAT = 'SAT' )
      parameter ( SUN = 'SUN' )
c
c   IDX***   C   offset of day *** in temporal profiles array
c
      integer*4   IDXMON
      integer*4   IDXTUE
      integer*4   IDXWED
      integer*4   IDXTHU
      integer*4   IDXFRI
      integer*4   IDXSAT
      integer*4   IDXSUN
c
      parameter ( IDXMON = 1 )
      parameter ( IDXTUE = 2 )
      parameter ( IDXWED = 3 )
      parameter ( IDXTHU = 4 )
      parameter ( IDXFRI = 5 )
      parameter ( IDXSAT = 6 )
      parameter ( IDXSUN = 7 )
c
c-----------------------------------------------------------------------
c    USERIN variables:
c-----------------------------------------------------------------------
c
c   idxday   I   offset of day of week in temporal profiles array
c                (see parameters above)
c   invtzn   I   inventory timezone
c
      integer*4 idxday
      integer*4 invtzn
c
      common /usrdat/ idxday, invtzn
