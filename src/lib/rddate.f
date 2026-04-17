C**** RDDATE.F 
c
      subroutine rddate( ierr,
     &                         iounit )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /DATE/
c    packet in common variables
c
c    Arguemnt declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       iounit I logical unit number of file to read
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamcntl.inc'
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin   I   returns actual length of string
c   caljul   I   returns the julian date for a calander day
c
      integer*4 strmin
      integer*4 caljul
c
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   keyword for packet as variable
c   nhours  I   number of hours in episode
c   jerr    I   error flag for subroutine calls
c
      character*(5*MXSTR) string
      character*(KEYLEN)  keywrd
      integer*4           nhours, irec, jerr
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/DATE/'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
c
c   --- call routine to get to find packet identifier ---
c
      call fndkey( jerr,
     &                   iounit, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read file note ---
c
      read( iounit,'(A)',ERR=7001,END=7002 ) string
      fnote = string(21:)
c
c   --- read time data (checking for valid data) ---
c
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7002 ) string
      read( string,'(20X:I10)',ERR=7003 ) ibgcal
      ibgcal = MOD(ibgcal,1000000)
      iencal = ibgcal
      ibgdat = caljul( ibgcal )
      iendat = ibgdat
c
      read( iounit,'(A)',ERR=7001,END=7002 ) string
      read( string,'(20X:F10.0)',ERR=7003 ) begtim
      begtim = begtim * 100.
c
      read( iounit,'(A)',ERR=7001,END=7002 ) string
      read( string,'(20X:F10.0)',ERR=7003 ) endtim
      endtim = endtim * 100.
c
      itzone = 6
      read( iounit,'(A)',ERR=7001,END=7002 ) string
      call low2up( string )
      call lftjst( string )
      if( string(1:5) .NE. KEYEND )
     &          read( string,'(20X:I10)',ERR=7003 ) itzone
c
c   --- make sure the data/times use consistent format ---
c
      if( endtim .EQ. 0. ) then
         iendat = iendat - 1
         endtim = 2400.
      endif
      nhours = (iendat-ibgdat)*24 + INT( (endtim-begtim)/100 )
      if( nhours .GT. MXHOUR ) goto 7004
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading USERIN file.'
      write(ioerr,'(3A,//)',ERR=9999)
     &         'This program requires the ',keywrd(:strmin(keywrd)),
     &                                             ' packet of USERIN.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999) 'Number of hours exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Hours in episode: ',nhours
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed: ',MXHOUR
      write(ioerr,'(10X,A,I10)',ERR=9999) 
     &                         'Beginning hour  : ',INT(begtim/100.)
      write(ioerr,'(10X,A,I10)',ERR=9999) 
     &                         'Ending hour     : ',INT(endtim/100.)
      write(ioerr,'(A,//)',ERR=9999) 
     &                     'This version expects hours from 1 to 24.'

      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7006 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A,//)',ERR=9999)  string(:strmin(string))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A)',ERR=9999)  string(:strmin(string))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
