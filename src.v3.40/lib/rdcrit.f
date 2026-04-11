C**** RDCRIT.F 
c    
      subroutine rdcrit( ierr,
     &                         iounit)
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the 
c    /CRITERIA POLLUTANT/ packet in common variables
c
c    Argument declaration.
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
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declerations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin   I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(KEYLEN) keywrd, keyin
      character*(MXSTR)  string
      integer*4          irec, jerr
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/CRITERIA POLLUTANT/'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      ncrthd = 0
      irec = 0
c
c   --- call routine to get to find packet identifier ---
c
      call fndkey( jerr,
     &                   iounit, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read origin data (reading through unused data) ---
c
  111 continue 
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7003 ) string
c
c   --- return if /END/ keyword found ---
c
      keyin = string(1:20)
      call lftjst( keyin )
      call low2up( keyin )
      if( keyin .EQ. KEYEND ) goto 222
c
c   --- incrament the counter and check for overflow ---
c
      ncrthd = ncrthd + 1
      if( ncrthd .GT. MXCRIT ) goto 7004
      read( string,'(20X,A10,I10)',ERR=7002,END=7002 ) crtnam(ncrthd), 
     &                                                  isarod(ncrthd)
      call lftjst( crtnam(ncrthd) )
      call low2up( crtnam(ncrthd) )
c
c   --- get next pollutant ----
c
      goto 111
c
c   --- set error flag to success ---
c
  222 continue
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
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                     keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999) 
     &                   'Number of criteria pollutants exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999)'Pollutants in USERIN: ',ncrthd
      write(ioerr,'(10X,A,I10)',ERR=9999)'Maxumium allowed    : ',MXCRIT
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXCRIT ',
     &                                                  'and recompile.'
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
