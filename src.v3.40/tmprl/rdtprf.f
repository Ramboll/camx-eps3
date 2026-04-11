C**** RDTPRF.F 
c
      subroutine rdtprf( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the temporal profiles file and stores the data in common
c    variables to be used by the TMPRL program
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'hash.inc'
      include 'iotmprl.inc'
      include 'usrtmprl.inc'
      include 'tmprlprf.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c     fndkey  --  searches a file for a keyword (library)
c     lftjst  --  left justifies a character string (library)
c     low2up  --  converts a character string to upper case (library)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin   I   returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   KEYMON  C   keyword used for search of /MONTHLY/ packet
c   KEYWEK  C   keyword used for search of /WEEKLY/ packet
c   KEYWKD  C   keyword used for search of /DIURNAL WEEKDAY/ packet
c   KEYWKE  C   keyword used for search of /DIURNAL WEEKEND/ packet
c
      character*(KEYLEN) KEYMON
      character*(KEYLEN) KEYWEK
      character*(KEYLEN) KEYWKD
      character*(KEYLEN) KEYWKE
c
      parameter( KEYMON = '/MONTHLY/' )
      parameter( KEYWEK = '/WEEKLY/' )
      parameter( KEYWKD = '/DIURNAL WEEKDAY/' )
      parameter( KEYWKE = '/DIURNAL WEEKEND/' )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   jerr    I   error flag for subroutine calls
c   keychk  C   temporary storage for looking for /END/ keyword
c   keywrd  C   temporary storage for keywords
c   ncount  I   temproary array counter
c   string  C   character string for temporary storage
c
      character*(4*MXSTR)  string
      character*(KEYLEN)   keywrd, keychk
      integer*4            ncount, jerr, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- call routine to find /MONTHLY/ packet identifier ---
c
      keywrd = KEYMON
      call fndkey( jerr,
     &                   IORPRF, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read monthly profiles (incramenting the array counter) ---
c
      nmncd = 0
      ncount = 1
  111 continue
      read( IORPRF,'(A)',ERR=7001,END=7002 ) string
      keychk = string(:20)
c
      call low2up( keychk )
      call lftjst( keychk )
      if( keychk .EQ. KEYEND ) goto 222
      nmncd = nmncd + 1
      if( nmncd .GT. MXPRF ) goto 7003
      read( string,8000,ERR=7004 ) imncd(nmncd),
     &                                 (imnfac(nmncd,i),i=1,MXMON+1)
      ncount = ncount + 1
      goto 111
c
c   --- call routine to find /WEEKLY/ packet identifier ---
c
  222 continue
      keywrd = KEYWEK
      call fndkey( jerr,
     &                   IORPRF, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read weekly profiles (incramenting the array counter) ---
c
      nwkcd = 0
      ncount = 1
  333 continue
      read( IORPRF,'(A)',ERR=7001,END=7002 ) string
      keychk = string(:20)
c
      call low2up( keychk )
      call lftjst( keychk )
      if( keychk .EQ. KEYEND ) goto 444
      nwkcd = nwkcd + 1
      if( nwkcd .GT. MXPRF ) goto 7003
      read( string,8001,ERR=7004 ) iwkcd(nwkcd),
     &                                (iwkfac(nwkcd,i),i=1,MXWEK+1)
      ncount = ncount + 1
      goto 333
c
c   --- call routine to find /DIURNAL WEEKDAY/ packet identifier ---
c
  444 continue
      keywrd = KEYWKD
      call fndkey( jerr,
     &                   IORPRF, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read daily profiles for mid-day
c                          (incramenting the array counter) ---
c
      ndycd(IDXMID) = 0
      ncount = 1
  555 continue
      read( IORPRF,'(A)',ERR=7001,END=7002 ) string
      keychk = string(:20)
c
      call low2up( keychk )
      call lftjst( keychk )
      if( keychk .EQ. KEYEND ) goto 666
      ndycd(IDXMID) = ndycd(IDXMID) + 1
      if( ndycd(IDXMID) .GT. MXPRF ) goto 7003
c
c   --- shift factors back by one hour if not using standard time ---
c
      read( string,8002,ERR=7004 ) idycd(IDXMID,ndycd(IDXMID)),
     &                    (idyfac(IDXMID,ndycd(IDXMID),i),i=1,MXDAY+1)
      ncount = ncount + 1
      goto 555
c
c   --- call routine to find /DIURNAL WEEKEND/ packet identifier ---
c
  666 continue
      keywrd = KEYWKE
      call fndkey( jerr,
     &                   IORPRF, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 888
c
c   --- read daily profiles for weekend
c                          (incramenting the array counter) ---
      ndycd(IDXEND) = 0
      ncount = 1
  777 continue
      read( IORPRF,'(A)',ERR=7001,END=7002 ) string
      keychk = string(:20)
c
      call low2up( keychk )
      call lftjst( keychk )
      if( keychk .EQ. KEYEND ) goto 888
      ndycd(IDXEND) = ndycd(IDXEND) + 1
      if( ndycd(IDXEND) .GT. MXPRF ) goto 7003
c
c   --- shift factors back by one hour if not using standard time ---
c
      read( string,8002,ERR=7004 ) idycd(IDXEND,ndycd(IDXEND)),
     &                    (idyfac(IDXEND,ndycd(IDXEND),i),i=1,MXDAY+1)
      ncount = ncount + 1
      goto 777
c
c   --- done processing file, echo some data to message file ---
c
  888 continue
      write(IOWMSG,'(/,T10,A,I5)',ERR=7005)
     &               'Number of MONTHLY temporal profiles read: ',nmncd
      write(IOWMSG,'(/,T10,A,I5)',ERR=7005)
     &               'Number of WEEKLY temporal profiles read: ',nwkcd
      write(IOWMSG,'(/,T10,A,I5)',ERR=7005)
     &            'Number of DIURNAL WEEKDAY temporal profiles read: ',
     &                                                     ndycd(IDXMID)
      write(IOWMSG,'(/,T10,A,I5)',ERR=7005)
     &            'Number of DIURNAL WEEKEND temporal profiles read: ',
     &                                                     ndycd(IDXEND)
c
c  --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,3A,//)',ERR=9999)
     &    'ERROR: This program requires the ',keywrd(:strmin(keywrd)),
     &                     ' packet of temporal profiles file.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 
     &   'ERROR:  Reading ',keywrd(:strmin(keywrd)),
     &         ' packet of temporal profiles file at record: ',ncount
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',prffl(:strmin(prffl))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I5)',ERR=9999) 'ERROR: Reading ',
     &                           keywrd(:strmin(keywrd)),
     &          ' packet of temporal profiles file at line: ',ncount
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A)',ERR=9999) 
     &        'ERROR:  Number of temporal profiles in ',
     &                                  keywrd(:strmin(keywrd)),
     &            ' packet of temporal profiles file exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXPRF
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXPRF ',
     &                                                  'and recompile.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 
     &   'ERROR:  Reading ',keywrd(:strmin(keywrd)),
     &         ' packet of temporal profiles file at record: ',ncount
      write(ioerr,'(2A)',ERR=9999) 'File: ',prffl(:strmin(prffl))
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(2A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7005 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(I5,12(I4),I5)
 8001 format(I5,7(I4),I5)
 8002 format(I5,24(I4),I5)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
