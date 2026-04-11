C**** RDEMBR.F 
c
      subroutine rdembr( ierr,
     &                         iounit, irec )
c
c-----------------------------------------------------------------------
c
c    Reads one record from the data section of EMBR file storing
c    data in common block variables
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       iounit I logical unit number of file to read
c       irec   I count of record for messages
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
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
      integer*4 irec
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns actual length of string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   string  C   string for first part of each record
c   keywrd  C   character string for /TRAILER/ keyword
c
      character*10       string
      character*(KEYLEN) keywrd
      integer*4          ilen, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/TRAILER/'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      ilen = strmin ( keywrd )
      read( iounit,ERR=7000,END=7001 ) string
c
c   --- check for end of data records ---
c
      if( string(:ilen) .EQ. keywrd(:ilen) ) then
          ierr = IEOD
          goto 9999
      endif
c
c   --- read record, again ---
c
      backspace( iounit )
      read( iounit,ERR=7000,END=7001 ) string, siccd, scccd, rectyp,
     &            period, invbeg, invend, ijcell, plant, stkid, point,
     &             segno,  utme, utmn, iutmzn, stkgev, stkdia, stktmp, 
     &              stkhgt, (emin(i),i=1,ncrthd), (spcpol(i),i=1,nspchd)
c
c   --- read data into variables ---
c
      read( string,'(2A5)',ERR=7000 ) fipin, sbrgn
c
c   --- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading data ',
     &                         'record from EMBR file at line:',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Premature EOF found',
     &                  ' reading data of EMBR file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
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
