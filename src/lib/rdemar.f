C**** RDEMAR.F 
c
      subroutine rdemar( ierr,
     &                         iounit, irec )
c
c-----------------------------------------------------------------------
c
c    Reads one record of the data section of EMAR using the variables
c    stored in common block of EMBRDAT.INC
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
c    Function declarations:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c       Local variables:
c-----------------------------------------------------------------------
c
      character*(LENREC) string
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
c
c   --- check for end of data records ---
c
      read(iounit,'(A)',ERR=7000,END=111 ) string
      ilen = strmin ( keywrd )
      if( string(1:ilen) .EQ. keywrd(1:ilen) ) then
          ierr = IEOD
          goto 9999
      endif
c
c   --- read EMAR record ---
c
      read(string,8000,ERR=7000) fipin, sbrgn, siccd, scccd, rectyp,
     &                   period, invbeg, invend, ijcell,
     &                   plant, stkid, point, segno, utme,
     &                   utmn, iutmzn, stkgev, stkdia, stktmp,
     &                   stkhgt, (emout(i),i=1,ncrthd),
     &                                       (spcout(i),i=1,nspchd)
c
c   --- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c   --- end of file found, so write warning and set flag
c       to end of data ---
c
  111 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(2A)',ERR=9999) 'WARNING: Input EMAR ',
     &                            'file contains no /TRAILER/ packet.'
      write(ioerr,'(A)',ERR=9999) 'The file may have been truncated.'
      write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
      ierr = IEOF
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading data ',
     &                         'record from EMAR file at line:',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(2(A5,1X),A4,1X,A10,1X,A1,1X,A2,1X,2(I8,1X),I6,3(1X,A10),1X,
     &               A3,1X,2(F10.0,1X),I2,5X,4(F10.0,1X),100(:,E10.4,1X),
     &                                                 100(:,E10.4,1X))
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
