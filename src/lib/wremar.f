C**** WREMAR.F 
c
      subroutine wremar( ierr,
     &                         iounit, irec )
c
c-----------------------------------------------------------------------
c
c    Writes one record of the data section of EMAR using the variables
c    stored in common block
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
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   KEYDAT  C   parameter for /DATA/ keyword
c
      character*(KEYLEN) KEYDAT
c
      parameter( KEYDAT = '/DATA/' )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- if this is the first record, write the /DATA/ keyword ---
c
      if( irec .EQ. 1 ) write(iounit,'(A)',ERR=7000) KEYDAT
c
c   --- write EMBR record ---
c
      write(iounit,9000) fipin, sbrgn, siccd, scccd, rectyp,
     &                    period, invbeg, invend, ijcell,
     &                    plant, stkid, point, segno, utme, 
     &                    utmn, iutmzn, stkgev, stkdia, stktmp, 
     &                    stkhgt, (emout(i),i=1,ncrthd),
     &                                       (spcout(i),i=1,nspchd)
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
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Writing data ',
     &                          'record to EMAR file at line:',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curofl(:strmin(curofl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(2(A5,1X),A4,1X,A10,1X,A1,1X,A2,1X,2(I8,1X),I6,3(1X,A10),1X,
     &                   A3,1X,2(F10.2,1X),I2,5X,4(F10.2,1X),
     &                                  100(:,1X,E10.4),100(:,1X,E10.4))
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
