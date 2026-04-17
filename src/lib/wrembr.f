C***** WREMBR.F 
c
      subroutine wrembr( ierr,
     &                         iounit, irec )
c
c-----------------------------------------------------------------------
c
c    Writes one record of the data section of EMBR using the variables
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
c    Local variables:
c-----------------------------------------------------------------------
c
c   string  C   string for first part of each record
c
      character*10 string
      integer*4    i
      real*4       total
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- make sure there is some emissions for this ---
c
      total = 0.
      do i=1,ncrthd
         total = total + MAX(emout(i),0.0)
      enddo
      do i=1,nspchd
         total = total + MAX(spcout(i),0.0)
      enddo
      if( total .LE. 0. ) then
        ierr = ISUCES
        goto 9999
      endif
c
c   --- write data into variables ---
c
      write( string,'(2A5)',ERR=7000 ) fipin, sbrgn
c
c   --- write EMBR record ---
c
      write( iounit,ERR=7000 )  string, siccd, scccd, rectyp,
     &           period, invbeg, invend, ijcell, plant, stkid, point,
     &            segno,  utme, utmn, iutmzn, stkgev, stkdia, stktmp,
     &             stkhgt, (emout(i),i=1,ncrthd), (spcout(i),i=1,nspchd)
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
     &                         'record from EMBR file at line:',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curofl(:strmin(curofl))
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
