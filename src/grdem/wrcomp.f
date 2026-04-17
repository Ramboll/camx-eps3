C***** WRCOMP.F 
c
      subroutine wrcomp( ierr,
     &                         iounit, irec )
c
c-----------------------------------------------------------------------
c
c    Writes one record of the data section of EMBR using the variables
c    stored in common block.  A subset of fields for a smaller output.
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
c      11/10/91  --gmw--  original development
c      08/07/97  --mmj--  modified to write shorter 
c                            length records (compressed)
c      01/10/08  --mmj--  modified to support EPS3 formats
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
c    Local variables:
c-----------------------------------------------------------------------
c
c   string  C   string for first part of each record
c
      character*10       string
      integer*4          i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- write data into variables ---
c
      write( string,9000,ERR=7000 ) fipin, sbrgn
c
c   --- write EMBR record ---
c
      write( iounit,ERR=7000 ) string, siccd, scccd, rectyp,
     &                         invbeg, invend, ijcell,
     &                         plant, stkid,
     &                         point, segno, utme, utmn, iutmzn,
     &                         stkgev, stkdia, stktmp, stkhgt,
     &                         (emout(i),i=1,ncrthd)
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
      write(IOWSTD,9001,ERR=9999) 'ERROR: Writing compressed data ',
     &                            'record to EMBR file at record:',irec
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(a5,a5)
 9001 format(/,1x,a,a,i5,/)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
