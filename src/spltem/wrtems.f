C**** WRTEMS.F
c
      subroutine wrtems( ierr,
     &                    iounit, ihour, icell, jcell, 
     &                    isarod, spnam, value )
c
c-----------------------------------------------------------------------
c
c    Writes a record to the output file for each chemical
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       iounit I logical unit number of file to write
c       ihour  I hour of episode
c       icell  I x grid location
c       jcell  I y grid location
c       isarod I saroad code of species
c       spnam  C species name
c       value  R emissions to write
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      08/13/97  --mmj--  original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      character*(*) spnam
      integer*4    ierr
      integer*4    iounit
      integer*4    ihour
      integer*4    icell
      integer*4    jcell
      integer*4    isarod
      real*4       value
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin 
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- write record for each chemical ---
c
      write( iounit,9000,ERR=7000 ) ihour, icell, jcell, isarod, value,
     &                              spnam(:strmin(spnam))
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
      write(IOWSTD,9001,ERR=9999) 'ERROR: Writing output data.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(I2,1X,I3,1X,I3,1X,I5,1X,E10.4,1X,A)
 9001 format(/,1X,A/)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
