C**** WRRGRD.F 
c
      subroutine wrrgrd( ierr,
     &                         iounit )
c
c-----------------------------------------------------------------------
c
c    Copies the input emissions to output and writes an EMBR record
c
c    Argument Declaration.
c     Outputs:
c       ierr     I error flag
c     Inputs:
c       iounit   I number of facilities currently in arrays
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
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 jerr, irec, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
c
c   --- set all EMBR common variables ---
c
      do i=1,ncrthd
         emout(i) = emin(i)
      enddo
      do i=1,nspchd
         spcout(i) = spcpol(i)
      enddo
c
c   --- call routine to write EMBR record ---
c
      call wrembr( jerr,
     &                      iounit, irec )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
