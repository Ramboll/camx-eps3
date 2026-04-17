C**** RDHCHM.F 
c
      subroutine rdhchm( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the header of the input EMBR file
c    Checks that the speciation doesn't already exist
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
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
      include 'iochmspl.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Library Subroutines Called:
c-----------------------------------------------------------------------
c
c   rdhdr  -- reads the header of an EMBR file
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c    jerr   I  error code for subroutines called
c
      integer*4 jerr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      curifl = infl
      curofl = outfl
c
c  --- call rdhdr ---
c
      call rdhdr(jerr, IOREMB)
      if(jerr .NE. ISUCES) goto 9999
c
c  --- check that speciation has not already been performed ---
c
      if(nspchd .GT. 0) goto 7000
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Input EMBR file is ',
     &                                             'already speciated.'
      write(ioerr,'(2A)',ERR=9999)  'An EMBR file can only be ',
     &                           'processed through this module once.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',infl(:strmin(infl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
