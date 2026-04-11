C*** HDRPRP.F 
c
      subroutine hdrprp( ierr )
c
c-----------------------------------------------------------------------
c
c    Sets the header variables and writes the EMBR header for the
c    PREPNT program
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
      include 'embrdat.inc'
      include 'uamregn.inc'
      include 'ioprepnt.inc'
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
c     getime  --  makes system call to get date and time (library)
c     wrhdr   --  writes the output EMBR header (library)
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   cdate   C   character string for date and time
c   jerr    I   error flag for subroutine calls
c   string  C   character string for temporary storage
c
      character*(5*MXSTR) string
      character*40        cdate
      integer*4           jerr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c  --- initialize some variables ----
c
      filtyp = 'POINTS'
c
c   --- call routine to get date and time string, and create header
c       string ---
c
      call getime(cdate)
      string = VERSON // cdate
c
c   --- set the rest of the header variables ----
c
      rgutme = xorig
      rgutmn = yorig
      irgzn = izone
      nxrgn = nxcell
      nyrgn = nycell
      dxrgn = dxcell
      dyrgn = dycell
c
c   --- call routine to create the header ---
c
      curofl = ombrfl
      curifl = afsfl
      call wrhdr(jerr,
     &                 string, IOWMBR )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
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
