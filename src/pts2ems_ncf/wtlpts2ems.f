C**** WTLPTS2EMS.F 
c
      subroutine wtlpts2ems( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the tables of emissions totals for the GRDEM program
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
      include 'uamcntl.inc'
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'iopts2ems.inc'
      include 'pts2emstot.inc'
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
c     wrctbl8  --  writes a table with character labels 
c                  (double precision version)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c
      character*80 title
      character*20 units, codtit, codnam(IDXOUT)
      character*10 name(MXSPEC)
      integer*4    jerr, i
      logical*4    lcrit_tbl(IDXCSV), lspec_tbl(IDXCSV)
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      curofl = msgfl
c
c   --- set some labels ---
c
      title = ' '
      codtit = ' '
      codnam(IDXIN) = ' '
      codnam(IDXOUT) = ' '

      title = 'Total Emissions Processed'
      units = TONLBL
      codnam(IDXIN) = 'Input Emissions'
      codnam(IDXOUT) = 'Output Emissions'
      name = ' '
      do i=1,nspecs
         name(i) = spcnam(i)
      enddo
c
c   --- call routine to output table of processed emissions ---
c
      call wrctbl8( jerr,
     &                   IOWMSG, title, units, IDXOUT, IDXOUT, 20,
     &                   codnam, codtit, nspecs, name, totspc, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if the species list is non-empty create table of speciated
c       emissions totals ---
c
      title = ' '
      title = 'Total Speciated Emissions Processed'
c
c   --- if doing hourly tables call the routine ---
c
      lcrit_tbl = .FALSE.
      lspec_tbl(IDXFMT) = .TRUE.
      lspec_tbl(IDXCSV) = .FALSE.
      call wrthrs( jerr, IOWMSG, lcrit_tbl, lspec_tbl )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error statements:
c-----------------------------------------------------------------------
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
