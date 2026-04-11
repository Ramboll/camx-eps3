C**** WTLMRGU.F 
c
      subroutine wtlmrgu( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes tables of emissions totals to the message file.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I  error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamcntl.inc'
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'iomrguam.inc'
      include 'mrguemis.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   MXVAL    I   maximum number of columns (species)
c
      integer*4 MXVAL
c
      parameter( MXVAL  = MXCRIT + MXSPEC )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*80 title
      character*20 units, codtit, codnam(IDXOUT)
      integer*4    jerr, i
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
      title = 'Total Emissions Processed'
      units = TONLBL
      codtit = ' '
      codnam(IDXIN) = 'Input Emissions'
      codnam(IDXOUT) = 'Output Emissions'
      do i=1,MXVAL
         header(1,i) = 'Input'
         header(2,i) = 'Output'
      enddo
      write(IOWMSG,'(//)',ERR=7000)
      write(IOWMSG,9000,ERR=7000)
     &'Note to the user:  The criteria pollutant emissions are ',
     &'                   reported as the sum of all contributing ',
     &'                   speciated emissions.',' ',
     &'                   The totals are calulated by applying the',
     &'                   molecular weights specified in the ',
     &'                   /SPECIES LIST/ packet of the USERIN file.'
c
c   --- call routine to output table of processed emissions ---
c
      call wrctbl8( jerr,
     &                IOWMSG, title, units, IDXOUT, IDXOUT, 20,
     &                 codnam, codtit, ncrthd, crtnam, totems, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to output table of speciated emissions ---
c
      title = 'Total Speciated Emissions Processed'
      call wrctbl8( jerr,
     &             IOWMSG, title, units, IDXOUT, IDXOUT, 20,
     &               codnam, codtit, nspecs, spcnam, totspc, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing source category tables call the routine ---
c
      if( lcrtasc(IDXFMT) .OR. lspcasc(IDXFMT) ) then
        call wrtasc( jerr, IOWMSG, lcrtasc, lspcasc )
        if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- if doing hourly tables call the routine ---
c
      if( lcrthr(IDXFMT) .OR. lspchr(IDXFMT) ) then
        call wrthrs( jerr, IOWMSG, lcrthr, lspchr )
        if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- if doing hourly tables by ASC call the routine ---
c
      if( lcrthcd(IDXFMT) .OR. lspchcd(IDXFMT) ) then
        call wrthcd( jerr, IOWMSG, lcrthcd, lspchcd )
        if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- set error flag and return ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999)
     &          'ERROR:  Writing message file generating output tables.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(10X,61('-'),7(/,10X,A),/,10X,61('-'))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
