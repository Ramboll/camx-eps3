C**** WTLLBS.F 
c
      subroutine wtllbs( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the tables of emissions totals for the LBASE program
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
      include 'cntylst.inc'
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'iolbase.inc'
      include 'lbasetot.inc'
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
c   codnam  C    character string array for title of procesed emissions
c   codtit  C    character string for title of codes
c   jerr    I    error flag for subroutine calls
c   name    C    character string array of names of criteria pollutants
c   title   C    character string for title of tables
c   units   C    character string of units of emissions
c
      character*80 title
      character*20 units, codtit, codnam(IDXSKP)
      character*10 name(MXCRIT)
      integer*4    jerr, i
      logical*4    lfalse(IDXCSV)
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      lfalse(IDXFMT) = .FALSE.
      lfalse(IDXCSV) = .FALSE.
      curofl = msgfl
c
c   --- set some labels ---
c
      title = 'Total Emissions Processed'
      units = TONLBL
      units = units(:strmin(units))//'/Day'
      codtit = ' '
      codnam(IDXIN) = 'Input Emissions'
      codnam(IDXOUT) = 'Output Emissions'
      codnam(IDXSKP) = 'Emissions Skipped'
c
c   --- set up the headers and pollutant names ---
c
      do i=1,ncrthd
         name(i) = crtnam(i)
         header(1,i) = 'Input'
         header(2,i) = 'Output'
      enddo
c
c   --- call routine to output table of processed emissions ---
c
      call wrctbl8( jerr,
     &               IOWMSG, title, units, IDXSKP, IDXSKP, 20,
     &                   codnam, codtit, ncrthd, name, totems, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing county tables call the routine ----
c
      if( lcrtcty(IDXFMT) .OR. lcrtcty(IDXCSV) ) then
        call wrtcty( jerr, IOWMSG, lcrtcty, lfalse )
        if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- if doing source category tables call the routine ----
c
      if( lcrtasc(IDXFMT) .OR. lcrtasc(IDXCSV) ) then
        call wrtasc( jerr, IOWMSG, lcrtasc, lfalse )
        if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- if doing county/source table call the routine ---
c
      if( lcrtfcd(IDXFMT) .OR. lcrtfcd(IDXCSV) ) then
        call wrtfcd( jerr, IOWMSG, lcrtfcd, lfalse )
        if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- if doing hourly tables call the routine ---
c
      if( lcrthr(IDXFMT) .OR. lcrthr(IDXCSV) ) then
        call wrthrs( jerr, IOWMSG, lcrthr, lfalse )
        if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- if doing hourly tables by ASC call the routine ---
c
      if( lcrthcd(IDXFMT) .OR. lcrthcd(IDXCSV) ) then
        call wrthcd( jerr, IOWMSG, lcrthcd, lfalse )
        if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Statements:
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
