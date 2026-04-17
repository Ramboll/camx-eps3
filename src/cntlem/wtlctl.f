C**** WTLCTL.F 
c
      subroutine wtlctl( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the tables of emissions totals for the CNTLEM program
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
      include 'uamcntl.inc'
      include 'msgtot.inc'
      include 'iocntlem.inc'
      include 'cntlem.inc'
      include 'ctltot.inc'
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
c   codnam  C    character string array for title of procesed emissions
c   codtit  C    character string for title of codes
c   jerr    I    error flag for subroutine calls
c   name    C    character string array of names of criteria pollutants
c   spccty  R    array for county table
c   title   C    character string for title of tables
c   totcty  R    array for county table
c   units   C    character string of units of emissions
c
      character*80 title
      character*20 units, codtit, codnam(IDXSKP)
      character*10 name(MXCRIT)
      integer*4    jerr, ifp, i
      real*4       tottmp
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      curofl = msgfl
      nspecs = nspchd
      do i=1,nspchd
        spcnam(i) = spclst(i)
      enddo
c
c   --- set some labels ---
c
      title = 'Total Emissions Processed'
      units = TONLBL
      codtit = ' '
      codnam(IDXIN) = 'Input Emissions'
      codnam(IDXOUT) = 'Output Emissions'
      codnam(IDXSKP) = 'Emissions Skipped'
      do i=1,ncrthd
         name(i) = crtnam(i)
      enddo
      do i=1,MXCRIT+MXSPEC
         header(1,i) = 'Input'
         header(2,i) = 'Output'
      enddo
c
c   --- call routine to output table of processed emissions ---
c
      call wrctbl8( jerr,
     &                IOWMSG, title, units, IDXSKP, IDXSKP, 20,
     &                 codnam, codtit, ncrthd, name, totems, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if the species list is non-empty create table of speciated
c       emissions totals ---
c
      title = 'Total Speciated Emissions Processed'
      if( nspchd .GT. 0 ) call wrctbl8( jerr,
     &              IOWMSG, title, units, IDXSKP, IDXSKP, 20,
     &                codnam, codtit, nspchd, spclst, totspc, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- sort array of source categories ---
c
      if( nscsic .LE. 0. ) goto 111
      call chrsrt(sccsic,14,nscsic,idxsrt)
c
c   --- write tables of regulatory controls by source category for
c       entire domain ---
c
      title = 'Totals by Source Category for Entire Domain'
c
c   --- write tables of discretionary controls by source category for
c       entire domain ---
c
      call dsctbl( ierr, title, 0 )
      if( ierr .NE. ISUCES ) goto 9999
c
c   --- if write flag is turned on, produce tables by county ---
c
      if( lprntc ) then
        do 10 ifp = 1,ncdfip
           tottmp = 0.
           do i=1,ncrthd
               tottmp = tottmp + totifp(0,ifp,i)
           enddo
           if( tottmp .LE. 0 ) goto 10
           title = 'Totals by Source Category for County: '//fipcd(ifp)
c
c   --- write tables of discretionary controls by source category for
c       this county ---
c
           call dsctbl( ierr, title, ifp )
           if( ierr .NE. ISUCES ) goto 9999
   10   continue
      endif
c
c   --- if doing county tables call the routine ---
c
  111 continue
      call wrtcty( jerr, IOWMSG, lcrtcty, lspccty )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing source category tables call the routine ---
c
      call wrtasc( jerr, IOWMSG, lcrtasc, lspcasc )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing table by facilty, call the routine ---
c   
      call wrtfac( jerr, IOWMSG, lcrtfac, lspcfac )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing table by stack, call the routine ---
c   
      call wrtstak( jerr, IOWMSG, lcrtstk, lspcstk )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing table by stack, call the routine ---
c
      call wrtpnt( jerr, IOWMSG, lcrtpnt, lspcpnt )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing county/source table call the routine ---
c
      call wrtfcd( jerr, IOWMSG, lcrtfcd, lspcfcd )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing hourly tables call the routine ---
c
      call wrthrs( jerr, IOWMSG, lcrthr, lspchr )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing hourly tables by ASC call the routine ---
c
      call wrthcd( jerr, IOWMSG, lcrthcd, lspchcd )
      if( jerr .NE. ISUCES ) goto 9999
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
