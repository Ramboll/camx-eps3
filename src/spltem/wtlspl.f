C**** WTLSPL.F
c
      subroutine wtlspl( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the tables of emissions totals for the SPLTEM program
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
c      08/19/97  --mmj--  original development
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iospltem.inc'
      include 'spltem.inc'
      include 'spltemtot.inc'
      include 'wgtfrac.inc'
      include 'embrdat.inc'
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
c     wrctbl  --  writes a table with character labels
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin   I   returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   codnam  C    character string array for title of procesed emissions
c   codtit  C    character string for title of codes
c   em      R    sum of criteria emissions
c   jerr    I    error flag for subroutine calls
c   ntmp    I    number of temporary non-zero profiles
c   title   C    character string for title of tables
c   tmpems  R    temporary array of nonzero profiles
c   tmpprf  C    temporary array of nonzero profile codes
c   units   C    character string of units of emissions
c
      character*(LENPRF) tmpprf(MXPRF)
      character*80       title
      character*20       units, codtit, codnam(IDXRNG)
      character*10       tmpnam
      integer*4          jerr, ifp, i, j
      integer*4          ntmp
      real*4             em, tmpems(MXPRF,MXCRIT)
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- set some labels ---
c
      title = 'Total Emissions Processed'
      units = 'English Tons'
      codtit = ' '
      codnam(IDXIN) = 'Input Emissions'
      codnam(IDXOUT) = 'Output Emissions'
      codnam(IDXDAT) = 'Low Level Emissions'
      codnam(IDXELE) = 'Elevated Emissions'
      codnam(IDXSKP) = 'Emissions Skipped'
      codnam(IDXRNG) = 'Outside Sub Region'
c
c   --- call routine to output table of processed emissions ---
c
      call wrctbl( jerr,
     &             IOWMSG, title, units, IDXRNG, IDXRNG, 20,
     &             codnam, codtit, ncrthd, crtnam, totems,.FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- produce profile table for criteria pollutants of output emissions ----
c
      title = 'Output Emissions by Profile Code'
      codtit = 'Prof'
      do i = 1, ncrthd
         if ( lsplit(i) ) tmpnam = crtnam(i)
      enddo
c
c   --- move non-zero profiles to temporary array ---
c
      ntmp = 0
      do 30 i = 1, nprfwf
         if ( totprf(i) .GT. 0. ) then
            ntmp = ntmp + 1
            tmpprf(ntmp) = profwf(i)
            tmpems(ntmp,1) = totprf(i)
         endif
   30 continue
c
      if( ntmp .GT. 0 ) then
         call wrctbl( jerr,
     &                  IOWMSG, title, units, MXPRF, ntmp, LENPRF,
     &                  tmpprf, codtit, 1, tmpnam, tmpems,.TRUE. )
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
 7000 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR:  Writing message file: ',
     &                                   msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(1X,A,A,A)
 9001 format(A)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
