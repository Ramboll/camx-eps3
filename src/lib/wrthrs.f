C**** WRTHRS.F 
c
      subroutine wrthrs( ierr, 
     &                         iounit, lcrit, lspec )
c
c-----------------------------------------------------------------------
c
c    Writes the table of emissions totals by hour.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       iounit I unit number of message file
c       lcrit  L .TRUE. if table of criteria emissions is requested
c       lspec  L .TRUE. if table of speciated emissions is requested
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    -gwilson-   Jul 22, 2009    now initializes units string even 
c                                when only doing speciated table
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'cntylst.inc'
      include 'msgtot.inc'
      include 'embrdat.inc'
      include 'uamcntl.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
      logical*4 lcrit(*)
      logical*4 lspec(*)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   MXVAL   I    maximum number of pollutant values
c
      integer*4 MXVAL
c
      parameter( MXVAL = MXCRIT+MXSPEC)
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) fname, tmpnam
      character*80      title
      character*20      units, codtit
      character*10      polnam(2,MXVAL)
      character*8       label(MXHOUR)
      integer*4         jerr, i, icsvunit
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- if not doing any of these tables, just return with success ---
c
      if( .NOT. lcrit(IDXFMT) .AND. .NOT. lspec(IDXFMT) .AND.
     &               .NOT. lcrit(IDXCSV) .AND. .NOT. lspec(IDXCSV)) then
        ierr = ISUCES
        goto 9999
      endif
c
      codtit = 'Hour'
      units = TONLBL
      units = units(:strmin(units))//'/Hour'
c
c   --- if not doing criteria pollutants skip this section ---
c
      if( .NOT. lcrit(IDXFMT) .AND. .NOT. lcrit(IDXCSV) ) goto 111
c
c   --- set up the headers and pollutant names ---
c
      do i=1,ncrthd
         polnam(1,i) = crtnam(i)
         polnam(2,i) = crtnam(i)
      enddo
c
      do i=1,MXHOUR
         write(label(i),'(3X,I2.2)') i
      enddo
      header = ' '
      do i=1,MXCRIT
         header(1,i) = 'Input'
         header(2,i) = 'Output'
      enddo
c
c   --- write out counties with no emissions ---
c
      title = 'Criteria Pollutant Emissions by Hour'
c
c   --- call routine to output table ---
c
      if( lcrit(IDXFMT) ) then
         call msgtbl( jerr,
     &                  iounit, title, units, MXHOUR, MXHOUR,
     &                       8, label, codtit, ncrthd, header,polnam, 
     &                                               crtihrs, crtohrs )
         if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- if doing CSV tables, write table and close file ---
c
      if( lcrit(IDXCSV) ) then
         tmpnam = curofl
         fname = curofl(:strmin(curofl))//'.hrs.crt.csv'
         icsvunit = 50+iounit
         open(unit=icsvunit,file=fname)
         curofl = fname
         call msgcsv( jerr,
     &                      icsvunit, title, units, MXHOUR, MXHOUR,
     &                          8, label, codtit, ncrthd, header,
     &                                      polnam, crtihrs, crtohrs )
         if( jerr .NE. ISUCES ) goto 9999
         close(icsvunit)
         curofl = tmpnam
      endif
c
c   --- if not doing speciated pollutants skip this section ---
c
  111 continue
      if( .NOT. lspec(IDXFMT) .AND. .NOT. lspec(IDXCSV) ) goto 222
c
c   --- set up the headers and pollutant names ---
c
      do i=1,nspecs
         polnam(1,i) = spcnam(i)
         polnam(2,i) = spcnam(i)
      enddo
c
      do i=1,MXHOUR
         write(label(i),'(3X,I2.2)') i
      enddo
      header = ' '
      do i=1,MXSPEC
         header(1,i) = 'Input'
         header(2,i) = 'Output'
      enddo
c
c   --- write out counties with no emissions ---
c
      title = 'Speciated Emissions by Hour'
      units = TONLBL
      units = units(:strmin(units))//'/Hour'
c
c   --- call routine to output table ---
c
      if( lspec(IDXFMT) ) then
          call msgtbl( jerr,
     &                  iounit, title, units, MXHOUR, MXHOUR,
     &                          8, label, codtit, nspecs, header, 
     &                                      polnam, spcihrs, spcohrs )
          if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- if doing CSV tables, write table and close file ---
c
      if( lspec(IDXCSV) ) then
         tmpnam = curofl
         fname = curofl(:strmin(curofl))//'.hrs.spc.csv'
         icsvunit = 50+iounit
         open(unit=icsvunit,file=fname)
         curofl = fname
         call msgcsv( jerr,
     &                      icsvunit, title, units, MXHOUR, MXHOUR,
     &                          8, label, codtit, nspecs, header,
     &                                      polnam, spcihrs, spcohrs )
         if( jerr .NE. ISUCES ) goto 9999
         close(icsvunit)
         curofl = tmpnam
      endif
c
c   --- set error flag to success ---
c
  222 continue
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
