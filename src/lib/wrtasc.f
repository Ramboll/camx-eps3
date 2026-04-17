C**** WRTASC.F 
c
      subroutine wrtasc( ierr,
     &                        iounit, lcrit, lspec )
c
c-----------------------------------------------------------------------
c
c    Writes the tables of emissions totals by source category code.
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
c                                when only doing speciated table:w
c
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
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
c   MXLBL   I    maximum number of labels
c   MXVAL   I    maximum number of pollutant values
c
      integer*4 MXLBL
      integer*4 MXVAL
c
      parameter( MXLBL = MXASC )
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
      character*13      label(MXLBL)
      integer*4         icsvunit
      integer*4         idxsrt(MXLBL), nlabel, jerr, jspc, kasc, i
      real*4            arrayin(MXLBL,MXVAL), arrayout(MXLBL,MXVAL)
      real*4            tottmp
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
c   --- call routine to sort ASC codes ---
c
      if( nmsgasc .LE. 0 ) goto 222
      call chrsrt(msgasc,10,nmsgasc,idxsrt)
      codtit = 'Source Code'
      units = TONLBL
      units = units(:strmin(units))//'/Day'
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
c   --- produce table of input emissions by source category ---
c
      title = 'Criteria Pollutant Emissions by Source Category'
      nlabel = 1
      do 10 i=1,nmsgasc
         kasc = idxsrt(i)
         tottmp = 0.
         do jspc=1,ncrthd
            arrayin(nlabel,jspc) = crtiasc(kasc,jspc)
            arrayout(nlabel,jspc) = crtoasc(kasc,jspc)
            tottmp = tottmp + crtiasc(kasc,jspc) + crtoasc(kasc,jspc)
         enddo
         if( tottmp .LE. 0. ) goto 10
         write(label(nlabel),'(2X,A)') msgasc(kasc)
         nlabel = nlabel + 1
   10 continue
      nlabel = nlabel - 1
c
c   --- call routine to output table of input by ASC ---
c
      if( lcrit(IDXFMT) ) then
         call msgtbl( jerr,
     &                  iounit, title, units, MXLBL, nlabel,
     &                          13, label, codtit, ncrthd, header, 
     &                                      polnam, arrayin, arrayout )
         if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- if doing CSV tables, write table and close file ---
c
      if( lcrit(IDXCSV) ) then
         tmpnam = curofl
         fname = curofl(:strmin(curofl))//'.asc.crt.csv'
         icsvunit = 50+iounit
         open(unit=icsvunit,file=fname)
         curofl = fname
         call msgcsv( jerr,
     &                      icsvunit, title, units, MXLBL, nlabel,
     &                          13, label, codtit, ncrthd, header,
     &                                      polnam, arrayin, arrayout )
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
c   --- produce table of input emissions by source category ---
c
      title = 'Speciated Emissions by Source Category'
      nlabel = 1
      do 20 i=1,nmsgasc
         kasc = idxsrt(i)
         tottmp = 0.
         do jspc=1,nspecs
            arrayin(nlabel,jspc) = spciasc(kasc,jspc)
            arrayout(nlabel,jspc) = spcoasc(kasc,jspc)
            tottmp = tottmp + spciasc(kasc,jspc) + spcoasc(kasc,jspc)
         enddo
         if( tottmp .LE. 0. ) goto 20
         write(label(nlabel),'(2X,A)') msgasc(kasc)
         nlabel = nlabel + 1
   20 continue
      nlabel = nlabel - 1
      units = TONLBL
      units = units(:strmin(units))//'/Day'
c
c   --- call routine to output table of input by ASC ---
c
      if( lspec(IDXFMT) ) then
          call msgtbl( jerr,
     &                  iounit, title, units, MXLBL, nlabel,
     &                          13, label, codtit, nspecs, header, 
     &                                      polnam, arrayin, arrayout )
          if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- if doing CSV tables, write table and close file ---
c
      if( lspec(IDXCSV) ) then
         tmpnam = curofl
         fname = curofl(:strmin(curofl))//'.asc.spc.csv'
         icsvunit = 50+iounit
         open(unit=icsvunit,file=fname)
         curofl = fname
         call msgcsv( jerr,
     &                      icsvunit, title, units, MXLBL, nlabel,
     &                          13, label, codtit, nspecs, header,
     &                                      polnam, arrayin, arrayout )
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
