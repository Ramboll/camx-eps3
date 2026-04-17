C**** WRTSTAK.F 
c
      subroutine wrtstak( ierr,
     &                        iounit, lcrit, lspec )
c
c-----------------------------------------------------------------------
c
c    Writes the tables of emissions totals by stack.
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
      parameter( MXLBL = MXPID )
      parameter( MXVAL = MXCRIT+MXSPEC)
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) fname, tmpnam
      character*80      title
      character*36      label(MXLBL)
      character*30      codtit
      character*20      units
      character*10      lastfac, laststk, polnam(2,MXVAL)
      character*10      factmp, staktmp
      character*5       lastcty
      integer*4         icsvunit
      integer*4         idxsrt(MXLBL), nlabel, jerr, jspc, kpid, i, j
      real*4            arrayin(MXLBL,MXVAL), arrayout(MXLBL,MXVAL)
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
      if( nmsgpid .LE. 0 ) goto 222
      call chrsrt(msgpid,35,nmsgpid,idxsrt)
      codtit = 'FIPS      Facility      Stack'
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
         do j=1,MXLBL
           arrayin(j,i) = 0.
           arrayout(j,i) = 0.
         enddo
      enddo
c
c   --- produce table of input emissions by source category ---
c
      title = 'Criteria Pollutant Emissions by Stack'
      nlabel = 0
      lastcty = ' '
      lastfac = ' '
      laststk = ' '
      do i=1,nmsgpid
         kpid = idxsrt(i)
c
c  -- check if this is a new stack ---
c
         if( lastcty .NE. msgpid(kpid)(:5) .OR. 
     &          lastfac .NE. msgpid(kpid)(6:15) .OR. 
     &                           laststk .NE. msgpid(kpid)(16:25) ) then
            nlabel = nlabel + 1
            factmp = msgpid(kpid)(6:15)
            call rgtjst( factmp )
            staktmp = msgpid(kpid)(16:25)
            call rgtjst( staktmp )
            label(nlabel) = ' ' // msgpid(kpid)(:5) // '  '  // 
     &                                      factmp // '  ' // staktmp
            lastcty = msgpid(kpid)(:5)
            lastfac = msgpid(kpid)(6:15)
            laststk = msgpid(kpid)(16:25)
         endif
         do jspc=1,ncrthd
            arrayin(nlabel,jspc) = arrayin(nlabel,jspc) + 
     &                                           crtipid(kpid,jspc)
            arrayout(nlabel,jspc) = arrayout(nlabel,jspc) + 
     &                                           crtopid(kpid,jspc)
         enddo
      enddo
c
c   --- call routine to output table of input by ASC ---
c
      if( lcrit(IDXFMT) ) then
          call msgtbl( jerr,
     &                  iounit, title, units, MXLBL, nlabel,
     &                          36, label, codtit, ncrthd, header, 
     &                                      polnam, arrayin, arrayout )
         if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- if doing CSV tables, write table and close file ---
c
      if( lcrit(IDXCSV) ) then
         tmpnam = curofl
         fname = curofl(:strmin(curofl))//'.stack.crt.csv'
         icsvunit = 50+iounit
         open(unit=icsvunit,file=fname)
         curofl = fname
c
c  --- put comma between the identifiers ---
c
         do i=1,nlabel
           label(i)(7:7) = ','
           label(i)(19:19) = ','
         enddo
         codtit = 'FIPS,Facility,Stack'
         call msgcsv( jerr,
     &                      icsvunit, title, units, MXLBL, nlabel,
     &                         36, label, codtit, ncrthd, header,
     &                                      polnam, arrayin, arrayout )
         if( jerr .NE. ISUCES ) goto 9999
         close(icsvunit)
         curofl = tmpnam
c
c  --- put space back between the identifiers ---
c
         do i=1,nlabel
           label(i)(7:7) = ' '
           label(i)(19:19) = ' '
         enddo
         codtit = 'FIPS      Facility      Stack'
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
         do j=1,MXLBL
           arrayin(j,i) = 0.
           arrayout(j,i) = 0.
         enddo
      enddo
c
c   --- produce table of input emissions by source category ---
c
      title = 'Speciated Emissions by Stack'
      nlabel = 0
      lastcty = ' '
      lastfac = ' '
      laststk = ' '
      do i=1,nmsgpid
         kpid = idxsrt(i)
c
c  -- check if this is a new stack ---
c
         if( lastcty .NE. msgpid(kpid)(:5) .OR. 
     &          lastfac .NE. msgpid(kpid)(6:15) .OR. 
     &                           laststk .NE. msgpid(kpid)(16:25) ) then
            nlabel = nlabel + 1
            factmp = msgpid(kpid)(6:15)
            call rgtjst( factmp )
            staktmp = msgpid(kpid)(16:25)
            call rgtjst( staktmp )
            label(nlabel) = ' ' // msgpid(kpid)(:5) // '  '  // 
     &                                         factmp // '  ' // staktmp
            lastcty = msgpid(kpid)(:5)
            lastfac = msgpid(kpid)(6:15)
            laststk = msgpid(kpid)(16:25)
         endif
         do jspc=1,nspecs
            arrayin(nlabel,jspc) = arrayin(nlabel,jspc) + 
     &                                           spcipid(kpid,jspc)
            arrayout(nlabel,jspc) = arrayout(nlabel,jspc) + 
     &                                           spcopid(kpid,jspc)
         enddo
      enddo
      units = TONLBL
      units = units(:strmin(units))//'/Day'
c
c   --- call routine to output table of input by ASC ---
c
      if( lspec(IDXFMT) ) then
          call msgtbl( jerr,
     &                  iounit, title, units, MXLBL, nlabel,
     &                          36, label, codtit, nspecs, header, 
     &                                      polnam, arrayin, arrayout )
          if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- if doing CSV tables, write table and close file ---
c
      if( lspec(IDXCSV) ) then
         tmpnam = curofl
         fname = curofl(:strmin(curofl))//'.stack.spc.csv'
         icsvunit = 50+iounit
         open(unit=icsvunit,file=fname)
         curofl = fname
c
c  --- put comma between the identifiers ---
c
         do i=1,nlabel
           label(i)(7:7) = ','
           label(i)(19:19) = ','
         enddo
         codtit = 'FIPS,Facility,Stack'
         call msgcsv( jerr,
     &                      icsvunit, title, units, MXLBL, nlabel,
     &                          36, label, codtit, nspecs, header,
     &                                      polnam, arrayin, arrayout )
         if( jerr .NE. ISUCES ) goto 9999
         close(icsvunit)
         curofl = tmpnam
c
c  --- put space back between the identifiers ---
c
         do i=1,nlabel
           label(i)(7:7) = ' '
           label(i)(19:19) = ' '
         enddo
         codtit = 'FIPS      Facility      Stack'
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
