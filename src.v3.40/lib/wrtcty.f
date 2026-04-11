C**** WRTCTY.F 
c
      subroutine wrtcty( ierr, 
     &                         iounit, lcrit, lspec )
c
c-----------------------------------------------------------------------
c
c    Writes the table of emissions totals by county.
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
c   strmin   I   returns the actual length of a string
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
      parameter( MXLBL = MXFIP+1 )
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
      character*8       label(MXLBL)
      integer*4         icsvunit
      integer*4         idxsrt(MXLBL), nlabel, jerr, ifp, jspc, i 	
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
c   --- call routine to sort county codes ---
c
      if( ncdfip .LE. 0 ) goto 222
      call chrsrt(fipcd,5,ncdfip,idxsrt)
      codtit = 'County'
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
c   --- write out counties with no emissions ---
c
      title = 'Criteria Pollutant Emissions by County'
      nlabel = 1
      write(iounit,'(A)',ERR=7000)
      do i=1,ncdfip
         ifp = idxsrt(i)
         tottmp = 0.
         do jspc=1,ncrthd
            tottmp = tottmp + crticty(ifp,jspc) + crtocty(ifp,jspc)
            arrayin(nlabel,jspc) = crticty(ifp,jspc)
            arrayout(nlabel,jspc) = crtocty(ifp,jspc)
         enddo
         if( tottmp .LE. 0. ) then
             write(iounit,'(T10,3A)',ERR=7000) 'County ',fipcd(ifp),
     &                         ' contained no emissions in input file.'
         else
            write(label(nlabel),'(2X,A)') fipcd(ifp)
            nlabel = nlabel + 1
         endif
      enddo
c
c  --- add the "skipped county" if needed ---
c
      tottmp = 0.
      do jspc=1,ncrthd
         tottmp = tottmp + crticty(MXFIP+1,jspc) + crtocty(MXFIP+1,jspc)
         arrayin(nlabel,jspc) = crticty(MXFIP+1,jspc)
         arrayout(nlabel,jspc) = crtocty(MXFIP+1,jspc)
      enddo
      if( tottmp .GT. 0. ) then
         write(label(nlabel),'(2X,A)') 'Skip'
         nlabel = nlabel + 1
      endif
      nlabel = nlabel - 1
c
c   --- call routine to output table of input by county ---
c
      if( lcrit(IDXFMT) ) then
         call msgtbl( jerr,
     &                  iounit, title, units, MXLBL, nlabel,
     &                          8, label, codtit, ncrthd, header,
     &                                      polnam, arrayin, arrayout )
         if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- if doing CSV tables, write table and close file ---
c
      if( lcrit(IDXCSV) ) then
         tmpnam = curofl
         fname = curofl(:strmin(curofl))//'.cnty.crt.csv'
         icsvunit = 50+iounit
         open(unit=icsvunit,file=fname)
         curofl = fname	
         call msgcsv( jerr,
     &                      icsvunit, title, units, MXLBL, nlabel,
     &                          8, label, codtit, ncrthd, header,
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
c   --- write out counties with no emissions ---
c
      title = 'Speciated Emissions by County'
      nlabel = 1
      write(iounit,'(A)',ERR=7000)
      do i=1,ncdfip
         ifp = idxsrt(i)
         tottmp = 0.
         do jspc=1,nspecs
            tottmp = tottmp + spcicty(ifp,jspc) + spcocty(ifp,jspc)
            arrayin(nlabel,jspc) = spcicty(ifp,jspc)
            arrayout(nlabel,jspc) = spcocty(ifp,jspc)
         enddo
         if( tottmp .GT. 0. ) then
            write(label(nlabel),'(2X,A)') fipcd(ifp)
            nlabel = nlabel + 1
         endif
      enddo
c
c  --- add the "skipped county" if needed ---
c
      tottmp = 0.
      do jspc=1,nspecs
         tottmp = tottmp + spcicty(MXFIP+1,jspc) + spcocty(MXFIP+1,jspc)
         arrayin(nlabel,jspc) = spcicty(MXFIP+1,jspc)
         arrayout(nlabel,jspc) = spcocty(MXFIP+1,jspc)
      enddo
      if( tottmp .GT. 0. ) then
         write(label(nlabel),'(2X,A)') 'Skip'
         nlabel = nlabel + 1
      endif
      nlabel = nlabel - 1
      units = TONLBL
      units = units(:strmin(units))//'/Day'
c
c   --- call routine to output table of input by county ---
c
      if( lspec(IDXFMT) ) then
          call msgtbl( jerr,
     &                  iounit, title, units, MXLBL, nlabel,
     &                          8, label, codtit, nspecs, header, 
     &                                      polnam, arrayin, arrayout )
          if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- if doing CSV tables, write table and close file ---
c
      if( lspec(IDXCSV) ) then
         tmpnam = curofl
         fname = curofl(:strmin(curofl))//'.cnty.spc.csv'
         icsvunit = 50+iounit
         open(unit=icsvunit,file=fname)
         curofl = fname
         call msgcsv( jerr,
     &                      icsvunit, title, units, MXLBL, nlabel,
     &                          8, label, codtit, nspecs, header,
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
 7000 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 
     &          'ERROR:  Writing message file generating output tables.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curofl(:strmin(curofl))
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
