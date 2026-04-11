C**** WRTSRG.F 
c
      subroutine wrtsrg( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the optional output surrogate report for the GRDEM program
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
      include 'iogrdem.inc'
      include 'embrdat.inc'
      include 'grdsrgrpt.inc'
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
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   MXLBL   I    maximum number of labels
c   MXVAL   I    maximum number of pollutant values
c
      integer*4 MXLBL
      integer*4 MXVAL
c
      parameter( MXLBL = MXFSSF )
      parameter( MXVAL = MXCRIT )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) fname, tmpnam
      character*80      title
      character*20      units
      character*20      codtit
      character*10      polnam(2,MXVAL)
      character*10      header(2,MXVAL)
      character*24      label(MXLBL)
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
c   --- call routine to sort labels FIPS/SCC/SrgCd/Flg codes ---
c
      if( nfssf .LE. 0 ) goto 222
      codtit = 'FIPS  SCC    Srg Flg'
      units = TONLBL
      units = units(:strmin(units))//'/Day'
c
c   --- set up the headers and pollutant names ---
c
      do i=1,MXVAL
         header(1,i) = 'Input'
         header(2,i) = 'Output'
      enddo
      do i=1,ncrthd
         polnam(1,i) = crtnam(i)
         polnam(2,i) = crtnam(i)
      enddo
c
c   --- produce table of input emissions by source category ---
c
      title = 'Criteria Pollutant Emissions by FIPS, SCC, '
     &         // 'Surrogate Code, and Surrogate Flag'
      nlabel = 1
      do 10 i=1,nfssf
c         kasc = idxsrt(i)
         kasc = i
         tottmp = 0.
         do jspc=1,ncrthd
            arrayin(nlabel,jspc) = emsifssf(kasc,jspc)
            arrayout(nlabel,jspc) = emsofssf(kasc,jspc)
            tottmp = tottmp + emsifssf(kasc,jspc) + emsofssf(kasc,jspc)
         enddo
         if( tottmp .LE. 0. ) goto 10
         write(label(nlabel),'(2X,A)') fssflst(kasc)
         nlabel = nlabel + 1
   10 continue
      nlabel = nlabel - 1
c
c   --- call routine to output table of input by ASC ---
c
      if ( lscsv )  then
         call msgcsv( jerr,
     &                  IOWSRG, title, units, MXLBL, nlabel,
     &                          24, label, codtit, ncrthd, header, 
     &                                      polnam, arrayin, arrayout )
         if( jerr .NE. ISUCES ) goto 9999
      else
         call msgtbl( jerr,
     &                  IOWSRG, title, units, MXLBL, nlabel,
     &                          24, label, codtit, ncrthd, header, 
     &                                      polnam, arrayin, arrayout )
         if( jerr .NE. ISUCES ) goto 9999
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
