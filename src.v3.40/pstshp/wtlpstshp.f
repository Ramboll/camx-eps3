C**** WTLPSTSHP.F
c
      subroutine wtlpstshp( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the tables of emissions totals for the PSTSHP program
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
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'cntylst.inc'
      include 'msgtot.inc'
      include 'embrdat.inc'
      include 'layers.inc'
      include 'iopstshp.inc'
      include 'pstshptot.inc'
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
c   MXVAL   I    maximum number of pollutant values
c   MXLBL   I    maximum number of labels
c
      integer*4 MXVAL
      integer*4 MXLBL
c
      parameter( MXVAL = MXCRIT+MXSPEC)
      parameter( MXLBL = MXLAY*MXASC  )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*80 title
      character*30 codtit, labelc(MXLBL)
      character*20 units, codnam(IDXSKP)
      character*10 name(MXVAL)
      integer*4    idxsrt(MXASC), jerr, kasc, i, j, k
      integer*4    nlabel, label2(MXLBL)
      real*4       tottmp, tot(MXLBL,MXCRIT)
      logical*4    lfalse(IDXCSV)
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- initialize error code ---
c
      ierr = IFAIL
      lfalse(IDXFMT) = .FALSE.
      lfalse(IDXCSV) = .FALSE.
      curofl = msgfl
c
c  --- set some labels ---
c
      title = 'Total Emissions Processed'
      units = TONLBL
      units = units(:strmin(units))//'/Day'
      codtit = ' '
      codnam(IDXIN) = 'Input Emissions'
      codnam(IDXOUT) = 'Output Emissions'
      codnam(IDXELV) = 'Elevated Emissions'
      codnam(IDXSRF) = 'Surface Emissions'
      codnam(IDXREG) = 'Outside Region'
      codnam(IDXERR) = 'Written to ERR file'
      codnam(IDXSKP) = 'Total Skipped'
c
c  --- set up the headers and pollutant names ---
c
      do i=1,ncrthd
         name(i) = crtnam(i)
      enddo
      do i=1,MXCRIT+MXSPEC
         header(1,i) = 'Input'
         header(2,i) = 'Output'
      enddo
c
c  --- call routine to output table of processed emissions ---
c
      call wrctbl8( jerr,
     &                 IOWMSG, title, units, IDXSKP, IDXSKP, 20,
     &                  codnam, codtit, ncrthd, name, totems, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- if the species list is non-empty create table of speciated
c      emissions totals ---
c
      title = 'Total Speciated Emissions Processed'
      if( nspchd .GT. 0 ) call wrctbl8( jerr,
     &                IOWMSG, title, units, IDXSKP, IDXSKP, 20,
     &                codnam, codtit, nspchd, spclst, totspc, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- write criteria emissions by vertical layer ---
c
c     title = 'Output Criteria Emissions by Vertical Layer'
c     codtit = 'Vertical Layer'
c     nlabel = 1
c     do i=0,nlayer
c        tottmp = 0
c        do j=1,ncrthd
c           if( crtlay(j,i) .GT. 0. ) then
c              tottmp = tottmp + crtlay(j,i)
c              tot(nlabel,j) = crtlay(j,i)
c           endif
c        enddo
c        if( tottmp .GT. 0. ) then
c            label2(nlabel) = i
c            if ( i.EQ.0 ) labelc(nlabel) = 'Surface'
c            if ( i.GT.0 ) 
c    &            write(labelc(nlabel),9000,ERR=9999) i, aglht(i)
c            nlabel = nlabel + 1
c        endif
c     enddo
c     nlabel = nlabel - 1
c     if( .NOT. lelvfl ) nlabel = 1
c
c     call wrctbl( jerr,
c    &               IOWMSG, title, units, MXLBL, nlabel,
c    &           20, labelc, codtit, ncrthd, crtnam, tot, .TRUE. )
c     if( jerr .NE. ISUCES ) goto 9999
c
c  --- write criteria emissions by vertical layer ---
c
c     if( lcrtasc(IDXFMT) ) then
c         call chrsrt(msgasc,10,nmsgasc,idxsrt)
c         title = 'Output Criteria Emissions by Vertical '//
c    &                                 'Layer and Source Category'
c         codtit = 'ASC        Vertical Layer'
c         nlabel = 1
c         do k=1,nmsgasc
c            kasc = idxsrt(k)
c            do i=0,nlayer
c               tottmp = 0
c               do j=1,ncrthd
c                  if( asclay(kasc,j,i) .GT. 0. ) then
c                     tottmp = tottmp + asclay(kasc,j,i)
c                     tot(nlabel,j) = asclay(kasc,j,i)
c                  endif
c               enddo
c               if( tottmp .GT. 0. ) then
c                   label2(nlabel) = i
c                   if ( i.EQ.0 ) labelc(nlabel) = 
c    &                                       msgasc(kasc)//'   Surface'
c                   if ( i.GT.0 ) write(labelc(nlabel),9001) 
c    &                                         msgasc(kasc),i, aglht(i)
c                   nlabel = nlabel + 1
c               endif
c            enddo
c         enddo
c         nlabel = nlabel - 1
c         if( .NOT. lelvfl ) nlabel = 1
c
c         call wrctbl( jerr,
c    &                   IOWMSG, title, units, MXLBL, nlabel,
c    &               30, labelc, codtit, ncrthd, crtnam, tot, .TRUE. )
c         if( jerr .NE. ISUCES ) goto 9999
c     endif
c
c  --- calls to standard message file output tables,
c      if doing county tables call the routine
c
      call wrtcty( jerr, IOWMSG, lcrtcty, lspccty )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- if doing source category tables call the routine ---
c
      call wrtasc( jerr, IOWMSG, lcrtasc, lspcasc )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- if doing table by facilty, call the routine ---
c
      call wrtfac( jerr, IOWMSG, lcrtfac, lspcfac )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- if doing table by stack, call the routine ---
c
      call wrtstak( jerr, IOWMSG, lcrtstk, lspcstk )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- if doing table by stack, call the routine ---
c
      call wrtpnt( jerr, IOWMSG, lcrtpnt, lspcpnt )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- if doing county/source table call the routine ---
c
      call wrtfcd( jerr, IOWMSG, lcrtfcd, lspcfcd )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- if doing table by hours call routine ---
c
      call wrthrs( jerr, IOWMSG, lcrthr, lspchr )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- if doing hourly tables by ASC call the routine ---
c
      call wrthcd( jerr, IOWMSG, lcrthcd, lspchcd )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- set error flag to success ---
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
 9000 format (I3,2X,F8.1)
 9001 format (A10,2X,I3,2X,F8.1)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
