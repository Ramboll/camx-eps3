C**** WTLCHM.F 
c  
      subroutine wtlchm( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the tables of emissions totals for the CHMSPL program
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
      include 'uamcntl.inc'
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'chmspl.inc'
      include 'iochmspl.inc'
      include 'splits.inc'
      include 'totchm.inc'
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
c     wrctbl   --  writes a table with character labels
c     wrctbl8  --  writes a table with character labels
c                  (double precision version)
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
c    Local parameters:     
c-----------------------------------------------------------------------
c
c   MXLABL   I   maximum number of labels in output tables
c   MXVAL    I   maximum number of columns (species)
c        
      integer*4 MXLABL
      integer*4 MXVAL
c
      parameter( MXLABL = MXSPL )
      parameter( MXVAL  = MXCRIT + MXSPEC )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   codnam  C    character string array for title of procesed emissions
c   codtit  C    character string for title of codes
c   idxprf  I    array of indexes into sorted array of profile codes
c   jerr    I    error flag for subroutine calls
c   name    C    character string array of names of criteria pollutants
c   title   C    character string for title of tables
c   totcpf  R    array for county table by criteria pollutant
c   totspf  R    array for table of emissions by profile code
c   units   C    character string of units of emissions
c
      character*(15)     label2(MXLABL)
      character*80       title
      character*20       units, codtit, codnam(IDXERR)
      character*10       name(MXVAL)
      character*10       polnam(2,MXVAL)
      integer*4          idxprf(MXSPL), nlabel, jerr, kprf, i, j
      real*4             arrayin(MXLABL,MXCRIT), arrayout(MXLABL,MXCRIT)
      real*4             totspf(MXLABL,MXSPEC), tottmp
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
c   --- echo the number of records processed ---
c
      write(IOWMSG,'(/,A,I8)',ERR=7000) 
     &               ' Number of EMBR records processed: ',nrec(IDXIN)
      write(IOWMSG,'(/,A,I8)',ERR=7000) 
     &               ' Number of records written to EMAR: ',nrec(IDXERR)
c
c   --- set some labels ---
c
      title = 'Total Criteria Emissions Processed'
      units = TONLBL
      codtit = ' '
      codnam(IDXIN) = 'Input Emissions'
      codnam(IDXOUT) = 'Output Criteria'
      codnam(IDXSPC) = 'Output Speciated'
      codnam(IDXSKP) = 'Emissions Skipped'
      codnam(IDXERR) = 'Written to EMAR'
      do i=1,ncrthd
         name(i) = crtnam(i)
         polnam(1,i) = crtnam(i)
         polnam(2,i) = crtnam(i)
      enddo
      do i=1,MXCRIT+MXSPEC
         header(1,i) = 'Input'
         header(2,i) = 'Speciated'
      enddo
c
c   --- write message to explain how VOC is reported ---
c
      write(IOWMSG,'(//)',ERR=7000)
      write(IOWMSG,9000,ERR=7000)
     &'Note to the user:  The criteria pollutant emissions are ',
     &'                   reported as the sum of all contributing ',
     &'                   speciated emissions.',' ',
     &'                   The speciated emissions are reported as ',
     &'                   methane equivalent.  Therefore the output ',
     &'                   criteria emissions may not match the input',
     &'                   criteria emissions.'
c
c   --- call routine to output table of processed emissions ---
c
      call wrctbl8( jerr,
     &               IOWMSG, title, units, IDXERR, IDXERR, 20,
     &                   codnam, codtit, ncrthd, name, totems, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   ---  skip this section if not doing a table by profile code ---
c
      if( .NOT. lprtpf .OR. nfac .LE. 0) goto 111
c
c   --- call routine to sort the profile codes ---
c
      call chrsrt(prfspl,LENPRF,nfac,idxprf)
c
c   ---  input emissions by and criteria pollutant --
c
      title = 'Emissions by Profile Code'
      codtit = 'Profile'
      nlabel = 1
      do i=1,nfac
         kprf = idxprf(i)
         tottmp = 0
         do j=1,ncrthd
            if( totpfc(IDXIN,kprf,j) .GT. 0. ) then
               tottmp = tottmp + totpfc(IDXIN,kprf,j)
               arrayin(nlabel,j) = totpfc(IDXIN,kprf,j)
               arrayout(nlabel,j) = totpfc(IDXOUT,kprf,j)
            endif
         enddo
         do j=1,nspecs
            totspf(nlabel,j) = totpfs(kprf,j)
         enddo
         if( tottmp .GT. 0. ) then
             label2(nlabel) = ' '
             label2(nlabel) = prfspl(kprf)
             nlabel = nlabel + 1
         endif
      enddo
      nlabel = nlabel - 1
      call msgtbl( jerr,
     &                  IOWMSG, title, units, MXLABL, nlabel,
     &                          13, label2, codtit, ncrthd, header,
     &                                      polnam, arrayin, arrayout )
       if( jerr .NE. ISUCES ) goto 9999
c
c  --- now output speciated emssions by profile code ---
c
      title = 'Output Speciated Emissions by Profile Code '
      call wrctbl( jerr,
     &                  IOWMSG, title, units, MXLABL, nlabel,
     &             13, label2, codtit, nspecs, spclst, totspf, .TRUE. )
c
c   --- if doing county tables call the routine ---
c
 111  continue
      call wrtcty( jerr, IOWMSG, lcrtcty, lspccty )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing source category tables call the routine ---
c
      call wrtasc( jerr, IOWMSG, lcrtasc, lspcasc )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if doing county/source table call the routine ---
c
      call wrtfcd( jerr, IOWMSG, lcrtfcd, lspcfcd )
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
 9000 format(10X,61('-'),8(/,10X,A),/,10X,61('-'))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
