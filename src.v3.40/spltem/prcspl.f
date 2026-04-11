C**** PRCSPL.F
c
      subroutine prcspl( ierr )
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the SPLTEM module.  One compressed
c    record is read from an EMBR file.  Verify record is gridded and
c    hourly.  Verify cell is within region of interest. 
c    Find speciation profile and apply weight fractions to get
c    emissions for each chemical compound.
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
c      08/11/97  --mmj--  original development
c      01/15/08  --mmj--  EPS3 compatible
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iospltem.inc'
      include 'embrdat.inc'
      include 'sccxref.inc'
      include 'spltem.inc'
      include 'wgtfrac.inc'
      include 'spltemtot.inc'
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
c     rdcomp  --  reads one compressed EMBR data record (library)
c     totspl  --  stores the total emissions processed
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   fndchr   I   returns the index of a string in an array of strings
c   fndint   I   returns the index of an integer in an integer array
c   fndxrf   I   cross-reference lookup for point sources (library)
c   fndxasc  I   cross-reference lookup for area sources (library)
c   strmin   I   returns the actual length of a string (min of 1)
c
      integer*4 fndchr
      integer*4 fndint
      integer*4 fndxrf
      integer*4 fndxasc
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   emiss   R   computed emissions for chemical species
c   emprf   R   emissions for split, for output by profile
c   icell   I   input EMBR record cell number in X direction
c   jcell   I   input EMBR record cell number in Y direction
c   idate   I   input EMBR record beginning inventory year/day
c   jdate   I   input EMBR record ending inventory year/day
c   ihr     I   input EMBR record beginning inventory hour
c   jhr     I   input EMBR record ending inventory hour
c   nhrs    I   number of hours per EMBR record
c   idxcel  I   index to cell number of output array
c   idxprf  I   index of profile code in weight fraction file
c   idxscc  I   index of ASC-SCC code in cross reference array
c   idxsrd  I   index of saroad code in list of codes
c   irec    I   number of last EMBR record read
c   jerr    I   error flag for subroutine calls
c   prfcd   C   referenced profile code for input EMBR record
c
      character*(LENPRF) prfcd
      integer*4          icell, jcell
      integer*4          idate, jdate, ihr, jhr, nhrs
      integer*4          idxcel, idxprf, idxscc, idxsrd, isrdtmp
      integer*4          irec, jerr, i, np, nc
      real*4             emiss, emprf(MXCRIT)
c
c-----------------------------------------------------------------------
c   Data Statements:
c-----------------------------------------------------------------------
c
      data irec  /0/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- add saroad codes from embr header to the full saroad list ---
c
      do i = 1, ncrthd
         if( .NOT.lsplit(i) )  then
c           --- if saroad code is zero then set to negative i; for lbase ---
            if (isarod(i) .EQ. 0) isarod(i) = -i
            idxsrd = fndint ( isarod(i), isrdcnt, isrdlst )
            if ( idxsrd .LE. 0 )  then
               isrdcnt = isrdcnt + 1
               if ( isrdcnt .GT. MXSRD )  goto 7003
               isrdlst(isrdcnt) = isarod(i)
               srdnam(isrdcnt) = crtnam(i)
               srdlng(isrdcnt) = crtnam(i)
               lreact(isrdcnt) = .FALSE.
            else
               srdnam(idxsrd) = crtnam(i)
               srdlng(idxsrd) = crtnam(i)
               lreact(isrdcnt) = .FALSE.
            endif
         endif
      enddo
c
c   --- read a compressed EMBR record ---
c
  111 continue
      irec = irec + 1
      call rdcomp(jerr,
     &                 IORMBR, irec )
      if( jerr .EQ. IEOD ) goto 222
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- extract grid cell location ---
c
      if( ijcell .GT. 0 ) then
         icell = int( ijcell/1000 )
         jcell = mod( ijcell, 1000 )
      else
         goto 7001
      endif
c
c   --- extract hours of day ---
c
      idate = INT (invbeg/100)
      jdate = INT (invend/100)
      ihr   = MOD (invbeg,100)
      jhr   = MOD (invend,100)
      if (  jhr .EQ. 0 )  jhr = 24
      if ( jhr - ihr .GT. 24 )  goto 7002
      nhrs = jhr - ihr
c
c   ---  store the input emissions ---
c
      call totspl ( IDXIN, 0, emin )
c
c   --- determine if cell is within region of interest, and cell index ---
c
      idxcel = fndint ( ijcell, ncells, ijlst )
      if ( idxcel .LE. 0 )  then
         call totspl ( IDXRNG, 0, emin )
         goto 111
      endif

c     --- for each criteria pollutant ---
c     --- determine chemical splits and store in output arrays ---
      do nc = 1, ncrthd

         if( emin(nc) .GT. 0. )  then

c           --- check if need to split ---
            if( lsplit(nc) ) then
c
c               --- look for ASC-SCC in cross-reference info ---
c               --- if not found, write message and skip ---
c
                if( rectyp .EQ. 'E' .OR. rectyp .EQ. 'L' ) then
                    idxscc = fndxrf ( fipin,plant,stkid,point,scccd )
                else
                    idxscc = fndxasc ( fipin, scccd )
                endif
                if(idxscc .LE. 0) goto 6000
c
c               --- call routine to find index in weight fractions file ---
c               --- if not found, write message and skip ---
c
                prfcd = prfxrf(idxscc)
                idxprf = fndchr ( prfcd, LENPRF, profwf, nprfwf)
                if ( idxprf .LE. 0 )  goto 6001
                emprf(1) = emin(nc)
c
                do 200 np = 1, nwtswf(idxprf)
                  emiss =  ( emin(nc)*CVTGRM*wgtswf(idxprf,np) ) / nhrs
                  isrdtmp = isrdwf(idxprf,np)
                  idxsrd = fndint (isrdtmp, isrdcnt, isrdlst)
                  if ( idxsrd .LE. 0 )  goto 7004
                  do 100 i = ihr, jhr - 1
                    if ( rectyp .NE. 'E' )  then
                        emslow(idxcel,i,idxsrd) 
     &                              = emslow(idxcel,i,idxsrd) + emiss
                    else
                        if ( lelev ) 
     &                  emsele(idxcel,i,idxsrd) 
     &                              = emsele(idxcel,i,idxsrd) + emiss
                    endif
  100             continue
  200           continue
                if ( rectyp .NE. 'E' )  then
                   call totspl ( IDXOUT, idxprf, emprf )
                else
                   if ( lelev )  then
                      call totspl ( IDXOUT, idxprf, emprf )
                   endif
                endif
c
c           --- criteria only (no splitting) --- ============================
            else
                isrdtmp = isarod(nc)
                idxsrd = fndint (isrdtmp, isrdcnt, isrdlst)
                if ( idxsrd .LE. 0 )  goto 7004
                do 300 i = ihr, jhr - 1
                   if ( rectyp .NE. 'E' )  then
                      emslow(idxcel,i,idxsrd) = emslow(idxcel,i,idxsrd) 
     &                                      + (emin(nc) * CVTGRM)/nhrs
                   else
                      if ( lelev ) 
     &                emsele(idxcel,i,idxsrd) = emsele(idxcel,i,idxsrd) 
     &                                     + (emin(nc) * CVTGRM)/nhrs
                   endif
  300           continue
c           --- end of check for split/criteria only ---
            endif

c        --- end of nonzero emissions() ---
         endif
c     --- end of criteria list ---
      enddo
c
c   --- sum totals for output message file ---
c
      if ( rectyp .NE. 'E' )  then
         call totspl ( IDXDAT, 0, emin )
         call totspl ( IDXOUT, 0, emin )
      else
         call totspl ( IDXELE, 0, emin )
         if  (lelev )  then
            call totspl ( IDXOUT, 0, emin )
         endif
      endif
c
c  --- get next record ---
c
      goto 111
c
c  --- finished processing records, write numbers to message file ---
c
  222 continue
      ierr = ISUCES
      write(IOWMSG,9002,ERR=7000)
     &                   'Number of input records read: ', nrec(IDXIN)
      write(IOWMSG,9002,ERR=7000)
     & 'Number of input records outside region of interest: ', 
     &                                                     nrec(IDXRNG)
      write(IOWMSG,9002,ERR=7000)
     &               'Number of lowlevel input records: ', nrec(IDXDAT)
      write(IOWMSG,9002,ERR=7000)
     &               'Number of elevated input records: ', nrec(IDXELE)
      write(IOWMSG,9002,ERR=7000)
     &                'Number of skipped input records: ', nrec(IDXSKP)
      write(IOWMSG,9002,ERR=7000)
     &      'Number of input records written for output: ',nrec(IDXOUT)
c
c  --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning Messages:
c-----------------------------------------------------------------------
c
 6000 continue
      write(IOWSTD,9000,ERR=9999) 'WARNING: ASC-SCC code: ',scccd,
     &    ' from EMBR file not found in',' XREF file.  Skipping record.'
      write(IOWMSG,9000,ERR=7000) 'WARNING: ASC-SCC code: ',scccd,
     &    ' from EMBR file not found in',' XREF file.  Skipping record.'
      call totspl(
     &             IDXSKP, 0, emin )
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6001 continue
      write(IOWSTD,9000,ERR=9999) 
     &      'WARNING: Weight fractions for profile: ', prfcd, 
     &               ' not found in weights file.  Skipping record.'
      write(IOWMSG,9000,ERR=7000) 
     &      'WARNING: Weight fractions for profile: ', prfcd, 
     &               ' not found in weights file.  Skipping record.'
      call totspl(
     &             IDXSKP, 0, emin )
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR:  Writing message file: ',
     &                                     msgfl(:strmin(msgfl))
      goto 9999
c
 7001 continue
      write (IOWSTD,9001,ERR=9999) 'ERROR:  Compressed EMBR record ',
     &                             'not gridded.  irec = ', irec
      write (IOWMSG,9001,ERR=7000) 'ERROR:  Compressed EMBR record ',
     &                             'not gridded.  irec = ', irec
      goto 9999
c
 7002 continue
      write (IOWSTD,9001,ERR=9999) 'ERROR:  Compressed EMBR record ',
     &                      'spans a single day.  irec = ', irec,
     &                    'start - ', invbeg, ' end - ', invend
      write (IOWSTD,'(A,A)',ERR=9999) 'QA - fip ', fipin
      write (IOWSTD,'(A,A)',ERR=9999) 'QA - sic ', siccd
      write (IOWSTD,'(A,A)',ERR=9999) 'QA - scc ', scccd
      write (IOWSTD,'(A,A)',ERR=9999) 'QA - rectype ', rectyp
      write (IOWMSG,9001,ERR=7000) 'ERROR:  Compressed EMBR record ',
     &                      'spans a single day.  irec = ', irec
      goto 9999
c
 7003 continue
      write(IOWSTD,9003,ERR=9999) 'ERROR: Exceed maximum number of ', 
     &                           'SAROAD codes.  MXSRD = ', MXSRD
      write(IOWMSG,9003,ERR=7000) 'ERROR: Exceed maximum number of ',
     &                           'SAROAD codes.  MXSRD = ', MXSRD
      goto 9999
c
 7004 continue
      write(IOWSTD,9003,ERR=9999) 'ERROR: SAROAD code not found ',
     &                           'in lookup table. ', isrdtmp
      write(IOWMSG,9003,ERR=7000)  'ERROR: SAROAD code not found ',
     &                           'in lookup table. ', isrdtmp
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(1X,A,A,A,A)
 9001 format(1X,A,A,I8,/A,I8,A,I8/)
 9002 format(/,T10,A,I8)
 9003 format(1X,A,A,I5)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
