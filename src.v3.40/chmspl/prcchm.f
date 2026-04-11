C**** PRCCHM.F 
c   
      subroutine prcchm( ierr )
c
c-----------------------------------------------------------------------
c
c    Read the input EMBR file.
c    Apply the split factors or use defaults.
c    Write the output EMBR file.
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c
c         1. gwilson      Tue Apr 26 10:56:37 PDT 2005
c            Added point ID to speciation lookup table
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
      include 'cntylst.inc'
      include 'uamcntl.inc'
      include 'sccxref.inc'
      include 'msgtot.inc'
      include 'iochmspl.inc'
      include 'splits.inc'
      include 'chmspl.inc'
      include 'totchm.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Functions called:
c-----------------------------------------------------------------------
c
c   fndhash_fips I  finds index of FIPS code in hash table (library)
c   fndxrf       I  cross-reference lookup for point sources (library)
c   fndxasc      I  cross-reference lookup for area sources (library)
c   fndchr       I  find a string in an array (library)
c
      integer*4 fndhash_fips
      integer*4 fndxrf
      integer*4 fndxasc
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c    rdembr  -- read one EMBR /DATA/ record (library)
c    wrechm  -- write error records (when SCC is not in cross ref)
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c     idxprf I   index of profile code in splits list
c     idxscc I   position of SCC in cross-reference file
c     idxfip I   index of FIPS code
c     jerr   I   error code for subroutine calls
c
      integer*4    jerr, idxscc, idxfip, idxprf, irec, icrt
      integer*4    i
      real*4       emstmp, emstot(MXCRIT)
      logical*4    lneedxrf, l6000, l6001, l6002, lwarn
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
      l6000 = .FALSE.
      l6001 = .FALSE.
      l6002 = .FALSE.
      lwarn = .FALSE.
c
c   --- read a record from the input EMBR file ---
c
  111 continue
      nspchd = 0
      call rdembr(jerr,IOREMB,irec)
      if(jerr .EQ. IEOD) goto 222
      if(jerr .NE. ISUCES) goto 9999
      irec = irec + 1
c
c   --- check to see if need xref file for this record ---
c
      lneedxrf = .FALSE.
      do i=1,ncrthd
        if( emin(i) .GT. 0. .AND. lsplcrt(i) ) lneedxrf = .TRUE.
        emstot(i) = 0.
      enddo

c   --- check for county in region ---
c
      idxfip = fndhash_fips ( fipin )
      if(idxfip .LE. 0) goto 6000
c
c   --- look for ASC-SCC in cross-reference info
c       if not found, write to error ----
c
      if( lneedxrf ) then
         if( rectyp .EQ. 'E' .OR. rectyp .EQ. 'L' ) then
             idxscc = fndxrf ( fipin, plant, stkid, point, scccd )
         else
             idxscc = fndxasc ( fipin, scccd )
         endif
         if(idxscc .LE. 0) goto 6001
c
c   --- look for profile code in splits file
c       if missing and if "all profile codes must match" flag
c       is turned of write to error, else apply global profile ---
c
         idxprf = fndchr ( prfxrf(idxscc),LENPRF,prfspl,nfac )
         if(idxprf .EQ. 0) then
             if( (.NOT. lreqpf) .AND. idefsp .GT. 0 .AND.
     &                                            .NOT. lwarn ) then
                 write(ioerr,'(2A,/,A)',ERR=9999) 'WARNING:  EMBR ',
     &              'file contains ASC/SCC codes that are mapped to ',
     &                                  'nonexistent profile codes.'
                 write(ioerr,'(10X,2A)',ERR=9999) 'ASC/SCC code: ',scccd
                 write(ioerr,'(10X,2A)',ERR=9999) 'Profile code: ',
     &                                                   prfxrf(idxscc)
                 write(ioerr,'(5X,2A)',ERR=9999) 'The default profile ',
     &                              'will be used for all such records.'
                 write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
                 idxprf = idefsp
                 lwarn = .TRUE.
                 nwarn = nwarn + 1
             else
                 goto 6002
             endif
         endif
      else
         idxprf = nfac
      endif
c
c  --- set output criteria pollutant values to missing ---
c
      do i=1,MXCRIT
         emout(i) = RMISS
      enddo
      do i=1,nspecs
         spcout(i) = RMISS
      enddo
c
c  --- add input emissions to arrays ---
c
      call totchm(IDXIN,emin,emin,spcpol,idxprf)
      do i=1,ncrthd
        call addtotcty( jerr,ICDIN,idxfip,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotasc( jerr,ICDIN,scccd,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotfcd( jerr,ICDIN,idxfip,scccd,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothrs( jerr,invbeg,invend,ICDIN,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotpid( jerr,ICDIN,idxfip,plant,stkid,
     &                                        point,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothcd( jerr,scccd,invbeg,invend,ICDIN,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
      enddo
c      
c  --- output criteria emissions is not changed from input ---
c
      do i=1,ncrthd
         emout(i) = emin(i)
      enddo
c
c  --- Apply split factors ---
c
      emstmp = 0.
      do 10 i=1,nspecs   
          icrt = ispcrt(i)
c
c  --- if criteria emissions is missing, skip this species ----
c
          if( icrt .LE. 0 .OR. emin(icrt) .LE. 0. ) goto 10
c
c  --- if species is in splits file, use the data ---
c

          if( indspl(i) .GT. 0 ) then
            spcout(i) = (emin(icrt) / CVTTON) * 
     &                                  spltab(idxprf,indspl(i))
          else if( idflst(i) .GT. 0 ) then
            spcout(i) = (emin(icrt) / CVTTON) * dftspl(idflst(i))
          endif

          emstmp = spcout(i) * CVTTON * molwt(i)
          emstot(icrt) = emstot(icrt) + emstmp
c
c   ---- call routines to load criteria output tables as 
c        sum of speciated ---
c
          call addtotcty( jerr,ICDOUT,idxfip,icrt,0,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
c
          call addtotasc( jerr,ICDOUT,scccd,icrt,0,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
c
          call addtotfcd( jerr,ICDOUT,idxfip,scccd,icrt,0,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
c
          call addtothrs( jerr,invbeg,invend,ICDOUT,icrt,0,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
c
          call addtotpid( jerr,ICDOUT,idxfip,plant,stkid,
     &                                        point,icrt,0,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
c
          call addtothcd( jerr,scccd,invbeg,invend,ICDOUT,
     &                                             icrt,0,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
c
c   ---- call routines to load output tables for speciated ---
c
          call addtotcty( jerr,ICDOUT,idxfip,0,i,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
c
          call addtotasc( jerr,ICDOUT,scccd,0,i,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
c
          call addtotfcd( jerr,ICDOUT,idxfip,scccd,0,i,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
c
          call addtothrs( jerr,invbeg,invend,ICDOUT,0,i,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
c
          call addtotpid( jerr,ICDOUT,idxfip,plant,stkid,
     &                                        point,0,i,emstmp )
        if( jerr .NE. ISUCES ) goto 9999
c
          call addtothcd( jerr,scccd,invbeg,invend,ICDOUT,0,i,emstmp )
          if( jerr .NE. ISUCES ) goto 9999
   10 continue
c
c  --- add emissions to totals ---
c
      call totchm(IDXOUT,emout,emstot,spcout,idxprf)
      call totchm(IDXSPC,emout,emstot,spcout,idxprf)
c
c  --- write EMBR record ----
c
      nspchd = nspecs
      call wrembr(jerr, IOWEMB, irec)
      if(jerr .NE. ISUCES) goto 9999
c
c  --- read next EMBR record ---
c
      goto 111
c
c  --- success ---
c
  222 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning Messages:
c-----------------------------------------------------------------------
c
 6000 continue
      if( .NOT. l6000 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                           'counties that are not in the USERIN.'
         write(ioerr,'(10X,2A)',ERR=9999) 'Unlisted FIPS: ',fipin
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ', 
     &                                    'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6000 = .TRUE.
      call totchm(IDXIN,emin,emin,spcpol,0)
      call totchm(IDXSKP,emin,emin,spcpol,0)
c
      do i=1,ncrthd
        call addtotcty( jerr,ICDIN,MXFIP+1,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotasc( jerr,ICDIN,scccd,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotfcd( jerr,ICDIN,MXFIP+1,scccd,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothrs( jerr,invbeg,invend,ICDIN,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothcd( jerr,scccd,invbeg,invend,ICDIN,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
      enddo
      goto 111
c
 6001 continue
      if( .NOT. l6001 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &         'ASC/SCC codes that are not in the speciation XREF file.'
         write(ioerr,'(10X,A,A10)',ERR=9999) 'ASC/SCC code: ',scccd
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be ',
     &                                    'written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6001 = .TRUE.
      call totchm(IDXIN,emin,emin,spcpol,0)
      call totchm(IDXERR,emin,emin,spcpol,0)
c
      do i=1,ncrthd
        call addtotcty( jerr,ICDIN,idxfip,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotasc( jerr,ICDIN,scccd,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotfcd( jerr,ICDIN,idxfip,scccd,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothrs( jerr,invbeg,invend,ICDIN,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothcd( jerr,scccd,invbeg,invend,ICDIN,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
      enddo
c
      call wrechm(jerr, nrec(IDXERR))
      if(jerr .NE. ISUCES) goto 9999
      goto 111
c
 6002 continue
      if( .NOT. l6002 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A,/,A)',ERR=9999) 'WARNING:  EMBR file ',
     &                'contains ASC/SCC codes that are mapped to ',
     &                                  'nonexistent profile codes.'
         write(ioerr,'(10X,2A)',ERR=9999) 'ASC/SCC code: ',scccd
         write(ioerr,'(10X,2A)',ERR=9999) 'Profile code: ',
     &                                                   prfxrf(idxscc)
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be ',
     &                                    'written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6002 = .TRUE.
      call totchm(IDXIN,emin,emin,spcpol,0)
      call totchm(IDXERR,emin,emin,spcpol,0)
c
      do i=1,ncrthd
        call addtotcty( jerr,ICDIN,idxfip,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotasc( jerr,ICDIN,scccd,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotfcd( jerr,ICDIN,idxfip,scccd,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothrs( jerr,invbeg,invend,ICDIN,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothcd( jerr,scccd,invbeg,invend,ICDIN,i,0,emin(i) )
        if( jerr .NE. ISUCES ) goto 9999
      enddo
c
      call wrechm(jerr, nrec(IDXERR))
      if(jerr .NE. ISUCES) goto 9999
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
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
