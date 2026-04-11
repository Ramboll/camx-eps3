C**** PRCSPC.F
c   
      subroutine prcspc( ierr )
c
c-----------------------------------------------------------------------
c
c    Read the input EMBR file.
c    Apply the speciation factors.  Write the output EMBR file.
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c
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
      include 'iospcems.inc'
      include 'factors.inc'
      include 'spcems.inc'
      include 'totspc.inc'
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
c   fndxcpt I  cross-reference lookup for point sources 
c   fndxcar I  cross-reference lookup for area sources 
c   fndchr  I  find a string in an array (library)
c   fndhash_fips I   returns index of FIPS code in an hash table
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 fndxcpt
      integer*4 fndhash_fips
      integer*4 fndxcar
      integer*4 fndchr
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c    rdembr  -- read one EMBR /DATA/ record (library)
c    wrespc  -- write error records (when SCC is not in cross ref)
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c    DEFPRF    C   standard default profile code
c
      character*(LENPRF)  DEFPRF
c
      parameter( DEFPRF = '0000' )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c     idxdef  I   index of default profile code
c     idxfip  I   index of FIPS code
c     idxscc  I   position of SCC in cross-reference file
c     idxprf  I   index of profile code in splits list for each criteria
c     jerr    I   error code for subroutine calls
c
      integer*4    idxdef
      integer*4    jerr, idxscc, idxfip, irec, icrt, ispc, i
      integer*4    idxprf(MXCRIT), idxpm
      real*4       emstmp, emstot(MXCRIT), spctmp, emsval
      real*4       tmpmin, tmpsub, tmpnew
      logical*4    l6000, l6001, l6002, lwarn
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c     --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      l6000 = .FALSE.
      l6001 = .FALSE.
      l6002 = .FALSE.
      lwarn = .FALSE.
c
c     --- report number of xref records ---
c
      write(IOWMSG,'(/,1X,A,I8)',ERR=7000)
     &    'Number of speciation xref records read:', npfxrf
c
c     --- determine if a default profile has been provided ---
c
      idxdef = fndchr ( DEFPRF,LENPRF,prffac,nprf )
      if ( idxdef .EQ. 0 ) then
          nprf = nprf + 1
          if( nprf .GT. MXSPL ) goto 7002
c
          prffac(nprf) = DEFPRF
          idxdef = nprf
      endif
c
c     --- read a record from the input EMBR file --- ================
c
  111 continue
      nspchd = 0
      irec = irec + 1
      call rdembr(jerr,IOREMB,irec)
      if(jerr .EQ. IEOD) goto 222
      if(jerr .NE. ISUCES) goto 9999
c
c     --- check for county in region ---
c
      idxfip = fndhash_fips ( fipin )
      if(idxfip .LE. 0) goto 6000
c
c     --- set profile index values to missing ---
c
      do i=1,ncrthd
         idxprf(i) = IMISS
      enddo
c
c     --- for every nonzero criteria pollutant --- ===============
c
      do icrt = 1,ncrthd
         if( emin(icrt) .GT. 0. )  then
c           --- skip if using this criteria as the basis of a new computed compound ---
            if ( lpart .AND. lpmM(icrt) )  goto 150
c
c           ---  look for pollutant specific ASC-SCC in cross-reference info ---
c           ---  if not found, end record processing and write to error ----
c
            if( rectyp .EQ. 'E' .OR. rectyp .EQ. 'L' ) then
                idxscc = fndxcpt
     &                (fipin, plant, stkid, point, scccd, crtnam(icrt))
            else
                idxscc = fndxcar ( fipin, scccd, crtnam(icrt))
            endif
            if(idxscc .LE. 0) goto 6001
c
c           --- look for profile code in speciation factors ---
c           --- if not found and user requires an exact match then write to error file ---
c           ---   else set to profile code 0000 (standard default) ---
c
            idxprf(icrt) = fndchr ( prfxrf(idxscc),LENPRF,prffac,nprf )
            if(idxprf(icrt) .EQ. 0) then
               if( .NOT. lreqpf .AND. idxdef .GT. 0 )  then
                   idxprf(icrt) = idxdef

                   if ( .NOT. lwarn ) then
                    write(ioerr,'(//,45A,/)',ERR=9999) ('/\\',i=1,45)
                    write(ioerr,'(3A)',ERR=9999) 'WARNING:  EMBR ',
     &                    'file contains ASC/SCC codes that are ',
     &                    'mapped to nonexistent profile codes.'
                    write(ioerr,'(10X,2A)',ERR=9999) 'ASC/SCC code: ',
     &                                                scccd
                    write(ioerr,'(10X,2A)',ERR=9999) 'Profile code: ',
     &                                                 prfxrf(idxscc)
                    write(ioerr,'(5X,4A)',ERR=9999) 'The default ',
     &                   'profile will be used for all such records.  ',
     &                   'Default: ', DEFPRF
                    write(ioerr,'(/3A)',ERR=9999) 'WARNING:  This ',
     &                  'may result in a loss of speciated emissions.'
                    write(ioerr,'(/,45A,//)',ERR=9999) ('/\\',i=1,45)
                    lwarn = .TRUE.
                    nwarn = nwarn + 1
                   endif
               else
                 goto 6002
               endif
            endif
c QA
c QA        write(*,*) irec, idxfip, idxscc, idxprf(icrt)  
  150       continue
         endif   ! emissions > 0
      enddo   ! criteria loop
c
c     --- set output values to missing ---
c
      do i=1,MXCRIT
         emout(i) = RMISS
      enddo
      do i=1,nspecs
         spcout(i) = 0.0
      enddo
c
c     --- add input emissions to arrays ---
c
      call totspc(IDXIN,emin,emin,.TRUE.,idxprf)
c
      do icrt = 1, ncrthd
         call addtotcty( jerr,ICDIN,idxfip,icrt,0,emin(icrt) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotasc( jerr,ICDIN,scccd,icrt,0,emin(icrt) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotfcd( jerr,ICDIN,idxfip,scccd,icrt,0,emin(icrt) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtothrs( jerr,invbeg,invend,ICDIN,icrt,0,emin(icrt) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotpid( jerr,ICDIN,idxfip,plant,stkid,
     &                                     point,icrt,0,emin(icrt) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtothcd( jerr,scccd,invbeg,invend,ICDIN,
     &                                          icrt,0,emin(icrt) )
         if( jerr .NE. ISUCES ) goto 9999
      enddo
c      
c     --- output criteria emissions is not changed from input ---
c
      do icrt = 1, ncrthd
         emout(icrt) = emin(icrt)
         emstot(icrt) = 0.
      enddo
c
c     --- Valid profile for all criteria pollutants ---
c     ---  Apply speciation factors ---
c
      do 200 icrt = 1, ncrthd
c
         if ( emin(icrt) .GT. 0 )  then
            
            if ( idxprf(icrt) .LE. 0 
     &         .AND. .NOT.(lpart .AND. lpmM(icrt)) )  goto 7001
            emstmp = 0.
            emsval = emin(icrt)
c           --- Apply conversion factor by profile ---
            if ( lconv .AND. icrt .EQ. idxfrm ) then
                   emsval = emsval * cnvftr(idxprf(icrt))
            endif
c
            do 100 ispc = 1,nspecs
cQA
cQA            write(*,*) 'inside ', idxprf(icrt), icrt, ispc, 
cQA     &                      spcfac(idxprf(icrt),icrt,ispc)
                  spctmp = (emsval / CVTTON) * 
     &                            spcfac(idxprf(icrt),icrt,ispc)
c                 --- compute the new 'particulate' compound ---
c                 --- and set output of A to zero where A - B = new ---
                  if ( lpart .AND. lpmM(icrt) )  then
                     idxpm = fndchr ( spcnam(ispc),10,pmnew,npmcs )
                     if ( idxpm .GT. 0 ) then
                        tmpmin = emin(icrt)
                        if ( tmpmin .LT. 0. )  tmpmin = 0.0
                        tmpsub = emin(idxsub(idxpm))
                        if ( tmpsub .LT. 0. )  tmpsub = 0.0
                        tmpnew = tmpmin - tmpsub
                        if ( tmpnew .LT. 0 )  tmpnew = 0.0
                        spctmp = tmpnew / CVTTON
                     endif
                  endif
                  spcout(ispc) = spcout(ispc) + spctmp
                  emstmp = spctmp * CVTTON * molwt(ispc)
                  emstot(icrt) = emstot(icrt) + emstmp
c
c                 --- load output profile speciated table ---
c
                  totpfs(idxprf(icrt),ispc) = 
     &                             totpfs(idxprf(icrt),ispc)
     &                            +  spctmp * molwt(ispc) * CVTTON
c
c                 ----load criteria output tables as sum of speciated ---
c
c  report PMs as new computed  if ( lpart .AND. lpmM(icrt) )  goto 500
                  call addtotcty( jerr,ICDOUT,idxfip,icrt,0,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
                  call addtotasc( jerr,ICDOUT,scccd,icrt,0,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
                  call addtotfcd( jerr,ICDOUT,idxfip,scccd,
     &                                                 icrt,0,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
                  call addtothrs( jerr,invbeg,invend,ICDOUT,
     &                                                  icrt,0,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
                  call addtotpid( jerr,ICDOUT,idxfip,plant,stkid,
     &                                           point,icrt,0,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
                  call addtothcd( jerr,scccd,invbeg,invend,ICDOUT,
     &                                                icrt,0,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
c                 ---- load output tables for speciated ---
c
  500             continue
                  call addtotcty( jerr,ICDOUT,idxfip,0,ispc,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
                  call addtotasc( jerr,ICDOUT,scccd,0,ispc,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
                  call addtotfcd( jerr,ICDOUT,idxfip,scccd,
     &                                                0,ispc,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
                  call addtothrs( jerr,invbeg,invend,ICDOUT,
     &                                                  0,ispc,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
                  call addtotpid( jerr,ICDOUT,idxfip,plant,stkid,
     &                                           point,0,ispc,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
                  call addtothcd( jerr,scccd,invbeg,invend,ICDOUT,
     &                                                  0,ispc,emstmp )
                  if( jerr .NE. ISUCES ) goto 9999
c
  100       continue ! enddo species list loop
c
         endif    ! > 0. criteria emissions
  200 continue  ! enddo criteria pollutants loop
c
c     --- add emissions to totals ---
c
      call totspc(IDXSPC,emout,emstot,.TRUE.,idxprf)
      call totspc(IDXOUT,emout,emstot,.TRUE.,idxprf)
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
c
cQA   
cQA      do i = 1, npfxrf
cQA         write(*,*) totpfc(1,i,1), totpfc(2,i,1),
cQA     &              totpfc(1,i,2), totpfc(2,i,2),
cQA     &              totpfc(1,i,3), totpfc(2,i,3)
cQA      enddo
        
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
     &          'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6000 = .TRUE.
      call totspc(IDXIN,emin,emin,.FALSE.,0)
      call totspc(IDXSKP,emin,emin,.FALSE.,0)
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
         write(ioerr,'(2(10X,A,A10))',ERR=9999) 'ASC/SCC code: ',scccd,
     &                                       'Criteria: ',crtnam(icrt)
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be ',
     &                                    'written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6001 = .TRUE.
      call totspc(IDXIN,emin,emin,.FALSE.,0)
      call totspc(IDXERR,emin,emin,.FALSE.,0)
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
      call wrespc(jerr, nrec(IDXERR))
      if(jerr .NE. ISUCES) goto 9999
      goto 111
c
 6002 continue
      if( .NOT. l6002 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A,/,A)',ERR=9999) 'WARNING:  EMBR file ',
     &                'contains ASC/SCC codes that are mapped to ',
     &            '          nonexistent speciation profile codes.'
         write(ioerr,'(15X,2A)',ERR=9999) 'ASC/SCC code: ',scccd
         write(ioerr,'(15X,2A)',ERR=9999) 'Profile code: ',
     &                                                prfxrf(idxscc)
         write(ioerr,'(10X,2A)',ERR=9999) 'All such records will be ',
     &                                    'written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6002 = .TRUE.
      call totspc(IDXIN,emin,emin,.FALSE.,0)
      call totspc(IDXERR,emin,emin,.FALSE.,0)
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
      call wrespc(jerr, nrec(IDXERR))
      if(jerr .NE. ISUCES) goto 9999
      goto 111

c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7001 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Processing emissions.'
      write(ioerr,'(2A,//)',ERR=9999) 'Nonzero emissions with no ',
     &                                'valid profile code.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999) 
     &       'ERROR:  Adding default profile for reporting ',
     &                'exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSPL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSPL ',
     &                                                  'and recompile.'
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
