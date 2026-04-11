**** PRCGRD.F 
c
      subroutine prcgrd(ierr)
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the GRDEM module.  An EMBR record is
c    read and prcocessed.  If it is a low level point source record
c    a grid cell is assigned by means of the source location.  If it is
c    an area source, the emissions is allocated to the cells in the
c    county based on the surrogate cross reference and gridded
c    surrogates data.  If it is a mobile source, then the emissions are
c    either distributed to the cells containing links in the county or
c    via surrogates as is done with area sources.
c    One of two types of files is written: a UAM file or an EMBR file.
c
c   Argument Declaration:
c   Outputs:
c     ierr     I   error flag
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
      include 'uamcntl.inc'
      include 'cntylst.inc'
      include 'embrdat.inc'
      include 'hash.inc'
      include 'msgtot.inc'
      include 'iogrdem.inc'
      include 'usrgrdem.inc'
      include 'grdemsrg.inc'
      include 'grdemlnk.inc'
      include 'grdemtot.inc'
c
c-----------------------------------------------------------------------
c    Argument decelarations:
c-----------------------------------------------------------------------
c
       integer*4 ierr
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   fndchr       I   returns the index of a string in an array of strings
c   fndhash_fips I   returns the index of the FIPS code in the hash table
c   fndint       I   returns the index of an integer in an array of integers
c   fndgasc      I   returns the index of the FIPS/ASC pair in dual arrays
c   julcal       I   returns the calander date for a julian day
c
      integer*4 fndchr
      integer*4 fndhash_fips
      integer*4 fndint
      integer*4 fndgasc
      integer*4 julcal
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c   icurdt  I   current date for UAM type file
c   idxfip  I   index of county in county list
c   idxlnk  I   index of surrogate code in links array
c   idxscc  I   index of FIPS/ASC pair in XREF arrays
c   idxlng  I   index of long FIPS code in surrogate arrays
c
      character*10 lngfip, lngtmp, lastlng, lastasc
      integer*4    jerr, idxfip, isrgcd
      integer*4    icurdt(MXHOUR), iclbeg, iclend, ihrbeg, ihrend
      integer*4    idxscc, idxlnk, irec, ibegdt, ienddt, idxlng
      integer*4    icell, jcell, ibeghr, iendhr, idxhr, nhours
      integer*4    idxscc_last, i, j, k, l
      real*4       emtot(MXCRIT), spctot(MXSPEC), denom
      logical*4    lhour(MXHOUR), lanyhr, l6001, l6002
      logical*4    l6003, l6004, l6005, l6006, l6007, l6008
      logical*4    lsec, ltert, lwarn, lusesec, lusetert
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      lwarn = .FALSE.
      l6001 = .FALSE.
      l6002 = .FALSE.
      l6003 = .FALSE.
      l6004 = .FALSE.
      l6005 = .FALSE.
      l6006 = .FALSE.
      l6007 = .FALSE.
      l6008 = .FALSE.
      lsec  = .FALSE.
      ltert = .FALSE.
      irec = 0
      fiplast = ' '
      asclast = ' '
      lnglast = ' '
c
c   --- if we want UAM output and the EMBR file is not speciated,
c       just write error and exit ----
c
      if( (lnetcdf_out .OR. luam) .AND. nspecs .LE. 0 ) goto 7000
c
c   --- zero out gridded array ---
c
      do l=1,MXHOUR
         do k=1,MXSPEC
           do j=1,MXCELY
               do i=1,MXCELX
                  emsgrd(l,k,j,i) = 0.
               enddo
           enddo
         enddo
      enddo
c
c  --- intialize link surrogates if no link data supplied ---
c
      dftyp(IDXHW) = TYPHW
      dftyp(IDXRR) = TYPRR
      dftyp(IDXAP) = TYPAP
      dftyp(IDXPT) = TYPPT
      dftyp(IDXDS) = TYPDS
c
c  --- initialize arrays with dates and hours for this epsiode ----
c
      nhours = 0
 111  continue
      nhours = nhours + 1
      ibeghr = INT( begtim / 100 ) + nhours - 1
      iendhr = INT( begtim / 100 ) + nhours 
      ibegdt = ibgdat * 100 + ibeghr
      ienddt = ibgdat * 100 + ibeghr
      if( ibeghr .GT. 24 ) then
          ibeghr = ABS(24 - ibeghr)
          ibegdt = (ibgdat+1) * 100 + ibeghr
      endif
      if( iendhr .GT. 24 ) then
          iendhr = ABS(24 - iendhr)
          ienddt = (ibgdat+1) * 100 + iendhr
      endif
      if( ( INT(ienddt/100.) .LT. iendat) .OR.
     &          (INT( ienddt/100.)  .EQ. iendat .AND.
     &                     iendhr .LE. INT( endtim/ 100 ) ) ) then
        if( nhours .GT. MXHOUR ) goto 7001
        icurdt(nhours) = ibegdt
        goto 111
      endif
      nhours = nhours - 1
c
c  ---  read an EMBR record ---
c
  222 continue
      irec = irec + 1
      call rdembr(jerr,
     &                  IORMBR, irec )
      if( jerr .EQ. IEOD ) goto 333
      if( jerr .EQ. IEOF ) goto 6000
      if( jerr .NE. ISUCES ) goto 9999
      lusesec = .FALSE.
      lusetert = .FALSE.
c
c   --- put values into local array for totals ---
c
      do i=1,ncrthd
        emtot(i) = emin(i)
      enddo
      do i=1,nspecs
        spctot(i) = spcpol(i)
      enddo
c
c   --- set long FIPS code ---
c
      lngfip(1:5) = fipin
      lngfip(6:10) = sbrgn
      lngtmp(1:5) = fipin
      lngtmp(6:10) = '     '
c
c   --- set output arrays to missing ---
c
      do i=1,ncrthd
         emout(i) = RMISS
      enddo
      do i=1,nspchd
         spcout(i) = RMISS
      enddo
c
c   --- call routine to get index of FIPS code in county list ---
c
      if( fipin .NE. fiplast ) then
         idxfip = fndhash_fips( fipin )

         if( idxfip .LE. 0 ) goto 6001
         fiplast = fipin
      endif
c
c   --- add input emissions to totals ---
c
      call totgrd(jerr,IDXIN,emin,spcpol,ICDIN,idxfip,
     &                         scccd,invbeg,invend,.FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if UAM file, check for correct hour ---
c
      denom = 0.
      if( (lnetcdf_out .OR. luam) .AND. .NOT. lmeds ) then
          ibeghr = MOD(invbeg,100)
          iendhr = MOD(invend,100)
          if( iendhr .EQ. 0 ) iendhr = 24
          if( iendhr - ibeghr .GT. 24 ) goto 6002
          lanyhr = .FALSE.
          do i=1,nhours
             lhour(i) = .FALSE.
             if( icurdt(i) .GE. invbeg .AND. icurdt(i) .LT. invend) then
                 lanyhr = .TRUE.
                 lhour(i) = .TRUE.
             endif
          enddo
c
c    --- data on the record for hours beyond the current hours ---
c
          if( .NOT. lanyhr ) goto 6003
c
c   --- calculate the hourly emisisons ---
c
          denom = FLOAT(iendhr - ibeghr)
      endif
      if( denom .EQ. 0. ) denom = 1.0
c
c  --- check for already gridded ---
c
      if( ijcell .GT. 0 ) then
         icell = INT( ijcell/1000 )
         jcell = MOD( ijcell, 1000 )
         do i=1,ncrthd
            emout(i) = emin(i)/denom
         enddo
         do i=1,nspchd
           if( spcpol(i) .GT. 0. ) then 
              spcout(i) = spcpol(i)/denom
              spctot(i) = spcpol(i)
              if( (lnetcdf_out .OR. luam) .AND. .NOT. lmeds ) then
                 do idxhr=1,nhours
                    if( lhour(idxhr) ) then
                       emsgrd(idxhr,i,jcell,icell) =
     &                           emsgrd(idxhr,i,jcell,icell) + spcout(i)
                    endif
                 enddo
              endif
            endif
         enddo
c
c   --- if not a UAM file, write a new EMBR record ---
c
         if( loutfile .AND. (.NOT. luam .OR. lmeds .OR. lcomp) ) then
             if( lmeds ) then
                  call wrmeds( jerr,
     &                               IOWEMS, nrec(IDXOUT) )
             elseif( lcomp ) then
                  call wrcomp( jerr,
     &                               IOWEMS, nrec(IDXOUT) )
             else
                  call wrembr( jerr,
     &                               IOWEMS, nrec(IDXOUT) )
             endif
             if( jerr .NE. ISUCES ) goto 9999
         endif
         call totgrd(jerr,IDXOUT,emtot,spctot,ICDOUT,idxfip,
     &                          scccd,invbeg,invend,lusesec,lusetert)
         if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- if emissions is area or motor vehicle, call processing
c      routine for area or link ---
c
      if(rectyp .EQ. ' ' .OR. rectyp .eq. 'M' .or. rectyp .eq. 'B') then
          if( .NOT. lsurg ) goto 6004
c
c   --- get index of long FIPS code in surrogate arrays ---
c
          idxlng = fndchr ( lngfip, 10, fipsrg, ncdsrg )
          if( idxlng .LE. 0 )
     &                 idxlng = fndchr ( lngtmp, 10, fipsrg, ncdsrg )
c
c   --- get index in cross reference arrays ---
c
          if( lngfip .NE. lnglast .OR. scccd .NE. asclast ) then
              idxscc = fndgasc ( lngfip, scccd )
              idxscc_last = idxscc
              lnglast = lngfip
              asclast = scccd
          else
              idxscc = 	idxscc_last 
          endif
          if( idxscc .LE. 0 ) goto 6005
          isrgcd = fndchr( srgxrf(idxscc,1), 3, codnm, ncodes )
c
c   --- use surrogates to process ---
c
          idxlnk = fndchr( srgxrf(idxscc,1), 3, dftyp, NLTYPE )
          if( idxlnk .LE. 0 ) then
              if( idxlng .LE. 0 ) goto 6006
              if( nsgcty(idxlng) .LE. 0 ) goto 6006
c
c  --- if using multiple surrogate codes, make sure this one
c      is valid ---
c
              if( lsrgmult ) then
                 if( isrgcd .LT. 1 .OR. isrgcd .GT. NSURG ) goto 6007
                 if( ABS(1.-srgtot(idxlng,isrgcd)) .GT. DELSRG ) then
c                    isrgcd = srgxrf(idxscc,2)
                    isrgcd = fndchr( srgxrf(idxscc,2), 
     &                                             3, codnm, ncodes )
                    if( isrgcd .LT. 1 .OR. isrgcd .GT. NSURG ) goto 6007
                    if( ABS(1.-srgtot(idxlng,isrgcd)) .GT. DELSRG ) then
c                        isrgcd = srgxrf(idxscc,3)
                        isrgcd = fndchr( srgxrf(idxscc,3), 
     &                                              3, codnm, ncodes )
                        ltert = .TRUE.
                        lusesec = .FALSE.
                        lusetert = .TRUE.
                    else
                        lsec = .TRUE.
                        lusesec = .TRUE.
                    endif
                 endif
              endif
              if( isrgcd .LT. 1 .OR. isrgcd .GT. NSURG ) goto 6007
              call prcar( jerr, isrgcd, idxfip, idxlng, lhour, 
     &                      denom, emtot, spctot, lusesec, lusetert )
              if( jerr .NE. ISUCES ) goto 9999
          else
c
c   --- use link definition to process ---
c
              if( .NOT. lnkdef .OR.
     &                          tlink(idxlnk,idxfip) .LE. 0.) goto 6008
              call prclnk( jerr, idxlnk, idxfip, lhour,
     &                                            denom, emtot, spctot )
              if( jerr .NE. ISUCES ) goto 9999
          endif
c
c   --- low level point source ---
c
      else if( rectyp .EQ. 'L' ) then
          call prcpt( jerr, idxfip, lhour, denom, lwarn, emtot, spctot )
          if( jerr .NE. ISUCES ) goto 9999
c
c   --- elevated point source, processed by another module ---
c
      else if( rectyp .EQ. 'E' ) then
          call totgrd(jerr,IDXELE,emtot,spctot,0,0,scccd,
     &                                   invbeg,invend,.FALSE.,.FALSE.)
          if( (lnetcdf_out .OR. luam) .AND. .NOT. lmeds ) then
              goto 222
          else
              call prcpt( jerr, idxfip, lhour, denom, lwarn,
     &                                                 emtot, spctot )
              if( jerr .NE. ISUCES ) goto 9999
          endif
      endif
c
c  --- get next record ---
c
      goto 222
c
c  --- finished this pass through, if output is UAM write data ---
c
  333 continue
      if( luam .AND. .NOT. lmeds ) then
          call wrtuam( jerr,
     &                       icurdt )
          if( jerr .NE. ISUCES ) goto 9999
      endif
      if( lnetcdf_out ) then
          call ncf_wrt_data_hourly( jerr,
     &                                    icurdt )
          if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- echo a warning if secondary or tertiary codes were used ---
c
      if( lsec ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  Because of ',
     &        'incomplete gridded surrogates data, secondary '
         write(ioerr,'(2A)',ERR=9999) 'surrogate codes were ',
     &              'used. You will have to inspect your gridded '
         write(ioerr,'(2A)',ERR=9999) 'surrogates file to find ',
     &                                 'holes or partial counties.'
         write(ioerr,'(/,2A)',ERR=9999) 'All sources using a ',
     &          'secondary surrogate should have been gridded properly.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
c
      if( ltert ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  Because of ',
     &        'incomplete gridded surrogates data, tertiary '
         write(ioerr,'(2A)',ERR=9999) 'surrogate codes were ',
     &              'used. You will have to inspect your gridded '
         write(ioerr,'(2A)',ERR=9999) 'surrogates file to find ',
     &                                 'holes or partial counties.'
         write(ioerr,'(/,2A)',ERR=9999) 'Sources using a ',
     &       'tertiary surrogate may not have been gridded completely.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
 6000 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(2A)',ERR=9999) 'WARNING:  Input EMBR file does ',
     &                                 'not contain a TRAILER section.'
      write(ioerr,'(2A)',ERR=9999) 'File may be truncated or ',
     &                                                    'incomplete.'
      write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      goto 333
c
 6001 continue
      if( .NOT. l6001 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                           'counties that are not in the USERIN.'
         write(ioerr,'(10X,2A)',ERR=9999) 'Unlisted FIPS: ',fipin
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ',
     &                                    'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6001 = .TRUE.
      call totgrd(jerr,IDXIN,emin,spcpol,ICDIN,MXFIP+1,
     &                              scccd,invbeg,invend,.FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
      call totgrd(jerr,IDXSKP,emin,spcpol,0,0,scccd,invbeg,invend,
     &                                                  .FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
 6002 continue
      if( .NOT. l6002 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &              'records that have not been temporally allocated.'
         iclbeg = julcal( invbeg/100 )
         ihrbeg = MOD(invbeg,100)
         iclend = julcal( invend/100 )
         ihrend = MOD(invend,100)
         write(ioerr,'(10X,A,I6,1X,I2)',ERR=9999) 
     &                                  'Beginning date: ',iclbeg,ihrbeg
         write(ioerr,'(10X,A,I6,1X,I2)',ERR=9999)
     &                                  'Ending date   : ',iclend,ihrend
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6002 = .TRUE.
      call totgrd(jerr,IDXEMR,emin,spcpol,0,0,scccd,
     &                                  invbeg,invend,.FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
      call wregrd(jerr, nrec(IDXEMR) )
      goto 222
c
 6003 continue
      if( .NOT. l6003 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                  'records that are not in the episode period.'
         iclbeg = julcal( invbeg/100 )
         ihrbeg = MOD(invbeg,100)
         iclend = julcal( invend/100 )
         ihrend = MOD(invend,100)
         write(ioerr,'(10X,A,I6,1X,I2)',ERR=9999) 
     &                                  'Beginning date: ',iclbeg,ihrbeg
         write(ioerr,'(10X,A,I6,1X,I2)',ERR=9999)
     &                                  'Ending date   : ',iclend,ihrend
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                                    ' skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6003 = .TRUE.
      call totgrd(jerr,IDXSKP,emin,spcpol,0,0,scccd,
     &                                   invbeg,invend,.FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
 6004 continue
      if( .NOT. l6004 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &        'records that need surrogate data in order to process.'
         write(ioerr,'(10X,A)',ERR=9999) 
     &                            'No surrogate data was provided.'
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                                    ' skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6004 = .TRUE.
      call totgrd(jerr,IDXSKP,emtot,spctot,0,0,scccd,
     &                                 invbeg,invend,.FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
 6005 continue
      if( .NOT. l6005 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &            'records that have no match in surrogates XREF file.'
         write(ioerr,'(10X,2A)',ERR=9999) 'County code    : ',fipin
         write(ioerr,'(10X,2A)',ERR=9999) 'Source category: ',scccd
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         if( lsrgmult ) then
             write(ioerr,'(/,3X,2A)',ERR=9999) 'You have selected to ',
     &                               'use secondary surrogate codes.'
             write(ioerr,'(3X,2A)',ERR=9999) 'Make sure your cross ',
     &             'reference file has 3 fields for surrogate codes.'
         endif
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6005 = .TRUE.
      call totgrd(jerr,IDXEMR,emtot,spctot,0,0,scccd,
     &                                 invbeg,invend,.FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
      call wregrd(jerr, nrec(IDXEMR) )
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
 6006 continue
      if( .NOT. l6006 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A,/,A)',ERR=9999) 'WARNING:  EMBR file ',
     &              'contains records with counties that have ',
     &                                  'no gridded surrogates data.'
         write(ioerr,'(10X,2A)',ERR=9999) 'County code    : ',fipin
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6006 = .TRUE.
      call totgrd(jerr,IDXEMR,emtot,spctot,0,0,scccd,
     &                                 invbeg,invend,.FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
      call wregrd(jerr, nrec(IDXEMR) )
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
 6007 continue
      if( .NOT. l6007 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A,/,A)',ERR=9999) 'WARNING:  EMBR file ',
     &              'countains records with source categories ',
     &                          'mapped to invalid surrogate code.'
         write(ioerr,'(10X,2A)',ERR=9999) 
     &                                'Source category        : ',scccd
         if ( lsrgmult ) then
            write(ioerr,'(10X,A,3(1X,A))',ERR=9999) 
     &           'Invalid surrogate codes: ', (srgxrf(idxscc,i),i=1,3)
         else
            write(ioerr,'(10X,A,A)',ERR=9999) 
     &                   'Invalid surrogate code : ', srgxrf(idxscc,1)
         endif
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6007 = .TRUE.
      call totgrd(jerr,IDXEMR,emtot,spctot,0,0,scccd,
     &                                 invbeg,invend,.FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
      call wregrd(jerr, nrec(IDXEMR) )
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
 6008 continue
      if( .NOT. l6008 ) then
         write(ioerr,'(2A,/,A)',ERR=9999) 'WARNING:  EMBR file ',
     &              'contains records with counties that have ',
     &                                           'no links data.'
         write(ioerr,'(10X,2A)',ERR=9999) 'County code    : ',fipin
         write(ioerr,'(10X,2A)',ERR=9999) 'Source category: ',scccd
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6008 = .TRUE.
      call totgrd(jerr,IDXEMR,emtot,spctot,0,0,scccd,
     &                                invbeg,invend,.FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
      call wregrd(jerr, nrec(IDXEMR) )
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Input EMBR file is not ',
     &                                                      'speciated.'
      write(ioerr,'(2A)',ERR=9999) 'In order to make an output file ',
     &                                  'for modeling the EMBR file '
      write(ioerr,'(A,//)',ERR=9999) 'must be processed through CHMSPL.'
      goto 9999
c 
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: EPS cannot process an ',
     &                      'inventory of longer than a 24 hour period.'
      write(ioerr,'(2A,//)',ERR=9999) 'Please update the USERIN ',
     &                                'to reflect a single day epsiode.'
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
