C***** PRCPSTSHP.F 
c
      subroutine prcpstshp(ierr)
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the PSTSHP module.  
c    Loops on EMBR record reads.
c
c    Emissions are distributed to surface file and vertical layers
c    based on ship class and hour of day. 
c
c   Argument Declaration:
c   Outputs:
c     ierr     I   error flag
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
      include 'uamcntl.inc'
      include 'cntylst.inc'
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'layers.inc'
      include 'iopstshp.inc'
      include 'pstshpstk.inc'
      include 'pstshptot.inc'

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
c   fndhash_fips I   returns index of FIPS code in an hash table
c   fndhash_asc  I   returns index of ASC code in an hash table
c   fndvsl       I   returns the index of vessel in Vessel/Stack XREF file
c   fndvstk      I   returns the index of stack in global arrays
c
      integer*4 fndhash_fips
      integer*4 fndhash_asc
      integer*4 fndvsl
      integer*4 fndvstk
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c   icurdt  I   current date for UAM type file
c   idxfip  I   index of county in county list
c   lwarn   L   true if the warning for fips not found has been given
c   emselv  R   array of elevated speciated emissions
c   emscrt  R   array of elevated criteria emissions
c   crtelv  R   array of elev criteria emissions used to set totals
c   spcelv  R   array of elev speciated emissions used to set totals
c   crtsrf  R   array of surface criteria emissions used to set totals
c   spcsrf  R   array of surface speciated emissions used to set totals
c
      character*10 vessel
      integer*4    irec, i, j, k, n, jerr, idxvsl, ibeghr, iendhr
      integer*4    icurdt(MXHOUR), idxfip, icell, jcell, idxstk
      integer*4    idxhr, nhours, ibegdt, ienddt, idxbin, nl
      integer*4    idxasc, ilay, idx_ijcell
      real*4       emselv(MXSPEC,MXSTK,MXHOUR)
      real*4       emscrt(MXCRIT,MXSTK), denom, tot, fout
      real*4       crtelv(MXCRIT), crtsrf(MXCRIT)
      real*4       spcelv(MXSPEC), spcsrf(MXSPEC)
      logical*4    lanyhr, lhour(MXHOUR), l6001, l6002, l6003, l6004
      logical*4    l6005
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- initialize error and warning flags ---
c
      ierr = IFAIL
      l6001 = .FALSE.
      l6002 = .FALSE.
      l6003 = .FALSE.
      l6004 = .FALSE.
      l6005 = .FALSE.
      irec = 0
c        
c  --- initialize emission arrays to zero ---
c
      emscrt = 0.0
      emselv = 0.0
c
c  --- initialize arrays with dates and hours for this epsiode ---
c
      nhours = 0
  111 continue
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
        if( nhours .GT. MXHOUR ) goto 7000
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
c
c   --- load the vessel type (stored in existing EMBR variable)
c
      vessel = plant
c
c   --- call routine to get index of FIPS code in county list ---
c
      idxfip = fndhash_fips ( fipin )
      if( idxfip .LE. 0 ) goto 6001
c
c   --- add input emissions to totals ---
c
      call totpstshp(jerr,IDXIN,emin,spcpol,ICDIN,idxfip,
     &                                            scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- check for correct hours ---
c
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
c   --- data on the record for hours beyond the current hours ---
c
      if( .NOT. lanyhr ) goto 6003
c
c   --- if not shipping point source, skip it ----
c
      if( rectyp .NE. 'S' ) goto 6005
c
c   --- call routine to get best match in the vessel/stack
c       cross reference file ----
c
      idxvsl = fndvsl( scccd, vessel )
      if( idxvsl .EQ. 0 ) goto 6004
c
c   --- calculate the cell indexes ---
c
      icell = INT( ijcell / 1000 )
      jcell = MOD( ijcell, 1000 )
c
c   --- call routine to get index of stack in arrays, if not
c       found, add to arrays ---
c
      idxstk = fndvstk ( fipin, scccd, vessel, ijcell )
      if( idxstk .EQ. 0 ) then
         if( icell .LE. 0 .OR. icell .GT. nxrgn .OR.
     &               jcell .LE. 0 .OR. jcell .GT. nyrgn ) then
              call totpstshp(jerr,IDXSKP,emin,spcpol,0,0,
     &                                        scccd,invbeg,invend)
              if( jerr .NE. ISUCES ) goto 9999
              call totpstshp(jerr,IDXREG,emin,spcpol,0,0,
     &                                        scccd,invbeg,invend)
              if( jerr .NE. ISUCES ) goto 9999
              goto 222
         endif
         nstack = nstack + 1
         if( nstack .GT. MXSTK ) goto 7001
         fipstk(nstack) = fipin
         ascstk(nstack) = scccd
         vslstk(nstack) = vessel
         ijclstk(nstack) = ijcell
         ptopstk(nstack) = plumetop(idxvsl)
         pbotstk(nstack) = plumebot(idxvsl)
         idxstk = nstack
      endif
c
c   --- initialize output arrays ---
c
      do i=1,ncrthd
         emout(i) = RMISS
         crtelv(i) = 0.
         crtsrf(i) = 0.
      enddo
      do i=1,nspchd
         spcout(i) = RMISS
         spcelv(i) = 0.
         spcsrf(i) = 0.
      enddo
c
c  --- calculate the hourly emissions ---
c
      denom = FLOAT(iendhr - ibeghr)
      if( denom .GT. 0. ) then
          do i=1,ncrthd
             if( emin(i) .GT. 0. ) then
                 emout(i) = emin(i) / denom
             endif
          enddo
          do i=1,nspchd
             if( spcpol(i) .GT. 0. ) then
                  spcout(i) = spcpol(i) / denom
             endif
          enddo
      endif
c
c     --- add emissions to output arrays and to totals ---
c
c     --- criteria emissions ---
c
      do i=1,ncrthd
         if( emout(i) .GT. 0. ) then
           do idxhr=1,MXHOUR
              if( lhour(idxhr) ) then

c  --- compute elevated emissions ---
c
                 if( lelvfl .OR. l3demiss ) then
                    fout = emout(i) * (1.-plumefrac(1,idxvsl))
                    emscrt(i,idxstk) = emscrt(i,idxstk) + fout 
                    crtelv(i) = crtelv(i) + fout
                 endif

c  --- compute surface emissions ---
c
                 if( lsrffl ) then
                    fout = emout(i) * plumefrac(1,idxvsl)
                    crtsrf(i) = crtsrf(i) + fout
                 endif

              endif
c
c  --- next hour ---
c
           enddo
         endif
c
c  --- next criteria pollutant ---
c
      enddo
c
c  --- speciated emissions ---
c
      do i=1,nspecs
         if( spcpol(i) .GT. 0. ) then
           do idxhr=1,MXHOUR
              if( lhour(idxhr) ) then
c
c  --- compute elevated emissions ---
c
                if( lelvfl ) then
                   fout = spcout(i) * (1.-plumefrac(1,idxvsl))
                   emselv(i,idxstk,idxhr) = 
     &                             emselv(i,idxstk,idxhr) + fout
                   spcelv(i) = spcelv(i) + fout
                endif
c
c  --- put emissions in
c
c
c  --- compute surface emissions ---
c
                if( lsrffl .OR. l3demiss ) then
                   fout = spcout(i) * plumefrac(1,idxvsl)
                   idx_ijcell = ijcell3d(icell,jcell,1)
                   if( idx_ijcell .LE. 0 ) then
                      num_ijcell3d = num_ijcell3d + 1
                      if( num_ijcell3d .GT. MX3DCELLS ) goto 7002
                      ijcell3d(icell,jcell,1) = num_ijcell3d
                      idx_ijcell = num_ijcell3d
                   endif
                   emiss3d(idxhr,i,idx_ijcell) = 
     &                    emiss3d(idxhr,i,idx_ijcell) + fout
                   spcsrf(i) = spcsrf(i) + fout
                   spcelv(i) = spcelv(i) + (spcout(i) -  fout)
                endif
c
c  --- compute gridded emissions for other layers  ---
c
                if( l3demiss ) then
                   do ilay=2,nlayer
                     fout = spcout(i) * plumefrac(ilay,idxvsl)
                     idx_ijcell = ijcell3d(icell,jcell,ilay)
                     if( idx_ijcell .LE. 0 ) then
                        num_ijcell3d = num_ijcell3d + 1
                        if( num_ijcell3d .GT. MX3DCELLS ) goto 7002
                        ijcell3d(icell,jcell,ilay) = num_ijcell3d
                        idx_ijcell = num_ijcell3d
                     endif
                     emiss3d(idxhr,i,idx_ijcell) = 
     &                      emiss3d(idxhr,i,idx_ijcell) + fout
                   enddo
                endif
             endif
c
c  --- next hour ---
c
           enddo
         endif
c
c  --- next species ---
c
      enddo
c
c  --- call routine to add to totals array ---
c
      call totpstshp(jerr,IDXOUT,crtsrf,spcsrf,ICDOUT,idxfip,
     &                                            scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call totpstshp(jerr,IDXSRF,crtsrf,spcsrf,0,idxfip,
     &                                            scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
c
      call totpstshp(jerr,IDXOUT,crtelv,spcelv,ICDOUT,idxfip,
     &                                            scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call totpstshp(jerr,IDXELV,crtelv,spcelv,0,idxfip,
     &                                            scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- get next record ---
c
      goto 222
c
c  --- finished pass through, write output file ---
c
  333 continue
c
c  ---- write the point source files ---
c
      if( lelvfl ) then
         call wrteleshp( jerr,
     &                      icurdt, emscrt, emselv, nhours )
         if( jerr .NE. ISUCES ) goto 9999
       endif
c
c  --- write the surface emissions file ---
c
      if ( lsrffl ) then
         call wrtuamshp( jerr,
     &                      icurdt ) 
         if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- set error flag and return ---
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
      call totpstshp(jerr,IDXIN,emin,spcpol,ICDIN,MXFIP+1,
     &                                           scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call totpstshp(jerr,IDXSKP,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
 6002 continue
      if( .NOT. l6002 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &              'records that have not been temporally allocated.'
         write(ioerr,'(10X,A,I8)',ERR=9999) 'Beginning date: ',invbeg
         write(ioerr,'(10X,A,I8)',ERR=9999) 'Ending date   : ',invend
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6002 = .TRUE.
      call totpstshp(jerr,IDXERR,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call wrepstshp(jerr, nrec(IDXERR) )
      goto 222
c
 6003 continue
      if( .NOT. l6003 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                  'records that are not in the episode period.'
         write(ioerr,'(10X,A,I8)',ERR=9999) 'Beginning date: ',invbeg
         write(ioerr,'(10X,A,I8)',ERR=9999) 'Ending date   : ',invend
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                                    ' skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6003 = .TRUE.
      call totpstshp(jerr,IDXSKP,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
 6004 continue
      if( .NOT. l6004 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &       'records that have no match in the Vessel/Stack XREF file.'
         write(ioerr,'(10X,2A)',ERR=9999) 'ASC code   : ',scccd
         write(ioerr,'(10X,2A)',ERR=9999) 'Vessel type: ',vessel
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6004 = .TRUE.
      call totpstshp(jerr,IDXERR,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call wrepstshp(jerr, nrec(IDXERR) )
      goto 222
c
 6005 continue
      if( .NOT. l6005 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                  'records that are not shipping records.'
         write(ioerr,'(10X,2A)',ERR=9999) 'Type : ',rectyp
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                                    ' skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6005 = .TRUE.
      call totpstshp(jerr,IDXSKP,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: EPS cannot process an ',
     &                      'inventory of longer than a 24 hour period.'
      write(ioerr,'(2A,//)',ERR=9999) 'Please update the USERIN ',
     &                                'to reflect a single day epsiode.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'Maximum number of stacks ',
     &                           'in the inventory exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSTK
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSTK ',
     &                                                  'and recompile.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999) 'Maximum number of cells in ',
     &                        'the gridded emissions file exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MX3DCELLS
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter ',
     &                                      'MX3DCELLS and recompile.'
      goto 9999
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
