C***** PRCPSTFIR.F 
c
      subroutine prcpstfir(ierr)
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the PSTFIR module.  
c    Loops on EMBR record reads.
c
c    Emissions are distributed to surface file and vertical layers
c    based on fire class and hour of day.  The vertical
c    distribution fractions are stored in vertfac array which is
c    populated in the CLCVERT routine.
c
c   Argument Declaration:
c   Outputs:
c     ierr     I   error flag
c
c-----------------------------------------------------------------------
c    LOG:
c mjimenez  03 july 2012 modified to optionally write surface emissions
c                                 to the elevated points file layer 1
c mjimenez june 2015     modified to support the CAMx plume rise
c                                 distribution override feature
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
      include 'msgtot.inc'
      include 'layers.inc'
      include 'firebin.inc'
      include 'iopstfir.inc'
      include 'usrpstfir.inc'
      include 'pstfirstk.inc'
      include 'pstfirtot.inc'

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
c   fndfir       I   returns the index of the stack data in stack arrays
c
      integer*4 fndhash_fips
      integer*4 fndfir
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
      integer*4 irec, i, j, k, n, jerr, idxstk, ibeghr, iendhr
      integer*4 icurdt(MXHOUR), idxfip, icell, jcell, ilay, idx_ijcell
      integer*4 idxhr, nhours, ibegdt, ienddt, idxbin, nl, ihr
      real*4    emselv(MXSPEC,MXSTK,MXHOUR), spctmp
      real*4    emscrt(MXCRIT,MXSTK), denom, tot
      real*4    crtelv(MXCRIT), crtsrf(MXCRIT)
      real*4    spcelv(MXSPEC), spcsrf(MXSPEC)
      real*4    plumefrac(MXLAYER), plumebot, plumetop, diff

      logical*4 lsrfplume
      logical*4 lanyhr, lhour(MXHOUR), l6001, l6002, l6003, l6004, l6005
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- initialize error and warning flags ---
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
      emssrf = 0.0
      emiss3d = 0.0
c
c  --- initialize reporting arrays ---
c
       crtbin = 0.0
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
c   --- call routine to get index of FIPS code in county list ---
c
      idxfip = fndhash_fips ( fipin )
      if( idxfip .LE. 0 ) goto 6001
c
c   --- add input emissions to totals ---
c
      call totpstfir(jerr,IDXIN,emin,spcpol,ICDIN,idxfip,
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
           lusehr(i) = .TRUE.
        endif
      enddo
c
c    --- data on the record for hours beyond the current hours ---
c
      if( .NOT. lanyhr ) goto 6003
c
c   --- if not fire point source, skip it ----
c
      if( rectyp .NE. 'F' ) goto 6005
c
c     --- check the fire class, stored in stack diameter ---
c
      idxbin = INT(stkdia)
      if ( idxbin .LE. 0 .OR. idxbin .GT. nfbin ) goto 6004
c
c   --- call routine to get index of stack in arrays, if not
c       found, add to arrays ---
c
      idxstk = fndfir ( fipin, stkdia, utme, utmn )
      if( idxstk .EQ. 0 ) then
           call setij( icell, jcell,
     &                       utme, utmn, 0. )
           if( icell .LE. 0 .OR. icell .GT. nxrgn .OR.
     &         jcell .LE. 0 .OR. jcell .GT. nyrgn ) then
                call totpstfir(jerr,IDXSKP,emin,spcpol,0,0,
     &                                           scccd,invbeg,invend)
                if( jerr .NE. ISUCES ) goto 9999
                call totpstfir(jerr,IDXREG,emin,spcpol,0,0,
     &                                           scccd,invbeg,invend)
                if( jerr .NE. ISUCES ) goto 9999
                goto 222
           endif
           nstack = nstack + 1
           if( nstack .GT. MXSTK ) goto 7001
           infips(nstack) = fipin
           stdiam(nstack) = stkdia
           xloc(nstack) = utme
           yloc(nstack) = utmn
           idxstk = nstack
           lsrfplume = .FALSE.
           do i=1,MXHOUR
              if ( aglfrac(idxbin,i) .GT. 0.0 ) lsrfplume = .TRUE.
           enddo
           if ( lsrfplume ) then
                 nstack = nstack + 1
                 infips(nstack) = fipin
                 stdiam(nstack) = stkdia
                 xloc(nstack) = utme
                 yloc(nstack) = utmn
           endif
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
             if( spcpol(i) .GT. 0. ) spcout(i) = spcpol(i) / denom
          enddo
      endif
c
c  --- add emissions to output arrays and to totals ---
c
c  --- criteria emissions ---
c
         do i=1,ncrthd
            if( emout(i) .GT. 0. ) then
              do idxhr=1,MXHOUR
                 if( lhour(idxhr) ) then
c
c  --- adjust lookup for timezone ---
c
                   ihr = idxhr + ( itmzon(idxfip) - invtzn )
                   if (ihr .GT. 24) ihr = ihr - 24
                   if (ihr .LT. 1) ihr = ihr + 24

                   crtbin(i,idxbin) = crtbin(i,idxbin) + emout(i)
                   if ( .NOT.lsrffl .OR. lelvall ) then    
c
c  --- all elevated output ---
c
                       crtsrf(i) = 0.
                       crtelv(i) = crtelv(i) + emout(i) 
                       emscrt(i,idxstk) = emscrt(i,idxstk) 
     &                            + emout(i) * elvfrac(idxbin,ihr)
                       emscrt(i,idxstk+1) = emscrt(i,idxstk+1) 
     &                            + emout(i) * srffrac(idxbin,ihr)
c
                   elseif ( .NOT. lelvfl .AND. .NOT. lptsfl ) then
c
c   --- all surface output ---
c
 
                       crtsrf(i) = crtsrf(i) + emout(i)
                   else
c
c   --- both surface and elevated outputs ---
c   --- include fraction in surface assigned a plume ---
c
                       if( aglfrac(idxbin,idxhr) > 0. ) then
                          emscrt(i,idxstk) = emscrt(i,idxstk) 
     &                        + emout(i) * elvfrac(idxbin,ihr)
                          emscrt(i,idxstk+1) = emscrt(i,idxstk+1) 
     &                        + emout(i) * aglfrac(idxbin,ihr)
                          crtelv(i) = crtelv(i) 
     &                        + emout(i) * elvfrac(idxbin,ihr)
     &                        + emout(i) * aglfrac(idxbin,ihr)
                          crtsrf(i) = crtsrf(i) 
     &                        + emout(i) * srffrac(idxbin,ihr)
                       else
c
c  --- no surface assigned to plume ---
c
                          emscrt(i,idxstk) = emscrt(i,idxstk) 
     &                        + emout(i) * elvfrac(idxbin,ihr)
                          crtelv(i) = crtelv(i) 
     &                        + emout(i) * elvfrac(idxbin,ihr)
                          crtsrf(i) = crtsrf(i) 
     &                        + emout(i) * srffrac(idxbin,ihr)
                       endif
                   endif


                 endif ! valid hour for record
              enddo  ! next hour
            endif  ! emiss>0
         enddo   ! next criteria pollutant
c
c  --- speciated emissions ---
c
         do i=1,nspecs
            if( spcpol(i) .GT. 0. ) then
              do idxhr=1,MXHOUR
                 if( lhour(idxhr) ) then
c
c   --- adjust lookup for timezone ---
c
                   ihr = idxhr + ( itmzon(idxfip) - invtzn )
                   if (ihr .GT. 24) ihr = ihr - 24
                   if (ihr .LT. 1) ihr = ihr + 24
c
c   --- compute elevated emissions by layer ---
c
                   if ( .NOT.lsrffl .OR. lelvall ) then    
c
c   --- all elevated output ---
c
                        spcsrf(i) = 0.0
                        spcelv(i) = spcelv(i) + spcout(i) 
                        emselv(i,idxstk,idxhr) = 
     &                             emselv(i,idxstk,idxhr) 
     &                             + spcout(i) * elvfrac(idxbin,ihr)
                        emselv(i,idxstk+1,idxhr) = 
     &                             emselv(i,idxstk+1,idxhr) 
     &                             + spcout(i) * srffrac(idxbin,ihr)
                        stktop(idxstk,idxhr) = plmovrtop(idxbin,ihr)
                        stkbot(idxstk,idxhr) = plmovrbot(idxbin,ihr)
                        stktop(idxstk+1,idxhr) = srftop(idxbin,ihr)
                        stkbot(idxstk+1,idxhr) = srfbot(idxbin,ihr)
c
c  -- if doing 3D emissions fill large 3D array ---
c
                        if( l3demiss ) then
                             spctmp = spcout(i) * elvfrac(idxbin,ihr)
                             call add_3demiss( icell,jcell,idxhr,
     &                          stkbot(idxstk,idxhr),stktop(idxstk,idxhr), 
     &                                                            i, spctmp )
                             spctmp = spcout(i) * srffrac(idxbin,ihr)
                             call add_3demiss( icell,jcell,idxhr,
     &                         stkbot(idxstk+1,idxhr),stktop(idxstk+1,idxhr), 
     &                                                             i, spctmp )
                        endif
                   elseif ( .NOT. lelvfl .AND. .NOT.lptsfl ) then
c
c   --- all surface output ---
c
                        spcsrf(i) = spcsrf(i) + spcout(i) 
                        emssrf(idxhr,i,jcell,icell) = 
     &                             emssrf(idxhr,i,jcell,icell) 
     &                           + spcout(i) 
                        spcelv(i) = 0.0
                        if( l3demiss ) then
                           idx_ijcell = ijcell3d(icell,jcell,1)
                           if( idx_ijcell .LE. 0 ) then
                              num_ijcell3d = num_ijcell3d + 1
                              if( num_ijcell3d .GT. MX3DCELLS ) goto 7000
                              ijcell3d(icell,jcell,1) = num_ijcell3d
                              idx_ijcell = num_ijcell3d
                           endif
                           emiss3d(idxhr,i,idx_ijcell) =
     &                          emiss3d(idxhr,i,idx_ijcell) +
     &                                         spctmp * plumefrac(1)
                        endif
                   else
c
c   --- both surface and elevated outputs ---
c
                       if( aglfrac(idxbin,idxhr) > 0. ) then
c
c   --- include fraction in surface assigned a plume ---
c
                          emselv(i,idxstk,idxhr) = 
     &                             emselv(i,idxstk,idxhr) 
     &                           + spcout(i) * elvfrac(idxbin,ihr)
                          emselv(i,idxstk+1,idxhr) = 
     &                             emselv(i,idxstk+1,idxhr) 
     &                           + spcout(i) * aglfrac(idxbin,ihr)
                          spcelv(i) = spcelv(i) 
     &                           + spcout(i) * elvfrac(idxbin,ihr)
     &                           + spcout(i) * aglfrac(idxbin,ihr)
                          emssrf(idxhr,i,jcell,icell) = 
     &                             emssrf(idxhr,i,jcell,icell) 
     &                           + spcout(i) * srffrac(idxbin,ihr)
                          spcsrf(i) = spcsrf(i) 
     &                           + spcout(i) * srffrac(idxbin,ihr)
                          stktop(idxstk,idxhr) = plmovrtop(idxbin,ihr)
                          stkbot(idxstk,idxhr) = plmovrbot(idxbin,ihr)
                          stktop(idxstk+1,idxhr) = srftop(idxbin,ihr)
                          stkbot(idxstk+1,idxhr) = srfbot(idxbin,ihr)
                          if( l3demiss ) then
                             spctmp = spcout(i) * elvfrac(idxbin,ihr)
     &                                  + spcout(i) * aglfrac(idxbin,ihr)
                             call add_3demiss( icell,jcell,idxhr,
     &                          stkbot(idxstk,idxhr),stktop(idxstk,idxhr), 
     &                                                         i, spctmp )
                             spctmp = spcout(i) * srffrac(idxbin,ihr)
                             call add_3demiss( icell,jcell,idxhr,
     &                         stkbot(idxstk+1,idxhr),stktop(idxstk+1,idxhr), 
     &                                                             i, spctmp )
                          endif
                       else
c
c  --- no surface assigned to plume ---
c
                          emselv(i,idxstk,idxhr) = 
     &                             emselv(i,idxstk,idxhr) 
     &                           + spcout(i) * elvfrac(idxbin,ihr)
                          spcelv(i) = spcelv(i) 
     &                           + spcout(i) * elvfrac(idxbin,ihr)
                          emssrf(idxhr,i,jcell,icell) = 
     &                             emssrf(idxhr,i,jcell,icell) 
     &                           + spcout(i) * srffrac(idxbin,ihr)
                          spcsrf(i) = spcsrf(i) 
     &                           + spcout(i) * srffrac(idxbin,ihr)
                          stktop(idxstk,idxhr) = plmovrtop(idxbin,ihr)
                          stkbot(idxstk,idxhr) = plmovrbot(idxbin,ihr)
                          if( l3demiss ) then
                             spctmp = spcout(i) * elvfrac(idxbin,ihr)
                             call add_3demiss( icell,jcell,idxhr,
     &                         stkbot(idxstk,idxhr),stktop(idxstk,idxhr), 
     &                                                         i, spctmp )
                          endif
                       endif
                   endif
                  endif ! valid hour for record
              enddo ! next hour
            endif ! emiss>0
         enddo ! next species
c
c  --- call routine to add to totals array ---
c
      call totpstfir(jerr,IDXOUT,crtsrf,spcsrf,ICDOUT,idxfip,
     &                                            scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call totpstfir(jerr,IDXSRF,crtsrf,spcsrf,0,idxfip,
     &                                            scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
c
      call totpstfir(jerr,IDXOUT,crtelv,spcelv,ICDOUT,idxfip,
     &                                            scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
c
      call totpstfir(jerr,IDXELV,crtelv,spcelv,0,idxfip,
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
c  ---- write the ascii point source files ---
c
      if ( lelvfl ) then
         call wrtelefir( jerr,
     &                   icurdt, emscrt, emselv, nhours )
         if( jerr .NE. ISUCES ) goto 9999
       endif
c
c  --- write the binary ptsrce file ---
c
      if ( lptsfl ) then
         call wrtptsrc( jerr,
     &                   icurdt, emselv, nhours )
         if( jerr .NE. ISUCES ) goto 9999
       endif
     
c
c  --- write the surface emissions file ---
c
      if ( .NOT.lelvall ) then
         call wrtuamfir( jerr,
     &                  icurdt ) 
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
      call totpstfir(jerr,IDXIN,emin,spcpol,ICDIN,MXFIP+1,
     &                                           scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call totpstfir(jerr,IDXSKP,emin,spcpol,0,0,scccd,invbeg,invend)
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
      call totpstfir(jerr,IDXERR,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call wrepstfir(jerr, nrec(IDXERR) )
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
      call totpstfir(jerr,IDXSKP,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
 6004 continue
      if( .NOT. l6004 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &              'records that have an invalid fire class.'
         write(ioerr,'(10X,A,I8)',ERR=9999) 'Fire class : ',idxbin
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(2A)',ERR=9999) 'Verify that the /FIRE BIN/ ',
     &              'packet was not changed following PREFIR run.'
         write(ioerr,'(2A)',ERR=9999) 'Fire class definitions must ',
     &              'be consistent through fire emissions processing.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6004 = .TRUE.
      call totpstfir(jerr,IDXERR,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call wrepstfir(jerr, nrec(IDXERR) )
      goto 222
c
 6005 continue
      if( .NOT. l6005 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                  'records that are not fire records.'
         write(ioerr,'(10X,2A)',ERR=9999) 'Type : ',rectyp
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                                    ' skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6005 = .TRUE.
      call totpstfir(jerr,IDXSKP,emin,spcpol,0,0,scccd,invbeg,invend)
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
