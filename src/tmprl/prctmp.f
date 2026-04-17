C**** PRTMP.F 
c
      subroutine prctmp( ierr )
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the TMPRL module.  One record is read
c    from an EMBR file and the emissions are temporally allocated based
c    on the emissions inventory interval and the episode interval
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamcntl.inc'
      include 'cntylst.inc'
      include 'msgtot.inc'
      include 'embrdat.inc'
      include 'hash.inc'
      include 'usrtmprl.inc'
      include 'iotmprl.inc'
      include 'tmprlprf.inc'
      include 'tmprltot.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines called:
c-----------------------------------------------------------------------
c
c     rdembr  --  reads one EMBR data record (library)
c     tottmp  --  stores emissions in totals arrays
c     wrembr  --  writes one EMBR data record (library)
c     wretmp  --  writes error record to EMAR file
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   fndhash_fips I returns index of FIPS code in an hash table
c   fndint       I returns the index of an integer in an array of integers
c   julcal       I converts a julian date to calander date (returns the
c                  calandar date
c   strmin       I returns the actual length of a string (min of 1)
c
      integer*4 fndhash_fips
      integer*4 fndint
      integer*4 julcal
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   adjday  R   adjustment factor for day of week adjustment
c   adjfac  R   array of cumalutive (sum of all adjusments) emissions
c               temporal adjustment factor by species and hour
c   adjmon  R   adjustment factor for monthly adjustment
c   emishr  R   array of emissions for each hour
c   emstmp  R   temporary array for storing criteria emissions
c   emstot  R   total emissions for the EMBR record
c   icalbg  I   beginning date of inventory as calander date
c   icalen  I   ending date of inventory as calander date
c   idxdiu  I   index of either mid-week or weekend in diurnal profiles
c               array
c   idxfip  I   index of input FIPS code in county list
c   idxhr   I   index of hour in diurnal profiles arrays
c   idxmn   I   index of monthly profile in temporal profiles array
c   idxscc  I   index of ASC-SCC code in cross reference array
c   idxwek  I   index of day of week in weekly profile arrays
c   iepbeg  I   begining date/time of episode
c   iepcal  I   episode date as calander date
c   iepend  I   ending date/time of episode
c   irec    I   number of last EMBR record read
c   jerr    I   error flag for subroutine calls
c   lflag   I   flag for determining if episode month is in inventory
c               period
c   ndays   I   number of days in inventory
c   nhours  I   number of hours in episode
c   nmonth  I   number of months covered by inventory
c   spchr   R   array of CBIV emissions for each hour
c   spctmp  R   temporary array for storing CBIV emissions
c   totwt   R   total of monthly profiles for inventory period
c   itzadj  I   time zone adjustment array
c
      character*10 lngfip
      integer*4 nhours, iepcal, iepmon, irec, idxfip, ndays, idxscc
      integer*4 iepbeg, iepend, nmonth, icalbg, icalen, ihour
      integer*4 imonth, idxmn, idxdiu, idxwek, idxtpl(MXCRIT)
      integer*4 idxhr, idxdy, jerr, ispc, icnt, idxspc, i
      integer*4 itzadj(8,8)
      real*4    emstmp(MXCRIT), spctmp(MXSPEC), totwt
      real*4    emshr(MXCRIT), spchr(MXSPEC)
      real*4    adjfac(MXHOUR,MXSPEC), adjmon, adjday, emstot
      real*4    adjlst(MXSPEC), wekavg, cntavg
      logical*4 lflag, lchang, ladjmon, ladjwek
      logical*4 l6000, l6001, l6002, l6003, l6004, l6005, l6006, leapyr
c
c-----------------------------------------------------------------------
c   Datat statements:
c-----------------------------------------------------------------------
c
      data itzadj / 0, 1, 2, 3, 1, 2, 3, 4,
     &             -1, 0, 1, 2, 0, 1, 2, 3,
     &             -2,-1, 0, 1,-1, 0, 1, 2,
     &             -3,-2,-1, 0,-2,-1, 0, 1,
     &             -1, 0, 1, 2, 0, 1, 2, 3,
     &             -2,-1, 0, 1,-1, 0, 1, 2,
     &             -3,-2,-1, 0,-2,-1, 0, 1,
     &             -4,-3,-2,-1,-3,-2,-1, 0 / 
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      l6000 = .FALSE.
      l6001 = .FALSE.
      l6002 = .FALSE.
      l6003 = .FALSE.
      l6004 = .FALSE.
      l6005 = .FALSE.
      l6006 = .FALSE.
      irec = 0
c
c   --- compute number of hours in episode, convert episode dates
c       to calander and compute month of episode ---
c
      nhours = (iendat-ibgdat)*24 + INT( (endtim-begtim)/100 )
      iepcal = julcal ( ibgdat )
      iepmon = INT( MOD(iepcal,10000) /100 )
      iepbeg = ibgdat*100 + INT(begtim/100)
      iepend = iendat*100 + INT(endtim/100)
c
c   --- read an EMBR record ---
c
  111 continue
      irec = irec + 1
      call rdembr(jerr,
     &                 IORMBR, irec )
      if( jerr .EQ. IEOD ) goto 222
      if( jerr .NE. ISUCES ) goto 9999
      ladjmon = .TRUE.
      ladjwek = .TRUE.
      if( period .EQ. POTYP .OR. period .EQ. PCTYP ) then
         ladjmon = .FALSE.
         if(idxday .NE. IDXSAT .AND. idxday .NE. IDXSUN) 
     &                                           ladjwek = .FALSE.
      endif
      if( period .EQ. ADTYP ) ladjmon = .FALSE.
c
c   --- set long FIPS code ---
c
      call lftjst( sbrgn )
      lngfip(1:5) = fipin
      lngfip(6:10) = sbrgn
c
c   --- set temporary emissions array, set output arrays 
c       to missing value ---
c
      do i=1,ncrthd
         emstmp(i) = emin(i)
         emout(i) = RMISS
      enddo
      do i=1,nspchd
         spctmp(i) = spcpol(i)
         spcout(i) = RMISS
      enddo
c
c   --- call routine to get index of FIPS code in county list ---
c
      idxfip = fndhash_fips ( fipin )
      if( idxfip .LE. 0 ) goto 6000
      call tottmp(jerr,IDXIN,emin,spcpol,ICDIN,
     &                                  idxfip,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- check for inventory that is already hourly
c       make sure ending date has hour 24 instead of hour 0 ---
c
      if( MOD(invend,100) .EQ. 0 ) invend = invend - 76
      if( ((invend-invbeg) .LE. 1) ) then
          if( invbeg .GE. iepbeg .AND. invend .LE. iepend ) then
              ihour = invbeg - iepbeg + 1
              do i=1,ncrthd
                 emout(i) = emstmp(i)
              enddo
              do i=1,nspchd
                 spcout(i) = spctmp(i)
              enddo
              call wrembr(jerr,
     &                          IOWMBR, nrec(IDXOUT) )
              if( jerr .NE. ISUCES ) goto 9999
              nrec(IDXOUT) = nrec(IDXOUT) + 1
              call tottmp(jerr,IDXOUT,emout,spcout,ICDOUT,
     &                                  idxfip,scccd,invbeg,invend)
              if( jerr .NE. ISUCES ) goto 9999
          else
              goto 6001
          endif
          goto 111
      endif
c
c   --- compute number of days in inventory ---
c
      ndays = INT(invend/100) - INT(invbeg/100) + 1
      leapyr = .FALSE.
      if( MOD(INT(invbeg/100000),4) .EQ. 0 ) leapyr = .TRUE.
      if( ndays .GT. 365 .AND. .NOT. leapyr ) ndays = ndays - 635
      if( ndays .GT. 366 .AND. leapyr ) ndays = ndays - 634
c
c   --- call routine to find match in cross reference array ---
c       different routines for point sources ---
c
      if( rectyp .EQ. 'E' .OR. rectyp .EQ. 'L' ) then
          call fndtxf( idxtpl, lngfip, plant, stkid, point, scccd )
      else
          call fndtasc( idxtpl, lngfip, scccd )
      endif
      do ispc=1,ncrthd
         idxscc = idxtpl(ispc)
         if( emstmp(ispc) .GT. 0. .AND. idxtpl(ispc) .LE. 0 ) goto 6002
      enddo
c
c   --- since temporal profiles are by pollutant, loop through each
c       criteria pollutant, skip if there is no emissions for the
c       pollutant  ---
c
      icnt = 0
      do 10 ispc=1,ncrthd
         if( emstmp(ispc) .LE. 0.0 ) goto 10
         idxscc = idxtpl(ispc)
         if( idxscc .LE. 0 ) goto 10
         icnt = icnt + 1
c
c   --- if this is not the first pollutant check for the same codes
c       and skip the processing if the codes are the same ---
c
         if( icnt .GT. 1 ) then
             if( imnxrf(idxscc,ispc) .EQ. imnxrf(idxscc,idxspc) .AND.
     &           iwkxrf(idxscc,ispc) .EQ. iwkxrf(idxscc,idxspc) .AND.
     &             idyxrf(idxscc,ispc) .EQ. idyxrf(idxscc,idxspc) ) then
                do ihour=1,MXHOUR
                   adjfac(ihour,ispc) = adjfac(ihour,idxspc) 
                enddo
                idxspc = ispc
                goto 10
             endif
         else
             idxspc = ispc
         endif
c
c   --- initialize adjustment factors ---
c
         adjmon = 1.0
         adjday = 1.0
c
c   --- only do monthly adjustment if the inventory is more than a month ---
c
         if( ndays .GT. 28 .AND. ladjmon ) then
c
c   --- get index of code in profiles array ---
c
             idxmn = fndint ( imnxrf(idxscc,ispc), nmncd, imncd )
             if( idxmn .EQ. 0 ) goto 6003
c
c   --- compute months covered by inventory ---
c
             icalbg = julcal ( INT(invbeg/100)  )
             icalen = julcal ( INT(invend/100)  )
             nmonth = INT(icalen/100) - INT(icalbg/100)
             if( nmonth .GT. MXMON ) nmonth = nmonth - 88  
             if( nmonth .LT. 0 ) nmonth = nmonth + MXMON
             if( nmonth .GT. MXMON  .OR. nmonth .LT. 0 ) goto 6004
c
c   --- loop through months, computing the total weight ---
c
             lflag = .FALSE.
             totwt = 0.
             do i=0,nmonth
                imonth = i + INT( MOD(icalbg,10000) /100 )
                if( imonth .GT. MXMON ) imonth = imonth - MXMON
                if( iepmon .EQ. imonth ) lflag = .TRUE.
                totwt = totwt + FLOAT( imnfac(idxmn,imonth) )
             enddo
c
c   --- if total weight is zero this plant is turned off during the
c       episode period or if flag is zero the episode month is not
c       in the inventory interval, should write error ---
c
             if( totwt .LE. 0. ) goto 6003
             if( .NOT. lflag ) goto 6001
c
c   --- set adjustment factor ---
c
             adjmon = FLOAT(nmonth+1) *
     &                               FLOAT(imnfac(idxmn,iepmon))/totwt
         endif
c
c   --- if inventory is longer than a day find index of code in
c       day-of-week profile arrays ---
c
         idxwek = 0
         if( ndays .GT.  1 .AND. ladjwek ) then
c
c   --- get index of code in profiles array ---
c
             idxwek = fndint ( iwkxrf(idxscc,ispc), nwkcd, iwkcd )
             if( idxwek .EQ. 0 ) goto 6005
         endif
c
c   --- calculate if this is mid-week or weekend ---
c
         idxdiu = IDXMID
         if( idxday .EQ. IDXSAT .OR. idxday .EQ. IDXSUN) idxdiu = IDXEND
c
c   --- get index of code in diurnal profiles array ---
c
  333    continue
         idxdy = 0
         do i=1,ndycd(idxdiu)
            if( idycd(idxdiu,i) .EQ. idyxrf(idxscc,ispc) ) idxdy = i
         enddo
c
c   --- if profile code not found either:
c             write error if weekday
c             change to weekday and try again if weekend ---
c
         if( idxdy .EQ. 0 ) then
              if( idxdiu .EQ. IDXMID ) then
                 goto 6006
              else
                 idxdiu = IDXMID
                 goto 333
              endif
         endif
c
c   --- set adjustment factor for this day of week ---
c
         if( idxwek .NE. 0 ) then
             if( iwkfac(idxwek,MXWEK+1) .EQ. 0 ) goto 6003
             if( .NOT. ladjmon ) then
                if( period .EQ. ADTYP ) then
                    adjday =  7.0 * FLOAT(iwkfac(idxwek,idxday)) /
     &                                  FLOAT( iwkfac(idxwek,MXWEK+1) )
                else
                   wekavg = 0.
                   cntavg = 0.
                   do i=IDXMON,IDXFRI
                      if( iwkfac(idxwek,i) .NE. 0 ) then
                         wekavg = wekavg + FLOAT(iwkfac(idxwek,i))
                         cntavg = cntavg + 1.0
                      endif
                   enddo
                   if( wekavg .GT. 0. ) adjday =  
     &                   FLOAT(iwkfac(idxwek,idxday)) / (wekavg/cntavg)
                 endif
             else
                adjday = adjmon *  7.0 *  FLOAT(iwkfac(idxwek,idxday)) /
     &                                  FLOAT( iwkfac(idxwek,MXWEK+1) )
             endif
         endif
c
c   --- loop over number of hours in episode ---
c
         do ihour=1,nhours
c
c   --- calculate diurnal adjustment factor for this hour ---
c
             if( idyfac(idxdiu,idxdy,MXDAY+1) .EQ. 0 ) goto 6003
             idxhr = MOD( INT(begtim/100)+ihour-1, MXDAY ) + 1
             if( idxhr .GT. MXDAY ) idxhr = MXDAY - idxhr
c
c   --- adjust for timezone ---
c
             idxhr = idxhr + ( itmzon(idxfip) - invtzn )
             if (idxhr .GT. 24) idxhr = idxhr - 24
             if (idxhr .LT. 1) idxhr = idxhr + 24
c
             adjfac(ihour,ispc) = adjday *
     &                    FLOAT(idyfac(idxdiu,idxdy,idxhr) ) /
     &                           FLOAT( idyfac(idxdiu,idxdy,MXDAY+1) )
c
c   --- next hour ---
c
         enddo
c
c   --- next pollutant ---
c
         idxspc = ispc
   10 continue
c
c   --- if no species had significant emissions, skip this record ---
c
      if( icnt .LE. 0 ) goto 111
c
c  --- add to totals and write the EMBR records ---
c
      invbeg = (ibgdat + INT((begtim/100)/24.0))
     &                         *100 + MOD( INT(begtim/100),24)
      invend = (ibgdat + INT( (begtim/100+1.0)/24.0))
     &                            *100 + MOD(INT(begtim/100)+1,24)
      do ihour=1,nhours
c
c   --- calculate index in mobile arrays ---
c
         idxhr = MOD( INT(begtim/100)+ihour-1, MXDAY ) + 1
         if( idxhr .GT. MXDAY ) idxhr = MXDAY - idxhr
c
c   --- adjust for timezone ---
c
         idxhr = idxhr + ( itmzon(idxfip) - invtzn )
         if (idxhr .GT. 24) idxhr = idxhr - 24
         if (idxhr .LT. 1) idxhr = idxhr + 24
c
c    --- check for same factor as last record, if so continue accumulating,
c        if different, write record and reset begin date/time ---
c
         if( ihour .GT. 1 ) then
            lchang = .FALSE.
            do ispc=1,ncrthd
              if( ABS( adjlst(ispc)-adjfac(ihour,ispc) )
     &                                      .NE. 0.) lchang = .TRUE.
            enddo
            if( lchang ) then
               emstot = 0.
               do i=1,ncrthd
                  if( emout(i) .GT. 0. ) emstot = emstot + emout(i)
               enddo
               if( emstot .GT. 0.) then
                   nrec(IDXOUT) = nrec(IDXOUT) + 1
                   call wrembr(jerr,
     &                               IOWMBR, nrec(IDXOUT) )
                   if( jerr .NE. ISUCES ) goto 9999
                   call tottmp(jerr,IDXOUT,emout,spcout,ICDOUT,
     &                                  idxfip,scccd,invbeg,invend)
                   if( jerr .NE. ISUCES ) goto 9999
               endif
c
c   --- calculate new inventory date/time for this hour ---
c
               invbeg = (ibgdat + INT((begtim/100+FLOAT(ihour-1))/24.0))
     &                         *100 + MOD( INT(begtim/100)+ihour-1,24)
               invend = (ibgdat + INT( (begtim/100+FLOAT(ihour))/24.0))
     &                            *100 + MOD(INT(begtim/100)+ihour,24)
c 
c   --- set output array to missing ---
c
               do i=1,ncrthd
                  emout(i) = RMISS
               enddo
               do i=1,nspchd
                  spcout(i) = RMISS
               enddo
            endif
         endif 
c
c   --- apply the factors to this hour ---
c
         emstot = 0.
         do 20 ispc=1,ncrthd
             emshr(ispc) = 0.
             if( emstmp(ispc) .LE. 0.0 ) goto 20
             if( emout(ispc) .LT. 0. ) emout(ispc) = 0.
             emshr(ispc) = emstmp(ispc) * adjfac(ihour,ispc)
             emout(ispc) = emout(ispc) + emshr(ispc) 
             emstot = emstot + emshr(ispc)
   20    continue
         do 30 ispc=1,nspchd
             spchr(ispc) = 0.
             if( spctmp(ispc) .LE. 0.0 ) goto 30
             if( spcout(ispc) .LT. 0. ) spcout(ispc) = 0.
             spchr(ispc) = spctmp(ispc) * adjfac(ihour,ispcrt(ispc))
             spcout(ispc) = spcout(ispc) + spchr(ispc)
   30    continue
c
c  ---  set last hour factors to current ---
c
         do ispc=1,ncrthd
            adjlst(ispc) = adjfac(ihour,ispc)
         enddo
c
c  --- set ending hour to current hour ---
c
         invend = (ibgdat + INT( (begtim/100+FLOAT(ihour))/24.0))
     &                            *100 + MOD(INT(begtim/100)+ihour,24)
      enddo
c
c  --- write the last hour record for the day ---
c
      emstot = 0.
      do i=1,ncrthd
          if( emout(i) .GT. 0. ) emstot = emstot + emout(i)
      enddo
      if( emstot .GT. 0 ) then
          nrec(IDXOUT) = nrec(IDXOUT) + 1
          call wrembr(jerr,
     &                      IOWMBR, nrec(IDXOUT) )
          if( jerr .NE. ISUCES ) goto 9999
c
c    --- add emissions to totals array if significant ---
c
          call tottmp(jerr,IDXOUT,emout,spcout,ICDOUT,
     &                                  idxfip,scccd,invbeg,invend)
          if( jerr .NE. ISUCES ) goto 9999
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
      write(IOWMSG,'(/,T10,A,I8)',ERR=7000) 
     &                    'Number of input records read: ',nrec(IDXIN)
      write(IOWMSG,'(/,T10,A,I8)',ERR=7000)
     &         'Number of input records written to EMAR: ',nrec(IDXEMR)
      write(IOWMSG,'(/,T10,A,I8)',ERR=7000)
     &                       'Number of skipped records: ',nrec(IDXSKP)
      write(IOWMSG,'(/,T10,A,I8)',ERR=7000)
     &                'Number of output records written: ',nrec(IDXOUT)
c
c  --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
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
      call tottmp(jerr,IDXIN,emin,spcpol,ICDIN,MXFIP+1,
     &                                         scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call tottmp(jerr,IDXSKP,emin,spcpol,0,
     &                                  idxfip,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6001 continue
      if( .NOT. l6001 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                'records that are not in the modeling episode.'
         write(ioerr,'(10X,A,I10)',ERR=9999) 'Beginning date: ',invbeg
         write(ioerr,'(10X,A,I10)',ERR=9999) 'Ending date   : ',invend
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be ',
     &             'skipped. Check error file for all skipped records.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6001 = .TRUE.
      call tottmp(jerr,IDXSKP,emin,spcpol,0,
     &                                  idxfip,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6002 continue
      if( .NOT. l6002 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &         'ASC/SCC codes that are not in the profile XREF file.'
         write(ioerr,'(10X,A,A10)',ERR=9999) 'ASC/SCC code: ',scccd
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6002 = .TRUE.
      call tottmp(jerr,IDXEMR,emin,spcpol,0,
     &                                  idxfip,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call wretmp(jerr,
     &                  nrec(IDXEMR) )
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6003 continue
      if( .NOT. l6003 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  Profile found with ',
     &                               'total episode activity of zero.'
         write(ioerr,'(10X,2A)',ERR=9999) 'ASC/SCC code: ',scccd
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6003 = .TRUE.
      call tottmp(jerr,IDXEMR,emin,spcpol,0,
     &                                  idxfip,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call wretmp(jerr,
     &                  nrec(IDXEMR) )
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6004 continue
      if( .NOT. l6004 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  Monthly profile ',
     &                   'in XREF file not found in profiles file.'
         write(ioerr,'(10X,2A)',ERR=9999) 'ASC/SCC code       : ',scccd
         write(ioerr,'(10X,A,I5)',ERR=9999) 'Montly profile code: ',
     &                                              imnxrf(idxscc,ispc)
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6004 = .TRUE.
      call tottmp(jerr,IDXEMR,emin,spcpol,0,
     &                                  idxfip,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call wretmp(jerr,
     &                  nrec(IDXEMR) )
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6005 continue
      if( .NOT. l6005 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  Weekday profile ',
     &                   'in XREF file not found in profiles file.'
         write(ioerr,'(10X,2A)',ERR=9999) 'ASC/SCC code       : ',scccd
         write(ioerr,'(10X,A,I5)',ERR=9999) 'Weekly profile code: ',
     &                                              iwkxrf(idxscc,ispc)
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6005 = .TRUE.
      call tottmp(jerr,IDXEMR,emin,spcpol,0,
     &                                  idxfip,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call wretmp(jerr,
     &                  nrec(IDXEMR) )
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6006 continue
      if( .NOT. l6006 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  Diurnal profile ',
     &                   'in XREF file not found in profiles file.'
         write(ioerr,'(10X,2A)',ERR=9999) 'ASC/SCC code       : ',scccd
         write(ioerr,'(10X,A,I5)',ERR=9999) 'Diurnal profile code: ',
     &                                              idyxrf(idxscc,ispc)
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6006 = .TRUE.
      call tottmp(jerr,IDXEMR,emin,spcpol,0,
     &                                  idxfip,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call wretmp(jerr,
     &                  nrec(IDXEMR) )
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
