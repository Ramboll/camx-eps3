C**** PRCPRP.F 
c
      subroutine prcprp( ierr )
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the PREPNT module.  Each AFS record
c    is read and the data is stored until a new facility is encountered.
c    Upon reaching a new facility, the total emissions from the last
c    facility is computed and written to each EMBR record.
c    When a new stack is encountered the stack data from the last stack
c    is written to the stack report file.
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
      include 'uamcntl.inc'
      include 'uamregn.inc'
      include 'cntylst.inc'
      include 'msgtot.inc'
      include 'embrdat.inc'
      include 'sccxref.inc'
      include 'usrprpnt.inc'
      include 'ioprepnt.inc'
      include 'prpntstk.inc'
      include 'prepntot.inc'
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
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   caljul   I   converts calander date to Julian data
c   fndhash_fips I   returns index of FIPS code in an hash table (library)
c   fndint   I   returns index of and integer in an array
c   strmin   I   returns the actual length of a string (min of 1)
c
      integer*4 caljul
      integer*4 fndhash_fips
      integer*4 fndint
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local prameters:
c-----------------------------------------------------------------------
c
c   AMTEMP  R   default ambient temperature for plume height algorithm
c
      real*4    AMTEMP
c
      parameter(AMTEMP = 293.0 )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   delhf    R   final plume rise
c   delhx    R   plume rise at downwind distance
c   distf    R   distance of final plume rise from source
c   dthdz    R   default potential lapse rate (for plume calculation)
c   dwdist   R   default down wind distance (for plume calculation)
c   emstmp   R   temporary variable for criteria emissions
c   flow     R   default volumetric flow rate (for plume calculation)
c   flux     R   buoyancy flux
c   heat     R   heat output of source
c   hitx     R   effective plume height at downwind distance
c   ibegcl   I   beginning date of season as calander day
c   ibegjl   I   beginning date of season as Julian day
c   icltmp   I   temporary variable for modelling cell containing point
c   idxfip   I   index of FIPS code in county list
c   idxpol   I   index of pollutant in EMBR arrays
c   idxrec   I   array of pointers into sorted AFS file
c   iendcl   I   ending date of season as calander day
c   iendjl   I   ending date of season as Julian day
c   irec     I   number of AFS record read
c   jcltmp   I   temporary variable for modelling cell containing point
c   jerr     I   error flag for subroutine calls
c   kst      I   default stability class (for plume calculation)
c   ndays    I   number of days in season
c   nfclty   I   number of records for the current facility
c   press    R   default ambient air pressure (for plume calculation)
c   string   C   character string for storing the AFS record as a string
c   temper   R   default ambient air temperature (for plume calculation)
c   wspeed   R   defualt wind speed (for plume calculation)
c   xtmp     R   temporary variable for X coordinates of point
c   ytmp     R   temporary variable for Y coordinate of point
c
      character*(5*MXSTR) string
      integer*4           irec, idxfip, ibegcl, iendcl, ibegjl, iendjl
      integer*4           idxpol, ndays, nfclty, jerr, icltmp, jcltmp
      integer*4           kst, idxpnt, ibeghr, iendhr, idum, i
      real*4              emstmp, xtmp, ytmp, tempst
      real*4              hitx, heat, flux, delhf, distf, delhx
      real*4              flow, wspeed, dwdist, dthdz, temper, press
      logical*4           lvalid, l6000, l6001, l6002, l6003, l6004
      logical*4           l6005, l6006, leapyr
c
c-----------------------------------------------------------------------
c   Entry Point:
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
c
c   --- read the first AFS record ---
c
      nfclty = 0
      irec = 0
  111 continue
      irec = irec + 1
      read(IORAFS,'(A)',ERR=7000,END=222) string
      read(string,8000,ERR=7001) irefyr, infips(0), insbrg(0), insic(0), 
     &         inscc(0), inplnt(0), instck(0), inpnt(0), inseg(0), 
     &         iperod, ibegdt(0), ienddt(0), xloc(0), yloc(0), 
     &         inzone(0), stkht(0), stdiam(0), stextp(0), stexvl(0), 
     &                                                   inpol, emstmp
      call rgtjst( inscc(0) )
      call lftjst( inplnt(0) )
      call lftjst( insbrg(0) )
      call lftjst( instck(0) )
      call lftjst( inpnt(0) )
      call lftjst( iperod )
c
c   --- convert emissions to average day ---
c
      leapyr = .FALSE.
      if( MOD(INT(ibegdt(0)/1000000),4) .EQ. 0 ) leapyr = .TRUE.
      ndays = 1
      if( iperod .EQ. ANTYP ) then
           ndays = 365
           if( leapyr ) ndays = 366
           period = 'AN'
           ibegcl = INT(ibegdt(0)/1000000) * 10000 + 101
           iendcl = INT(ibegdt(0)/1000000) * 10000 + 1231
           ibegdt(0) = caljul ( ibegcl ) * 100
           ienddt(0) = caljul ( iendcl ) * 100
      else if( iperod .EQ. POTYP .OR. iperod .EQ. PCTYP .OR.
     &                   iperod .EQ. ADTYP .OR. iperod .EQ. SPTYP ) then
           period = iperod
           ibegcl = INT(ibegdt(0)/100)
           iendcl = INT(ienddt(0)/100)
           ibeghr = ibegdt(0) - ibegcl * 100
           iendhr = ienddt(0) - iendcl * 100
           if( iperod .NE. SPTYP ) then
              ibeghr = 0
              iendhr = 24
           endif
           ibegjl = caljul ( ibegcl )
           iendjl = caljul ( iendcl )
           if( ibegjl .GT. 90000 .and. iendjl .LT. 1000 ) then
               ibegjl = ibegjl + 1900000
               iendjl = iendjl + 2000000
           endif
           if( ibeghr .EQ. 24 ) then
                ibegjl = ibegjl + 1
                ibeghr = 0
           endif
           if( iendhr .EQ. 24 ) then
                iendjl = iendjl + 1
                iendhr = 0
           endif
           ndays = iendjl - ibegjl
           if( ndays .GT. 365 .AND. .NOT. leapyr ) ndays = ndays - 635
           if( ndays .GT. 366 .AND. leapyr ) ndays = ndays - 634
           if( ndays .LT. 1 ) ndays = 1
           ibegdt(0) = ibegjl  * 100 + ibeghr
           ienddt(0) = iendjl  * 100 + iendhr
      else
           goto 6000
      endif
c
c   --- check for valid pollutant ---
c
      idxpol = fndint ( inpol, ncrthd, isarod )
      if( idxpol .LE. 0 ) goto 6001
c
c   --- adjust emissions to average day ---
c
      if( iperod .EQ. ANTYP .OR. iperod .EQ. SPTYP )
     &                           emstmp = emstmp / FLOAT(ndays)
c
c   --- skip if county not in region or emissions insignificant ---
c
      idxfip = fndhash_fips ( infips(0) )
      if( idxfip .LE. 0 ) goto 6002
c
c   --- store input emissions in arrays ---
c
      call totprp( jerr,IDXIN,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotcty( jerr,ICDIN,idxfip,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotasc( jerr,ICDIN,inscc(0),idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotfcd( jerr,ICDIN,idxfip,inscc(0),idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotpid( jerr,ICDIN,idxfip,inplnt(0),instck(0),inpnt(0),
     &                                              idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothrs( jerr,ibegdt(0),ienddt(0),ICDIN,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothcd( jerr,inscc(0),ibegdt(0),ienddt(0),ICDIN,
     &                                                idxpol,0,emstmp)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- check for valid stack id code, valid point id means
c       "non-stacked source" ---
c
      lvalid = .TRUE.
      if( instck(0) .EQ. ' ' ) then
          if( inpnt(0) .NE. ' ' ) then
             plmhit(0) = 0.
             lvalid = .FALSE.
             goto 6003
          else
             goto 6004
          endif
      endif
c
c   --- convert coordinates if they are in geodisic coordinates
c
 333  continue
      if( lgeods .AND. izone .GE. 0. ) then
          yloc(0) = -ABS(yloc(0))
          call mapgtu( yloc(0), xloc(0), izone, idum, xtmp, ytmp )
          xloc(0) = xtmp
          yloc(0) = ytmp
      endif
c
c   --- convert coordinates if they are UTM with different zone ---
c
      if( .NOT. lgeods .AND. inzone(0) .NE. izone ) then
          call maputg( xloc(0), yloc(0), inzone(0), ytmp, xtmp )
          call mapgtu( ytmp, xtmp, izone, idum, xloc(0), yloc(0) )
      endif
      inzone(0) = izone
c
c   --- check for point inside modelling region ---
c
      call setij( icltmp, jcltmp,
     &                            xloc(0), yloc(0), 0. )
      if( icltmp .LE. 0 .OR. jcltmp .le. 0 .OR. icltmp .GT. nxcell .OR.
     &                                   jcltmp .GT. nycell ) then
         call totprp( jerr,IDXSKP,idxpol,emstmp )
         if( jerr .NE. ISUCES ) goto 9999
         call totprp( jerr,IDXREG,idxpol,emstmp )
         if( jerr .NE. ISUCES ) goto 9999
         goto 111
      endif
c
c   --- check for invalid stack parameters, and replace invalid
c       data with defaults ---
c
      if( stkht(0) .LE. 0.0 .OR. stdiam(0) .le. 0.0 .OR.
     &                 stextp(0) .LE. 0 .OR. stexvl(0) .le. 0.0 ) then
         if( .NOT. lprces ) goto 6005
         if( stkht(0) .LE. 0.0 ) stkht(0) = -dfhite
         if( stdiam(0) .LE. 0.0 ) stdiam(0) = -dfdiam
         if( stextp(0) .LE. 0.0 ) stextp(0) = -dftemp
         if( stexvl(0) .LE. 0.0 ) stexvl(0) = -dfvel
      endif
c
c    --- check for unreasonable exit velocity ---
c
      if ( stexvl(0) .GE. 1000. )  then
         if( .NOT. lprces ) goto 6005
         stexvl(0) = -dfvel
      endif
c
c   --- check for valid gas exit temperature ---
c
      tempst = ABS(stextp(0))
      if( lvalid .AND. tempst .LT. AMTEMP ) goto 6006
c
c   --- passed all data checks so add emissions to output totals ---
c
  444 continue
      call totprp( jerr,IDXOUT,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotcty( jerr,ICDOUT,idxfip,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotasc( jerr,ICDOUT,inscc(0),idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotpid( jerr,ICDOUT,idxfip,inplnt(0),instck(0),inpnt(0),
     &                                               idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotfcd( jerr,ICDOUT,idxfip,inscc(0),idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothrs( jerr,ibegdt(0),ienddt(0),ICDOUT,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothcd( jerr,inscc(0),ibegdt(0),ienddt(0),ICDOUT,
     &                                             idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if this is the first record then initialize counter ---
c
      if( nfclty .LE. 0 ) then
          nfclty = 1
          idxpnt = 1
c
c   --- set default met conditions ---
c
          flow = 0.
          kst = 2
          wspeed = 2.0
          dwdist = 0.
          dthdz = 0.
          temper = AMTEMP
          press = 0.
c
c   --- get plume height for this stack ---
c
          if( lvalid ) call beh072 (plmhit(0),hitx,heat,flux,delhf,
     &            distf,delhx,ABS(stkht(0)),tempst,ABS(stexvl(0)),
     &        ABS(stdiam(0)),flow,kst,wspeed,dwdist,dthdz,temper,press)
          goto 555
      endif
c
c   --- check if this is a different facility ----
c
      if( infips(0) .NE. infips(1) .OR.
     &                           insbrg(0) .NE. insbrg(1) .OR.
     &                              inplnt(0) .NE. inplnt(1) ) then
c
c   --- call routine to write stack report file ---
c
          call wrtstk( jerr,
     &                       nfclty )
          if( jerr .NE. ISUCES ) goto 9999
c
c   --- different facility, so call routine to output current arrays ---
c
          call wrrprp( jerr,
     &                       nfclty )
          if( jerr .NE. ISUCES ) goto 9999
c
c   --- set default met conditions ---
c
          flow = 0.
          kst = 2
          wspeed = 2.0
          dwdist = 0.
          dthdz = 0.
          temper = AMTEMP
          press = 0.
c
c   --- get plume height for this stack ---
c
          if( lvalid ) call beh072 (plmhit(0),hitx,heat,flux,delhf,
     &            distf,delhx,ABS(stkht(0)),tempst,ABS(stexvl(0)),
     &        ABS(stdiam(0)),flow,kst,wspeed,dwdist,dthdz,temper,press)
c
c   --- reset array counter ---
c
          nfclty = 1
          idxpnt = 1
          goto 555
      endif
c
c   --- same facility, look for existing point (search backward in
c       array for faster processing when data is grouped by stack ---
c
      idxpnt = 0
      do i=nfclty,1,-1
          if( insic(0) .EQ. insic(i) .AND.
     &            inscc(0) .EQ. inscc(i) .AND.
     &                 instck(0) .EQ. instck(i) .AND.
     &                        inpnt(0) .EQ. inpnt(i) .AND.
     &                            inseg(0) .EQ. inseg(i) .AND.
     &                              ibegdt(0) .EQ. ibegdt(i) .AND.
     &                                ienddt(0) .EQ. ienddt(i) ) then
              idxpnt = i
              goto 555
          endif
      enddo
c
c   --- set default met conditions ---
c
      flow = 0.
      kst = 2
      wspeed = 2.0
      dwdist = 0.
      dthdz = 0.
      temper = AMTEMP
      press = 0.
c
c   --- get plume height for this stack ---
c
      if( lvalid ) call beh072 (plmhit(0),hitx,heat,flux,delhf,
     &            distf,delhx,ABS(stkht(0)),tempst,ABS(stexvl(0)),
     &        ABS(stdiam(0)),flow,kst,wspeed,dwdist,dthdz,temper,press)
c
c   --- incrament counter for facility ---
c
      nfclty = nfclty + 1
      if( nfclty .GT. MXFAC ) goto 7003
      idxpnt = nfclty
c
c   --- fill arrays for current facility ---
c
  555 continue
      infips(1) = infips(0)
      insbrg(1) = insbrg(0)
      insic(idxpnt) = insic(0)
      inscc(idxpnt) = inscc(0)
      inplnt(1) = inplnt(0)
      instck(idxpnt) = instck(0)
      inpnt(idxpnt) = inpnt(0)
      inseg(idxpnt) = inseg(0)
      ibegdt(idxpnt) = ibegdt(0)
      ienddt(idxpnt) = ienddt(0)
      xloc(idxpnt) = xloc(0)
      yloc(idxpnt) = yloc(0)
      inzone(1) = inzone(0)
      stkht(idxpnt) = stkht(0)
      stdiam(idxpnt) = stdiam(0)
      stextp(idxpnt) = stextp(0)
      stexvl(idxpnt) = stexvl(0)
      crtpol(idxpol,idxpnt) = crtpol(idxpol,idxpnt) + emstmp
      plmhit(idxpnt) = plmhit(0)
c
c  --- add emissions to totals for elevated or low level ---
c
      if( plmhit(0) .GE. pcutof ) then
          call totprp( jerr,IDXELE,idxpol,emstmp )
      else
          call totprp( jerr,IDXLOW,idxpol,emstmp )
      endif
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- get next record ---
c
      goto 111
c
c  --- done processing file, write current array values ---
c
  222 continue
      call wrtstk(jerr,
     &                  nfclty )
      if( jerr .NE. ISUCES ) goto 9999
c
      write(IOWSTK,'(1X,250(:A))',ERR=7004) ('-',i=1,94+10*ncrthd)
      write(IOWSTK,'(1X,A)',ERR=7004)
     &            'NOTE: Negative numbers indicate defaults were used.'
      write(IOWSTK,'(1X,A)',ERR=7004)
     &   'NOTE: A * indicates stacks treated as elevated point sources.'
c
      call wrrprp(jerr,
     &                  nfclty )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- write number of records to message file ---
c
      write(IOWMSG,'(/,A,I8)',ERR=7005) 
     &                 'Number of input records read: ',nrec(IDXIN)
      write(IOWMSG,'(/,A,I8)',ERR=7005)
     & 'Number of input records written to EMAR: ',nrec(IDXERR)
      write(IOWMSG,'(/,A,I8)',ERR=7005)
     &                 'Number of input records skipped: ',nrec(IDXSKP)
      write(IOWMSG,'(/,A,I8)',ERR=7005)
     &                'Number of output records written: ',nrec(IDXOUT)
      write(IOWMSG,'(/,A,I8)',ERR=7005)
     &       'Number of elevated output records written: ',nrec(IDXELE)
      write(IOWMSG,'(/,A,I8)',ERR=7005)
     &       'Number of low level output records written: ',nrec(IDXLOW)
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
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  AFS file contains ',
     &                                          'invalid period flags.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,A,A10)',ERR=9999) 'Invalid flag: ',iperod
         write(ioerr,'(A)',ERR=9999) 'Record will be skipped.'
         write(ioerr,'(A)',ERR=9999)
     &                'Emissions will not be included in output tables.'
         write(ioerr,'(A)',ERR=9999)
     &                       'Check error file for all skipped records.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6000 = .TRUE.
      call wreprp(jerr,string)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6001 continue
      if( .NOT. l6001 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING: There are pollutants',
     &        ' that have no match in the /CRITERIA POLLUTANT/ packet.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(A,I10)',ERR=9999) 'Unmatched code: ',inpol
         write(ioerr,'(5X,2A)',ERR=9999) 'Record will be skipped. ',
     &                       'Check error file for all skipped records.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6001 = .TRUE.
      nrec(IDXIN) = nrec(IDXIN) + 1
      call wreprp(jerr,string)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6002 continue
      if( .NOT. l6002 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  AFS file contains ',
     &                           'counties that are not in the USERIN.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,2A)',ERR=9999) 'Unlisted FIPS: ',infips(0)
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ',
     &                                    'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6002 = .TRUE.
      call totprp( jerr,IDXIN,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotcty( jerr,ICDIN,MXFIP+1,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotasc( jerr,ICDIN,inscc(0),idxpol,0,emstmp )
c
      call addtotfcd( jerr,ICDIN,MXFIP+1,inscc(0),idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothrs( jerr,ibegdt(0),ienddt(0),ICDIN,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothcd( jerr,inscc(0),ibegdt(0),ienddt(0),ICDIN, 
     &                                            idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call totprp( jerr,IDXSKP,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call totprp( jerr,IDXREG,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6003 continue
      if( .NOT. l6003 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  AFS file contains ',
     &           'records with blank stack ID with non-blank point id.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,2A)',ERR=9999) 'County ID: ',infips(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Plant ID: ',inplnt(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Stack ID: ',instck(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Point ID: ',inpnt(0)
         write(ioerr,'(2A)',ERR=9999) 'All such records will be ',
     &                              'treated as "non-stacked" sources.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6003 = .TRUE.
      goto 333
c
 6004 continue
      if( .NOT. l6004 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  AFS file contains ',
     &               'records with blank stack ID with blank point id.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,2A)',ERR=9999) 'County ID: ',infips(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Plant ID: ',inplnt(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Stack ID: ',instck(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Point ID: ',inpnt(0)
         write(ioerr,'(2A)',ERR=9999) 'All such records will be ',
     &                                     'written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6004 = .TRUE.
      call totprp( jerr,IDXERR,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
      call wreprp(jerr,string)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6005 continue
      if( .NOT. l6005 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  AFS file contains ',
     &                       'records with invalid stack parameters.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,2A)',ERR=9999) 'County ID : ',infips(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Plant ID  : ',inplnt(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Stack ID  : ',instck(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Point ID  : ',inpnt(0)
         write(ioerr,'(10X,A,F10.3)',ERR=9999) 'Height    : ',stkht(0)
         write(ioerr,'(10X,A,F10.3)',ERR=9999) 'Diameter  : ',stdiam(0)
         write(ioerr,'(10X,A,F10.3)',ERR=9999) 'Exit Temp : ',stextp(0)
         write(ioerr,'(10X,A,F10.3)',ERR=9999) 'Exit Veloc: ',stexvl(0)
         write(ioerr,'(2A)',ERR=9999) 'All such records will be ',
     &                                     'written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6005 = .TRUE.
      call totprp( jerr,IDXERR,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
      call wreprp(jerr,string)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6006 continue
      if( .NOT. l6006 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(3A)',ERR=9999) 'WARNING:  AFS file contains ',
     &           'records with Gas Exit Temperature less than the ',
     &                                            'ambient temperature.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,2A)',ERR=9999) 'County ID: ',infips(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Plant ID: ',inplnt(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Stack ID: ',instck(0)
         write(ioerr,'(10X,2A)',ERR=9999) 'Point ID: ',inpnt(0)
         write(ioerr,'(10X,A,F10.3)',ERR=9999) 'Exit Temp : ',stextp(0)
         write(ioerr,'(2A)',ERR=9999) 'Using ambient temperature in ',
     &                    'plume height algorithm for all such records.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6006 = .TRUE.
      tempst = AMTEMP
      goto 444
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &                'ERROR:  Reading input AFS file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',afsfl(:strmin(afsfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &                'ERROR:  Reading input AFS file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',afsfl(:strmin(afsfl))
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                         string(:strmin(string))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &       'ERROR:  Maximum number of points per facility exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXFAC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXFAC ',
     &                                                  'and recompile.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing stack report file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',stkfl(:strmin(stkfl))
      goto 9999
c
 7005 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c
 8000 format(2X,I2,7X,2(A5,1X),A4,1X,4(A10,1X),A3,1X,A2,1X,2(I8,1X),
     &      2(F10.0,1X),I2,1X,4(F5.0,1X),29X,I5,1X,F10.0)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
