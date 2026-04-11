C**** PRCPRA.F 
c
      subroutine prcpra( ierr )
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the prepna module.
c    The input AMS file is grouped and then each AMS record is read.
c    We check to see if we need to disag.
c    Finally, EMBR records are written to area and mobile files.
c    Error records are written in EMAR format using wrepra
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
      include 'cntylst.inc'
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'iopream.inc'
      include 'pream.inc'
      include 'preamtot.inc'
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
c   totpra   adds emissions to total arrays
c   wrepra   writes error records
c   wrrpra   writes each record
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   caljul       I   converts calander date to Julian data (library)
c   fndhash_fips I   returns index of FIPS code in an hash table (library)
c   fndint       I   returns index of and integer in an array (library)
c   strmin       I   returns the actual length of a string (library)
c
      integer*4 caljul
      integer*4 fndhash_fips
      integer*4 fndint
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    local prameters:
c-----------------------------------------------------------------------
c
c   MXARC   I   maximum number of AMS records for a single source
c
      integer*4 MXARC
c
      parameter(MXARC = 250 )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   curasc   C   current ASC
c   curfip   C   current FIP
c   emstmp   R   temporary variable for criteria emissions
c   ibegcl   I   beginning date of season as calendar day
c   ibegjl   I   beginning date of season as Julian day
c   idxfip   I   index of FIPS code in county list
c   idxpol   I   index of pollutant in EMBR arrays
c   idxref   I   index of ASC in cross-reference
c   iendcl   I   ending date of season as calendar day
c   iendjl   I   ending date of season as Julian day
c   istcnt   I   string count
c   jerr     I   error flag for subroutine calls
c   leof     L   .TRUE. if at end of input file
c   ndays    I   number of days in season
c   newstr   C   character string for one AMS record as a string
c   string   C   character string for storing the all AMS record
c                with the same FIP/ASC pair
c
      character*(MXAMS) string(MXARC), newstr, tmpstr
      character*10      curasc, newasc
      character*5       curfip, newfip, cursbr, newsbr
      character*2       curper
      integer*4         istcnt, ibegcl, iendcl, ibegjl, iendjl, ndays
      integer*4         ibeghr, iendhr, icurbg, icurnd, itmpdat, i
      integer*4         jerr, istpos, irec, idxpol, idxfip
      real*4            emsval(MXARC), emstmp
      logical*4         leof, l6000, l6001, l6002 , leapyr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      leof = .FALSE.
      l6000 = .FALSE.
      l6001 = .FALSE.
      l6002 = .FALSE.
c
c   --- EMBR variables that are not used ---
c
      ijcell = -9
      plant = ' '
      stkid = ' '
      point = ' '
      segno = ' '
      utme = -9.0
      utmn = -9.0
      iutmzn = -9
      stkgev = -9.0
      stkdia = -9.0
      stktmp = -9.0
      stkhgt = -9.0
c
c   --- initialize ---
c
      nmsgfcd = 0
      irec = 0
      istcnt = 0
  111 continue
c
c   --- read AMS record from file into string ---
c
      irec = irec + 1
      read(IORAMS,'(A)',ERR=7000,END=555) newstr
c
c   --- get data from string ---
c
      tmpstr = newstr
      read(newstr,8000,ERR=7001) irefyr, newfip, newsbr, newasc, iperod,
     &                                   ibegdt, ienddt, inpol, emstmp
      call lftjst( iperod )
      call rgtjst( newasc )
      if( irec .EQ. 1 ) then
          curfip = newfip
          cursbr = newsbr
          curasc = newasc
          icurbg = ibegdt
          icurnd = ienddt
          curper = iperod
      endif
c
c   --- calculate ndays to be used for converting to average day
c       make other adjustments to time variables ---
c
      ndays = 1
      leapyr = .FALSE.
      if( MOD(INT(ibegdt/1000000),4) .EQ. 0 ) leapyr = .TRUE.
      if( iperod .EQ. ANTYP ) then
           ndays = 365
           if( leapyr ) ndays = 366
           period = 'AN'
      else if( iperod .EQ. POTYP .OR. iperod .EQ. PCTYP .OR.
     &                   iperod .EQ. ADTYP .OR. iperod .EQ. SPTYP ) then
           period = iperod
           ibegcl = INT(ibegdt/100)
           iendcl = INT(ienddt/100)
           ibeghr = ibegdt - ibegcl * 100
           iendhr = ienddt - iendcl * 100
           if( iperod .NE. SPTYP ) then
              ibeghr = 0
              iendhr = 24
           endif
           itmpdat = ibegdt
           ibegjl = caljul ( ibegcl )
           if( ibegjl .EQ. IMISS ) goto 7003
           itmpdat = ienddt
           iendjl = caljul ( iendcl )
           if( ibegjl .EQ. IMISS ) goto 7003
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
c   --- check for county in region ---
c
      idxfip = fndhash_fips( newfip )
      if( idxfip .LE. 0 ) goto 6002
c
c   --- check if the same or different FIP/ASC pair and time period ---
c
      if(newfip .EQ. curfip .AND. newasc .EQ. curasc .AND.
     &            newsbr .EQ. cursbr .AND. ibegdt .EQ. icurbg .AND.
     &                 ienddt .EQ. icurnd .AND. iperod .EQ. curper) then
          istcnt = istcnt + 1
          if( istcnt .GT. MXARC ) goto 7004
          string(istcnt) = newstr
          emsval(istcnt) = emstmp
          goto 111
      else
          curfip = newfip
          cursbr = newsbr
          curasc = newasc
          icurbg = ibegdt
          icurnd = ienddt
          curper = iperod
      endif
  222 continue
c
c  --- loop over all strings for this FIPS/ASC pair ---
c
      do 10 istpos=1,istcnt
c
c   --- read AMS record from string into variables ---
c
         tmpstr = string(istpos)
         read(string(istpos),8001,ERR=7001) itype, irefyr, ibasyr,
     &                 inetyp, infips, insbrg, insic, inasc, iperod,
     &                                         ibegdt, ienddt, inpol
         call lftjst( iperod )
         call rgtjst( inasc )
c
c   --- compute beginning and ending date ---
c
         if( iperod .EQ. ANTYP ) then
              period = 'AN'
              ibegcl = INT(ibegdt/1000000) * 10000 + 101
              iendcl = INT(ibegdt/1000000) * 10000 + 1231
              ibegdt = caljul ( ibegcl ) * 100
              ienddt = caljul ( iendcl ) * 100
         else if( iperod .EQ. POTYP .OR. iperod .EQ. PCTYP .OR.
     &               iperod .EQ. ADTYP .OR. iperod .EQ. SPTYP ) then
              period = iperod
              ibegcl = INT(ibegdt/100)
              iendcl = INT(ienddt/100)
              ibeghr = ibegdt - ibegcl * 100
              iendhr = ienddt - iendcl * 100
              ibegjl = caljul ( ibegcl )
              iendjl = caljul ( iendcl )
              if( ibegjl .GT. 90000 .and. iendjl .LT. 1000 ) then
                 ibegjl = ibegjl + 1900000
                 iendjl = iendjl + 2000000
              endif
              if( iperod .NE. SPTYP ) then
                 ibeghr = 0
                 iendhr = 24
              endif
              if( ibeghr .EQ. 24 ) then
                   ibegjl = ibegjl + 1
                   ibeghr = 0
              endif
              if( iendhr .EQ. 24 ) then
                   iendjl = iendjl + 1
                   iendhr = 0
              endif
              ibegdt = ibegjl  * 100 + ibeghr
              ienddt = iendjl  * 100 + iendhr
         endif
c
c   --- get index of county in region ---
c
         idxfip = fndhash_fips( infips )
         if( idxfip .LE. 0 ) then
             string(istpos) = ' '
             emsval(istpos) = 0.
             goto 10
         endif
c
c   --- find index of pollutant code in array ---
c
         idxpol = fndint ( inpol, ncrthd, isarod )
         if( idxpol .LE. 0 ) then
             string(istpos) = ' '
             emsval(istpos) = 0.
             goto 10
         endif
c
c   --- store input emissions in arrays ---
c
         call totpra( jerr,IDXIN,idxpol,emsval(istpos) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotcty( jerr,ICDIN,idxfip,idxpol,0,emsval(istpos) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotasc( jerr,ICDIN,inasc,idxpol,0,emsval(istpos) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotfcd( jerr,ICDIN,idxfip,inasc,
     &                                     idxpol,0,emsval(istpos) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtothrs( jerr,ibegdt,ienddt,ICDIN,
     &                                     idxpol,0,emsval(istpos) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtothcd( jerr,inasc,ibegdt,ienddt,ICDIN,
     &                                     idxpol,0,emsval(istpos) )
         if( jerr .NE. ISUCES ) goto 9999
c
c   --- fill arrays by pollutant ---
c
         if( crtpol(idxpol) .LT. 0. ) crtpol(idxpol) = 0.
         crtpol(idxpol) = crtpol(idxpol) + emsval(istpos)
         string(istpos) = ' '
         emsval(istpos) = 0.
c
   10 continue
c
c  --- call routine to write the EMBR record ---
c
      call wrrpra(jerr, idxfip )
      if(jerr .NE. ISUCES) goto 9999
c
c  --- get next set of records
c      istcnt is 1 because we had a mis-match retrieve that newstr ---
c
      if (.NOT. leof) then
          istcnt = 1
          string(1) = newstr
          emsval(1) = emstmp
          goto 111
      endif
c
c  --- write number of records to message file ---
c
      write(IOWMSG,'(/,A,I8)',ERR=7002) 
     &            'Number of input records read: ',nrec(IDXIN)
      write(IOWMSG,'(/,A,I8)',ERR=7002)
     &            'Number of output EMBR records written: ',nrec(IDXOUT)
      write(IOWMSG,'(/,A,I8)',ERR=7002)
     &         'Number of input records written to EMAR: ',nrec(IDXERR)
      write(IOWMSG,'(/,A,I8)',ERR=7002)
     &                 'Number of records skipped: ',nrec(IDXSKP)
c
c  --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c
c  --- End-of-File reached, finish with last set of records ---
c
  555 continue
      leof = .TRUE.
      newfip = '     '
      goto 222
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
 6000 continue
      if( .NOT. l6000 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  AMS file contains ',
     &                                          'invalid period flags.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          tmpstr(:strmin(tmpstr))
         write(ioerr,'(10X,A,I10)',ERR=9999) 'Invalid flag: ',iperod
         write(ioerr,'(A)',ERR=9999) 'Record will be skipped.'
         write(ioerr,'(A)',ERR=9999) 
     &                'Emissions will not be included in output tables.'
         write(ioerr,'(A)',ERR=9999) 
     &                       'Check error file for all skipped records.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6000 = .TRUE.
      call wrepra(jerr,newstr)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6001 continue
      if( .NOT. l6001 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING: There are pollutants',
     &        ' that have no match in the /CRITERIA POLLUTANT/ packet.'
         write(ioerr,'(A,//,A,/)',ERR=9999) 'Line read: ',
     &                                          tmpstr(:strmin(tmpstr))
         write(ioerr,'(A,I10)',ERR=9999) 'Unmatched code: ',inpol
         write(ioerr,'(5X,2A)',ERR=9999) 'Record will be skipped. ',
     &                       'Check error file for all skipped records.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6001 = .TRUE.
      nrec(IDXIN) = nrec(IDXIN) + 1
      call wrepra(jerr,newstr)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6002 continue
      if( .NOT. l6002 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  AMS file contains ',
     &                           'counties that are not in the USERIN.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          tmpstr(:strmin(tmpstr))
         write(ioerr,'(10X,2A)',ERR=9999) 'Unlisted FIPS: ',newfip
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ', 
     &                                    'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6002 = .TRUE.
      call totpra( jerr,IDXIN,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call totpra( jerr,IDXSKP,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotcty( jerr,ICDIN,MXFIP+1,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotasc( jerr,ICDIN,newasc,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotfcd( jerr,ICDIN,MXFIP+1,newasc,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothrs( jerr,ibegdt,ienddt,ICDIN,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothcd( jerr,newasc,ibegdt,ienddt,ICDIN,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999) 
     &                'ERROR:  Reading input AMS file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',amsfl(:strmin(amsfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999) 
     &                'ERROR:  Reading input AMS file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',amsfl(:strmin(amsfl))
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                         tmpstr(:strmin(tmpstr))
      goto 9999
c
 7002 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A,I10,A,//)',ERR=9999)
     &          'ERROR: Invalid date read on AMS file: ',itmpdat,
     &               ' Format for the date fields should be yymmddhh'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,A)',ERR=9999)
     &       'ERROR:  Maximum number of records per source exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXARC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXARC ',
     &                                                  'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(1X,I2,5X,2(A5,1X),5X,A10,1X,A2,1X,2(I8,1X),I5,1X,F10.0)
 8001 format(A1,I2,1X,I2,A2,2(A5,1X),A4,1X,A10,1X,A2,1X,2(I8,1X),I5,12X,
     &                                            I3,2(1X,F6.0),1X,F5.0)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
