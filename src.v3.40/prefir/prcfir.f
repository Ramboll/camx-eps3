C**** PRCFIR.F
c
      subroutine prcfir( ierr )
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the PREFIR module.  
c    Input data is read, points are optionally combined, and output
c    EMBR records written.
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
      include 'firebin.inc'
      include 'usrprfir.inc'
      include 'ioprefir.inc'
      include 'prfirtot.inc'
      include 'prfirpnt.inc'
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
c   caljul       I   converts calander date to Julian data
c   fndhash_fips I   returns index of FIPS code in an hash table
c   fndint       I   returns index of and integer in an array
c   strmin       I   returns the actual length of a string (min of 1)
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
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   emstmp   R   temporary variable for criteria emissions
c   ibegcl   I   beginning date of season as calander day
c   ibegjl   I   beginning date of season as Julian day
c   icltmp   I   temporary variable for modelling cell containing point
c   idxfip   I   index of FIPS code in county list
c   idxpol   I   index of pollutant in EMBR arrays
c   iendcl   I   ending date of season as calander day
c   iendjl   I   ending date of season as Julian day
c   irec     I   number of input record read
c   jcltmp   I   temporary variable for modelling cell containing point
c   jerr     I   error flag for subroutine calls
c   ndays    I   number of days in season
c   string   C   character string for storing the input record as a string
c   xtmp     R   temporary variable for X coordinates of point
c   ytmp     R   temporary variable for Y coordinate of point
c
      character*(5*MXSTR) string
      character*5         infips
      character*2         inperod
      character*10        inscc, inplnt, instck, inpnt
      integer*4           irec, idxfip, ibegcl, iendcl, ibegjl, iendjl
      integer*4           idxpol, ndays, jerr, icltmp, jcltmp
      integer*4           ibeghr, iendhr, idum, i, ndxcmb
      integer*4           icur, jcur, ifirst
      real*4              emstmp, xtmp, ytmp
      real*4              xlocin, ylocin, diamin, emsin
      integer*4           inryr, inbeg, inend, inzone, inpol, inbin
      logical*4           lvalid, l6000, l6001, l6002, l6005, leapyr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c     --- initialize error flag ---
c
      ierr = IFAIL
      l6000 = .FALSE.
      l6001 = .FALSE.
      l6002 = .FALSE.
      l6005 = .FALSE.
      inpnt = '          '
c
c     --- initialize EMBR variables that are not changed ---
c
      rectyp = 'F'
      sbrgn = ' '
      siccd = ' '
      point = ' '
      segno = ' '
      stkgev = -9.0
      stktmp = -9.0
      stkhgt = -9.0
c
c     --- read the first input record ---
c
      irec = 0
      ifirst = 0
  111 continue
      irec = irec + 1
      read(IORAFS,'(A)',ERR=7000,END=444) string
      read(string,8000,ERR=7001) infips, inscc, inplnt, instck,
     &         inperod, inbeg, inend, xlocin, ylocin, 
     &         inzone, diamin, inpol, emsin
c
      call rgtjst( inscc )
      call lftjst( inplnt )
      call lftjst( instck )
      call lftjst( inperod )
c
c     --- check for valid pollutant ---
c
      idxpol = fndint ( inpol, ncrthd, isarod )
      if( idxpol .LE. 0 ) goto 6001
c
c     --- skip if county not in region ---
c
      idxfip = fndhash_fips ( infips )
      if( idxfip .LE. 0 ) goto 6002
c
c     --- convert emissions to average day ---
c
      leapyr = .FALSE.
      if( MOD(INT(inbeg/1000000),4) .EQ. 0 ) leapyr = .TRUE.
      ndays = 1
      if ( ifirst .GT. 1 .AND. inperod .NE. fperiod ) goto 7002
      if( inperod .EQ. ANTYP ) then
           ndays = 365
           if( leapyr ) ndays = 366
           period = 'AN'
           ibegcl = INT(inbeg/1000000) * 10000 + 101
           iendcl = INT(inbeg/1000000) * 10000 + 1231
           inbeg = caljul ( ibegcl ) * 100
           inend = caljul ( iendcl ) * 100
      else if( inperod .EQ. POTYP .OR. inperod .EQ. PCTYP .OR.
     &              inperod .EQ. ADTYP .OR. inperod .EQ. SPTYP ) then
           period = inperod
           ibegcl = INT(inbeg/100)
           iendcl = INT(inend/100)
           ibeghr = inbeg - ibegcl * 100
           iendhr = inend - iendcl * 100
           if( inperod .NE. SPTYP ) then
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
           inbeg = ibegjl  * 100 + ibeghr
           inend = iendjl  * 100 + iendhr
      else
           goto 6000
      endif
c
c     --- adjust emissions to average day ---
c
      if( inperod .EQ. ANTYP .OR. inperod .EQ. SPTYP )
     &                           emsin = emsin / FLOAT(ndays)
c
c     --- store input emissions in arrays ---
c
      call totfir( jerr,IDXIN,idxpol,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotcty( jerr,ICDIN,idxfip,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotasc( jerr,ICDIN,inscc,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotfcd( jerr,ICDIN,idxfip,inscc,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothrs( jerr,inbeg,inend,ICDIN,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothcd( jerr,inscc,inbeg,inend,ICDIN,
     &                                                idxpol,0,emsin)
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotpid( jerr,ICDIN,idxfip,inplnt,instck,inpnt,
     &                                              idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
c     --- convert coordinates if they are in geodisic coordinates
c
      if( lgeods .AND. izone .GE. 0. ) then
          ylocin = -ABS(ylocin)
          call mapgtu( ylocin, xlocin, izone, idum, xtmp, ytmp )
          xlocin = xtmp
          ylocin = ytmp
      endif
c
c     --- convert coordinates if they are UTM with different zone ---
c
      if( .NOT. lgeods .AND. inzone .NE. izone ) then
          call maputg( xlocin, ylocin, inzone, ytmp, xtmp )
          call mapgtu( ytmp, xtmp, izone, idum, xlocin, ylocin )
      endif
      inzone = izone
c
c     --- check for point inside modeling region ---
c
      call setij( icltmp, jcltmp,
     &                            xlocin, ylocin, 0.)
      if( icltmp .LE. 0 .OR.  jcltmp .LE. 0 .OR. 
     &    icltmp .GT. nxcell .OR.  jcltmp .GT. nycell ) then
         call totfir( jerr,IDXSKP,idxpol,emsin )
         if( jerr .NE. ISUCES ) goto 9999
         call totfir( jerr,IDXREG,idxpol,emsin )
         if( jerr .NE. ISUCES ) goto 9999
         goto 111
      endif
c
c     --- set fire class bin ---
c     --- check for invalid fire acreage, and replace invalid ---
c     --- data with default ---
c
      inbin = 0
      if(  diamin .le. 0.0 ) then
         if( .NOT. lprces ) goto 6005
         inbin = ibindf
      else
         do i = 1, nfbin
            if ( diamin .LT. sizfbin(i) ) then
               inbin = i
               goto 222
            endif
         enddo
      endif
      if ( inbin .EQ. 0 )  inbin = nfbin
c
  222 continue
c
c     --- passed all data checks so add emissions to output totals ---
c
      call totfir( jerr,IDXOUT,idxpol,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotcty( jerr,ICDOUT,idxfip,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotasc( jerr,ICDOUT,inscc,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotfcd( jerr,ICDOUT,idxfip,inscc,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothrs( jerr,inbeg,inend,ICDOUT,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothcd( jerr,inscc,inbeg,inend,ICDOUT,
     &                                             idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotpid( jerr,ICDOUT,idxfip,inplnt,instck,inpnt,
     &                                              idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- check if this is the same source as last record ---
c
      if ( ifirst .EQ. 1 ) then
         fperiod = inperod
         ifirst = 99
         goto 555
      endif
      if( curfips .EQ. infips .AND.
     &      curscc .EQ. inscc .AND.
     &        curplnt .EQ. inplnt .AND.
     &           curstck .EQ. instck .AND.
     &              curbin .EQ. inbin .AND.
     &                curbegdt .EQ. inbeg .AND.
     &                  curenddt .EQ. inend .AND.
     &                    xloccur .EQ. xlocin .AND.
     &                      yloccur .EQ. ylocin .AND.
     &                        izoncur .EQ. inzone ) then
           curems(idxpol) = curems(idxpol) + emsin
           goto 111
c
c     --- new source, so save or output the previous one ---
c
      else
c
c         --- combining sources by grid cell ---
c
          if ( lcomb ) then
             ndxcmb = 0
             call getcell( icur, jcur,
     &                        xloccur, yloccur )
             ijcur = (icur*10000) + jcur 
             do i = ncmb, 1, -1
                if ( ijcur .EQ. ijcmb(i) .AND.
     &                 curfips .EQ. cmbfips(i) .AND.
     &                   curscc .EQ. cmbscc(i) .AND.
     &                      curbin .EQ. ibincmb(i) .AND.
     &                        curbegdt .EQ. ibegcmb(i) .AND.
     &                          curenddt .EQ. iendcmb(i)  ) then
                     ndxcmb = i
                     goto 333
                endif
             enddo
c            --- not found, add to list ---
             ncmb = ncmb + 1
             if ( ncmb .GT. MXPTS ) goto 7003
             ndxcmb = ncmb
             ijcmb(ncmb) = ijcur
             cmbfips(ncmb) = curfips
             cmbscc(ncmb) = curscc
             ibincmb(ncmb) = curbin
             ibegcmb(ncmb) = curbegdt
             iendcmb(ncmb) = curenddt
             pntscmb(ncmb) = 0
             do i = 1, ncrthd
                emiscmb(i,ncmb) = 0.0
             enddo
c
  333        continue
             pntscmb(ndxcmb) = pntscmb(ndxcmb) + 1
             do i = 1, ncrthd
                emiscmb(i,ndxcmb) = emiscmb(i,ndxcmb) + curems(i)
             enddo
c
c         --- not combining sources, write each point to EMBR ---
c
          else
             call wrrfir( jerr )
             if( jerr .NE. ISUCES ) goto 9999
          endif
      endif
c
c     --- move the just read data to "current" fields ---
c
  555 continue
      curfips = infips
      curscc= inscc
      curplnt = inplnt
      curstck = instck
      curbegdt = inbeg
      curenddt = inend
      xloccur = xlocin
      yloccur = ylocin
      inzone = inzone
      ijcur = (icltmp*1000) + jcltmp
      curbin = inbin
      do i = 1, ncrthd
         curems(i) = 0.0
      enddo
      curems(idxpol) = curems(idxpol) + emsin
c
c     --- get next record ---
c
      goto 111
c
c     --- done processing file, save last record and ---
c     --- write current array values ---
c
  444 continue
      if ( lcomb ) then
         ndxcmb = 0
         call getcell( icur, jcur,
     &                        xloccur, yloccur )
         ijcur = (icur*10000) + jcur 
         do i = ncmb, 1, -1
            if ( ijcur .EQ. ijcmb(i) .AND.
     &             curfips .EQ. cmbfips(i) .AND.
     &               curscc .EQ. cmbscc(i) .AND.
     &                  curbin .EQ. ibincmb(i) .AND.
     &                    curbegdt .EQ. ibegcmb(i) .AND.
     &                      curenddt .EQ. iendcmb(i)  ) then
                 ndxcmb = i
                 goto 666
            endif
         enddo
c        --- not found, add to list ---
         ncmb = ncmb + 1
         if ( ncmb .GT. MXPTS ) goto 7003
         ndxcmb = ncmb
         ijcmb(ncmb) = ijcur
         cmbfips(ncmb) = curfips
         cmbscc(ncmb) = curscc
         ibincmb(ncmb) = curbin
         ibegcmb(ncmb) = curbegdt
         iendcmb(ncmb) = curenddt
         pntscmb(ncmb) = 0
         do i = 1, ncrthd
            emiscmb(i,ncmb) = 0.0
         enddo

  666    continue
         pntscmb(ndxcmb) = pntscmb(ndxcmb) + 1
         do i = 1, ncrthd
            emiscmb(i,ndxcmb) = emiscmb(i,ndxcmb) + curems(i)
         enddo
      endif
c
      call wrrfir(jerr)
      if( jerr .NE. ISUCES ) goto 9999
c
c     --- write number of records to message file ---
c
      write(IOWMSG,'(/,A,I8)',ERR=7005) 
     &                 'Number of input records read: ',nrec(IDXIN)
      write(IOWMSG,'(/,A,I8)',ERR=7005)
     & 'Number of input records written to EMAR: ',nrec(IDXERR)
      write(IOWMSG,'(/,A,I8)',ERR=7005)
     &                 'Number of input records skipped: ',nrec(IDXSKP)
      write(IOWMSG,'(/,A,I8)',ERR=7005)
     &                'Number of input to output records: ',nrec(IDXOUT)
      write(IOWMSG,'(/,A,I8)',ERR=7005)
     &                'Number of output EMBR records: ',nrec(IDXMBR)
c
c    --- set error flag to sucess ---
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
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  Input file contains ',
     &                                          'invalid period flags.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,A,A10)',ERR=9999) 'Invalid flag: ',inperod
         write(ioerr,'(A)',ERR=9999) 'Record will be skipped.'
         write(ioerr,'(A)',ERR=9999)
     &                'Emissions will not be included in output tables.'
         write(ioerr,'(A)',ERR=9999)
     &                       'Check error file for all skipped records.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6000 = .TRUE.
      call wrefir(jerr,string)
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
      call wrefir(jerr,string)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6002 continue
      if( .NOT. l6002 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  Input file contains ',
     &                           'counties that are not in the USERIN.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,2A)',ERR=9999) 'Unlisted FIPS: ',infips
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ',
     &                                    'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6002 = .TRUE.
      call totfir( jerr,IDXIN,idxpol,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotcty( jerr,ICDIN,MXFIP+1,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotasc( jerr,ICDIN,inscc,idxpol,0,emsin )
c
      call addtotfcd( jerr,ICDIN,MXFIP+1,inscc,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothrs( jerr,inbeg,inend,ICDIN,idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothcd( jerr,inscc,inbeg,inend,ICDIN, 
     &                                            idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotpid( jerr,ICDIN,MXFIP+1,inplnt,instck,inpnt,
     &                                              idxpol,0,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call totfir( jerr,IDXSKP,idxpol,emsin )
      if( jerr .NE. ISUCES ) goto 9999
c
      call totfir( jerr,IDXREG,idxpol,emsin )
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6005 continue
      if( .NOT. l6005 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  Input file contains ',
     &                       'records with invalid virtual acreage.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,2A)',ERR=9999) 'County ID : ',infips
         write(ioerr,'(10X,2A)',ERR=9999) 'Plant ID  : ',inplnt
         write(ioerr,'(10X,2A)',ERR=9999) 'Stack ID  : ',instck
         write(ioerr,'(10X,A,F10.3)',ERR=9999) 'Acreage  : ',diamin
         write(ioerr,'(2A)',ERR=9999) 'All such records will be ',
     &                                     'written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6005 = .TRUE.
      call totfir( jerr,IDXERR,idxpol,emsin )
      if( jerr .NE. ISUCES ) goto 9999
      call wrefir(jerr,string)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &                'ERROR:  Reading input file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',afsfl(:strmin(afsfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &                'ERROR:  Reading input file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',afsfl(:strmin(afsfl))
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                         string(:strmin(string))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &       'ERROR:  Input FIRES data has multiple period types.'
      write(ioerr,'(A)',ERR=9999)
     &       'This feature is not supported for FIRES data.'
      write(ioerr,'(2A,//)',ERR=9999) 
     &      'Separate FIRES input data by period type and ',
     &          'process separately.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &       'ERROR:  Maximum number of combined points exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXPTS
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXPTTS',
     &                                                  'and recompile.'
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
 8000 format(A5,1X,3(A10,1X),A2,1X,2(I8,1X),
     &      2(F10.0,1X),I2,1X,F8.0,1X,I5,1X,F10.0)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
