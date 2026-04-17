C**** PRCAFS.F 
c
      subroutine prcafs( ierr )
c
c-----------------------------------------------------------------------
c
c    Processes the AFS records for the TMPFAC program.  The temporal
c    profile data is read from each record of the AFS file and temporal
c    profiles are calculated.  The existing profiles are then searched
c    for a match with the calculated profiles.  If a match is found
c    the cross reference data is updated,  otherwise a new profile code
c    is created.
c
c    Argument Declaration.
c     Outputs:
c       ierr  I error flag
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
      include 'iotmpfac.inc'
      include 'tpfacprf.inc'
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
c    rgtjst  --  right justifies a character string (library)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   fndint   I  returns the index of an integer in an array of integers
c   fndfxf   I  returns the index of the data in the cross reference
c               arrays
c   strmin   I  returns the actual lenght of a string
c
      integer*4 fndint
      integer*4 fndfxf
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   NDFDY   I   number of default days per week codes
c   NDFHR   I   number of default hours per day codes
c
      integer*4 NDFDY
      integer*4 NDFHR
c
      parameter( NDFDY = 4 )
      parameter( NDFHR = 4 )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   iday    I   diurnal profile code for mid-week
c   idfday  I   default days per week codes
c   idfdpf  I   default profiles for the days per week value
c   idfhpf  I   default profiles for hours per day
c   idfhr   I   default hours per day codes
c   idxpol  I   index of pollutant code in pollutant arrays
c   idypwk  I   days per week operating
c   ihrpdy  I   hours per day operating
c   ihrpyr  I   hours per year operating
c   imonth  I   monthly profile code
c   infips  C   FIPS code read from record
c   inplnt  C   NEDS plant id read from record
c   inpol   I   pollutant code read from record
c   inscc   C   SCC code read from record
c   stkid   C   stack id read from record
c   inthru  I   annual throughput for seasons
c   iweek   I   day of week profile code
c   pctin   I   percentage for computed profile
c   point   C   point id code
c   string  C   AFS record as character string
c
      character*(2*MXSTR) string
      character*10 inscc, lngfip, inplnt, stkid, point
      character*5  infips, insbrg
      integer*4    inthru(4), ihrpdy, idypwk, ihrpyr, ibeghr
      integer*4    idfday(NDFDY), idfdpf(NDFDY,MXWEK+1), idfhr(NDFHR)
      integer*4    idfhpf(NDFHR,MXDAY+1), inpol, idxmch
      integer*4    idxpol, imonth, iweek, iday
      integer*4    idxprf, irec, inewrc, iskprc, jerr, i, j
      real*4       pctin(MXDAY), totmon
      logical*4    l6000, l6001
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      iskprc = 0
      inewrc = 0
      l6000 = .FALSE.
      l6001 = .FALSE.
c
c   --- initialize the default profiles ---
c
      idfday(1) = 2
      idfday(2) = 5
      idfday(3) = 6
      idfday(4) = 7
      do i=1,MXWEK
         if( i .EQ. IDXSAT ) then
              idfdpf(1,i) = 1
              idfdpf(2,i) = 0
              idfdpf(3,i) = 1
              idfdpf(4,i) = 1
         else if( i .EQ. IDXSUN ) then
              idfdpf(1,i) = 1
              idfdpf(2,i) = 0
              idfdpf(3,i) = 0
              idfdpf(4,i) = 1
         else
              idfdpf(1,i) = 0
              idfdpf(2,i) = 1
              idfdpf(3,i) = 1
              idfdpf(4,i) = 1
         endif
      enddo
      idfdpf(1,MXWEK+1) = 2
      idfdpf(2,MXWEK+1) = 5
      idfdpf(3,MXWEK+1) = 6
      idfdpf(4,MXWEK+1) = 7
      idfhr(1) = 8
      idfhr(2) = 12
      idfhr(3) = 16
      idfhr(4) = 24
      do i=1,MXDAY
         if( i .LE. 8 .OR. i .GE. 17 ) then
             idfhpf(1,i) = 0
         else
             idfhpf(1,i) = 1
         endif
         if( i .LE. 6 .OR. i .GE. 19 ) then
             idfhpf(2,i) = 0
         else
             idfhpf(2,i) = 1
         endif
         if( i .LE. 8 ) then
             idfhpf(3,i) = 0
         else
             idfhpf(3,i) = 1
         endif
         idfhpf(4,i) = 1
      enddo
      idfhpf(1,MXDAY+1) = 8
      idfhpf(2,MXDAY+1) = 12
      idfhpf(3,MXDAY+1) = 16
      idfhpf(4,MXDAY+1) = 24
c
c   --- reset percentages to zero ---
c
  111 continue
      do i=1,MXDAY
         pctin(i) = 0.
      enddo
c
c   --- read an AFS record ---
c
      read(IORAFS,'(A)',ERR=7000,END=444) string
      read(string,8000,ERR=7001) infips, insbrg, inscc, inplnt, stkid,
     &              point, (inthru(i),i=1,4), ihrpdy, ibeghr, idypwk,
     &                                                   ihrpyr, inpol
      irec = irec + 1
      call rgtjst( inscc )
      call lftjst( inplnt )
      call lftjst( insbrg )
      call lftjst( stkid )
      call lftjst( point )
      lngfip(1:5) = infips      
      lngfip(6:10) = insbrg
c
c   --- get pollutant index, if it does not exist add it ---
c
      idxpol = fndint ( inpol, ncdpol, ipolcd )
      if( idxpol .LE. 0 .AND. inpol .NE. 0 ) then
         ncdpol = ncdpol + 1
         if( ncdpol .GT. MXPOL ) goto 7002
         ipolcd(ncdpol) = inpol
         idxpol = ncdpol
      endif
c
c   --- call routine to find match in cross reference arrays ---
c
      imonth = IMISS
      iweek = IMISS
      iday = IMISS
      idxmch = fndfxf ( lngfip, inplnt, stkid, point, inscc, idxpol )
      if( idxmch .GT. 0 ) then
         imonth = imnxrf(idxmch,idxpol)
         iweek =  iwkxrf(idxmch,idxpol)
         iday = idyxrf(idxmch,idxpol)
      endif
c
c   --- compute the day of week profile ---
c
      idxprf = fndint ( idypwk, NDFDY, idfday )
      if( idxprf .EQ. 0 ) goto 6000
      do i=1,MXWEK
         pctin(i) = FLOAT(idfdpf(idxprf,i)) /
     &                             FLOAT(idfdpf(idxprf,MXWEK+1))
      enddo
c
c   --- call routine to find the existing profile ---
c
      call setwek ( jerr, iweek,
     &                          pctin )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- compute the monthly profiles ---
c
  222 continue
      totmon = 0
      do i=1,4
        totmon = totmon + inthru(i)
      enddo
      if( totmon .GT. 0. ) then
         do i=1,11
            j = INT(i/3) + 1
            pctin(i) = (FLOAT(inthru(j)) / 3.0) / totmon
         enddo
         pctin(12) = (FLOAT(inthru(1)) / 3.0) / totmon
      endif
c
c   --- call routine to find the existing profile ---
c
      call setmon ( jerr, imonth,
     &                            pctin )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- compute the diurnal profile ---
c
      do i=1,MXDAY
         pctin(i) = 0.
      enddo
      if( ibeghr .GE. 0 .and. ihrpdy .GT. 0 ) then
         do i=1,ihrpdy
            j = MOD(ibeghr+(i-1),MXDAY) + 1
            pctin(j) = 1.0 / FLOAT(ihrpdy)
         enddo
      else
         idxprf = fndint ( ihrpdy, NDFHR, idfhr )
         if( idxprf .EQ. 0 ) goto 6001
         do i=1,MXDAY
            pctin(i) = FLOAT(idfhpf(idxprf,i)) /
     &                                FLOAT(idfhpf(idxprf,MXDAY+1))
         enddo
      endif
c
c   --- call routine to find the existing profile ---
c
      call setday ( jerr, iday,
     &                          pctin )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- if source is not in arrays, add it ---
c
  333 continue
      if( idxmch .EQ. 0 ) then
          inewrc = inewrc + 1
          npfxrf = npfxrf + 1
          if( npfxrf .GT. MXREF ) goto 7003
          fipxrf(npfxrf) = lngfip
          facxrf(npfxrf) = inplnt
          stkxrf(npfxrf) = stkid
          pntxrf(npfxrf) = point
          sccxrf(npfxrf) = inscc
          idxmch = npfxrf
      endif
c
c   --- was not exact match, so if codes are not the same as current one,
c       create an exact match entry ---
c
      if( fipxrf(idxmch) .NE. lngfip .OR.
     &                         facxrf(idxmch) .NE. inplnt .OR.
     &                            stkxrf(idxmch) .NE. stkid .OR.
     &                                pntxrf(idxmch) .NE. point .OR.
     &                                  sccxrf(idxmch) .NE. inscc ) then
          if( imnxrf(idxmch,idxpol) .NE. imonth .OR.
     &            iwkxrf(idxmch,idxpol) .NE. iweek .OR.
     &                idyxrf(idxmch,idxpol) .NE. iday ) then
              inewrc = inewrc + 1
              npfxrf = npfxrf + 1
              if( npfxrf .GT. MXREF ) goto 7003
              fipxrf(npfxrf) = lngfip
              facxrf(npfxrf) = inplnt
              stkxrf(npfxrf) = stkid
              pntxrf(npfxrf) = point
              sccxrf(npfxrf) = inscc
              idxmch = npfxrf
          endif
c
c   --- was exact match so if codes are not the same, exit ---
c
      else
          if( imnxrf(idxmch,idxpol) .NE. imonth .AND. 
     &                       .NOT. lglob(idxmch,idxpol) .AND.
     &                    imnxrf(idxmch,idxpol) .NE. IMISS ) goto 6002
          if( iwkxrf(idxmch,idxpol) .NE. iweek .AND. 
     &                       .NOT. lglob(idxmch,idxpol) .AND.
     &                    iwkxrf(idxmch,idxpol) .NE. IMISS ) goto 6002
          if( idyxrf(idxmch,idxpol) .NE. iday .AND. 
     &                       .NOT. lglob(idxmch,idxpol) .AND.
     &                    idyxrf(idxmch,idxpol) .NE. IMISS ) goto 6002
      endif
c
c   --- put codes in array ---
c
      imnxrf(idxmch,idxpol) = imonth
      iwkxrf(idxmch,idxpol) = iweek
      idyxrf(idxmch,idxpol) = iday
      goto 111
c
c   --- processed entire file, echo to message file ---
c
  444 continue
      write(IOWMSG,'(/,T10,A,I8)',ERR=7001) 
     &                      'Number of AFS records processed: ',irec
      write(IOWMSG,'(/,T10,A,I8)',ERR=7001) 
     &                      'Number of AFS records skipped: ',iskprc
      write(IOWMSG,'(/,T10,A,I8)',ERR=7001)
     &    'Number of cross reference entries added after AFS file: ',
     &                                                          inewrc
c
c   --- set error flag to sucess ---
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
     &                     'records with invalid days per week code.'
         write(ioerr,'(10X,2A)',ERR=9999) 'FIPS code: ',infips
         write(ioerr,'(10X,2A)',ERR=9999) 'Plant id : ',inplnt
         write(ioerr,'(10X,2A)',ERR=9999) 'Stack id : ',stkid
         write(ioerr,'(10X,2A)',ERR=9999) 'Point id : ',point
         write(ioerr,'(10X,2A)',ERR=9999) 'SCC code : ',inscc
         write(ioerr,'(10X,A,I5)',ERR=9999) 'Days/week code: ',idypwk
         write(ioerr,'(5X,2A)',ERR=9999) 'Default values will be ',
     &                                     'used for all such records.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6000 = .TRUE.
      iskprc = iskprc + 1
      goto 222
c
 6001 continue
      if( .NOT. l6001 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  AFS file contains ',
     &                     'records with invalid hours per day code.'
         write(ioerr,'(10X,2A)',ERR=9999) 'FIPS code: ',infips
         write(ioerr,'(10X,2A)',ERR=9999) 'Plant id : ',inplnt
         write(ioerr,'(10X,2A)',ERR=9999) 'Stack id : ',stkid
         write(ioerr,'(10X,2A)',ERR=9999) 'Point id : ',point
         write(ioerr,'(10X,2A)',ERR=9999) 'SCC code : ',inscc
         write(ioerr,'(10X,A,I5)',ERR=9999) 'Hours/day code: ',ihrpdy
         write(ioerr,'(5X,2A)',ERR=9999) 'Default values will be ',
     &                                     'used for all such records.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      if( iday .LE. 0 ) then
         do i=1,MXPOL
           if( idyxrf(idxmch,i) .GT. 0 ) iday = idyxrf(idxmch,i)
         enddo
      endif
      l6001 = .TRUE.
      iskprc = iskprc + 1
      goto 333
c
 6002 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(/,A)',ERR=9999) 'WARNING:  Reading input AFS file.'
      write(ioerr,'(2A)',ERR=9999) 'Existing profile codes for source ',
     &                        'do not coincide with computed profiles.'
      write(ioerr,'(10X,2A)',ERR=9999) 'FIPS code: ',fipxrf(idxmch)
      write(ioerr,'(10X,2A)',ERR=9999) 'Plant id : ',facxrf(idxmch)
      write(ioerr,'(10X,2A)',ERR=9999) 'Stack id : ',stkxrf(idxmch)
      write(ioerr,'(10X,2A)',ERR=9999) 'Point id : ',pntxrf(idxmch)
      write(ioerr,'(10X,2A)',ERR=9999) 'SCC code : ',sccxrf(idxmch)
      write(ioerr,'(10X,A,3I5)',ERR=9999) 'Current profiles   : ',
     &                  imnxrf(idxmch,idxpol),iwkxrf(idxmch,idxpol),
     &                                         idyxrf(idxmch,idxpol)
      write(ioerr,'(10X,A,3I5)',ERR=9999) 'Calculated profiles: ',
     &                                           imonth, iweek, iday
      write(ioerr,'(2A)',ERR=9999) 'The current profiles will be ',
     &                                         'used for this source.'
      write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      goto 111
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
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading input AFS file.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &                                  'pollutant codes exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXPOL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXPOL ',
     &                                                  'and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Reading input AFS file.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &          'temporal profile cross reference records exceeded.'
      write(ioerr,'(A,I8)',ERR=9999) 'At AFS line: ',irec
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXREF
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXREF ',
     &                                                  'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(11X,2(A5,1X),5X,4(A10,1X),74X,4(I3,1X),I2,1X,
     &                                                2I2,1X,I4,1X,I5)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
