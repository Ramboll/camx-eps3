C**** WRTSTK.F 
C
      subroutine wrtstk( ierr,
     &                         nfac )
c
c-----------------------------------------------------------------------
c
c    Writes the stack report records for all stacks currently in arrays
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       nfac   I number of records currently in arrays
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
      include 'embrdat.inc'
      include 'ioprepnt.inc'
      include 'usrprpnt.inc'
      include 'prpntstk.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 nfac
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin   I   returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c----------------------------------------------------------------------
c    Local Parameters:
c----------------------------------------------------------------------
c
c   PAGWID  I  number of columns per page
c
      integer*4 PAGWID
c
      parameter( PAGWID = 999 )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   diamax  R  max diameter for all records with this stack
c   hitmax  R  max height for all records with this stack
c   ibeg    I  beginning of this stack
c   name    C  array of names of criteria pollutants
c   nline   I  number of lines in page
c   plumax  R  max plume height for all records with this stack
c   string  C  character string for each output record
c   tmpmax  R  max temperature for all records with this stack
c   totfac  R  array of total emissions for the stack
c   velmax  R  max exit velocity for all records with this stack
c
      character*(PAGWID) string
      character*10       name(MXCRIT)
      character*1        elevat
      integer*4          nline, ifac, ibeg, icnt, i, j
      real*4             totfac(MXCRIT), hitmax, diamax, tmpmax, velmax
      real*4             plumax
      logical*4          lfnd1, lfnd2, lwarn
c
c-----------------------------------------------------------------------
c   Save statements:
c-----------------------------------------------------------------------
c
      save nline, icnt, lwarn
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data icnt /1/
      data lwarn /.FALSE./
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- ititialize for first time ---
c
      do i=1,ncrthd
         totfac(i) = 0
         name(i) = crtnam(i)
         call rgtjst( name(i) )
      enddo
      ibeg = 1
c
c   --- write titles if this is first time through ---
c
      if( icnt .EQ. 1 ) then
         write(IOWSTK,9000,ERR=7000)'Stack parameters and emissions',
     &                          'Emissions reported for average day'
         write(IOWSTK,9003,ERR=7000)
     &            'NOTE: Negative numbers indicate defaults were used.'
         write(IOWSTK,9003,ERR=7000)
     &   'NOTE: A * indicates stacks treated as elevated point sources.'
         write(IOWSTK,9002,ERR=7000)
         write(IOWSTK,9001,ERR=7000) '  FIPS',' Plant','Stack',
     &          '      UTM','       UTM','      Stack','  Stack',
     &                                   '   Exit','   Exit','  Plume'
         write(IOWSTK,9001,ERR=7000) '  Code','  ID','   ID',
     &        '        East','     North','    Height','   Diam',
     &               '   Temp','  Veloc',' height',(name(i),i=1,ncrthd)
         write(IOWSTK,9001,ERR=7000) '      ','   ','    ',
     &          '          (km)','      (km)','      (m)','     (m)',
     &          '     K','   (m/s)','   (m)  ',('    tons ',i=1,ncrthd)
         write(IOWSTK,9002,ERR=7000) ('-',i=1,94+10*ncrthd)
         nline = 8
      endif
      icnt = icnt + 1
c
c   --- write each record to stack control file ---
c
      do ifac=2,nfac+1
c
c   --- if not the same stack write the last one ---
c
         
        lfnd1 = .TRUE.
        lfnd2 = .TRUE.
        if( instck(ibeg) .NE. instck(ifac) ) lfnd1 = .FALSE.
        if( stdiam(ibeg) .NE. stdiam(ifac) ) lfnd2 = .FALSE.
        if( stextp(ibeg) .NE. stextp(ifac) ) lfnd2 = .FALSE.
        if( stkht(ibeg) .NE. stkht(ifac) )  lfnd2 = .FALSE.
        if( stexvl(ibeg) .NE. stexvl(ifac) )  lfnd2 = .FALSE.
        if( xloc(ibeg) .NE. xloc(ifac)  ) lfnd2 = .FALSE.
        if( yloc(ibeg) .NE. yloc(ifac) ) lfnd2 = .FALSE.
c
c   --- if the stack has inconsistent write a message ---
c
        if( lfnd1 .AND. .NOT. lfnd2 .AND. .NOT. lwarn ) then
            write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
            write(ioerr,'(3A)',ERR=9999)'WARNING:  AFS file contains ',
     &        'stacks with matching ID but different stack parameters.'
            write(ioerr,'(10X,2A)',ERR=9999) 'County ID: ',infips(1)
            write(ioerr,'(10X,2A)',ERR=9999) 'Plant ID: ',inplnt(1)
            write(ioerr,'(10X,2A)',ERR=9999) 'Stack ID: ',instck(ibeg)
            write(ioerr,'(25X,2A)',ERR=9999)
     &                       'First Record    Second Record'
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999)
     &                          'Diameter  ',stdiam(ibeg),stdiam(ifac)
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999)
     &                            'Height    ',stkht(ibeg),stkht(ifac)
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999)
     &                          'Exit Temp ',stextp(ibeg),stextp(ifac)
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999)
     &                     'Exit Vel  ',stexvl(ibeg),stexvl(ifac)/3600.
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999)
     &                              'X-Coord   ',xloc(ibeg),xloc(ifac)
            write(ioerr,'(15X,A,F10.4,6X,F10.4)',ERR=9999)
     &                             'Y-Coord   ',yloc(ibeg),yloc(ifac)
            write(ioerr,'(3A)',ERR=9999)'These records will be ',
     &                                  'treated as separate stacks. '
            write(ioerr,'(3A)',ERR=9999)'This is just the first ',
     &                              'occurance.  There may be others.'
            write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
            lwarn = .TRUE.
            nwarn = nwarn + 1
         endif
c
c   --- first compute emissions for this stack ---
c
        if( .NOT. lfnd1 .OR. .NOT. lfnd2 ) then
           hitmax = RMISS * 1000
           diamax = RMISS * 1000
           tmpmax = RMISS * 1000
           velmax = RMISS * 1000
           plumax = RMISS * 1000
           do i=ibeg,ifac-1
              hitmax = MAX( hitmax, stkht(i) )
              diamax = MAX( diamax, stdiam(i) )
              tmpmax = MAX( tmpmax, stextp(i) )
              velmax = MAX( velmax, stexvl(i) )
              plumax = MAX( plumax, plmhit(i) )
              do j=1,ncrthd
                 if( crtpol(j,i) .GT. 0. ) totfac(j) = totfac(j) +
     &                                                     crtpol(j,i)
              enddo
           enddo
c
c  --- set stack parameters to maximum value ---
c
           do i=ibeg,ifac-1
              stkht(i) = hitmax
              stdiam(i) = diamax
              stextp(i) = tmpmax
              stexvl(i) = velmax
              plmhit(i) = plumax
           enddo
c
c   --- write out last stack ---
c
           if( plmhit(ifac-1) .GE. pcutof ) then
               elevat = '*'
           else
               elevat = ' '
           endif
           write(string,9004,ERR=7000) elevat,infips(1),
     &                      inplnt(1),instck(ifac-1),xloc(ifac-1),
     &                      yloc(ifac-1),stkht(ifac-1),stdiam(ifac-1),
     &                      stextp(ifac-1),stexvl(ifac-1),plmhit(ifac-1)
           do i=1,ncrthd
               j = 91 + (i-1)*10
               if( totfac(i) .EQ. 0. ) then
                   write(string(j:j+10),9005,ERR=7000) totfac(i)
               else if( totfac(i) .LT. 1000.0
     &                               .AND. totfac(i) .GT. 0.001 ) then
                   write(string(j:j+10),9006,ERR=7000) totfac(i)
               else
                   write(string(j:j+10),9007,ERR=7000) totfac(i)
               endif
               totfac(i) = 0.
           enddo
           write(IOWSTK,'(A)',ERR=7000) string(:strmin(string))
           nline = nline + 1
           ibeg = ifac
         endif
      enddo
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      ioerr = IOWMSG
      write(ioerr,'(/,A)',ERR=9999)   
     &                         'ERROR:  Writing stack report file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',stkfl(:strmin(stkfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(' ',/,T40,A,/,T39,A)
 9001 format(1X,A,1X,A,5X,A,5X,A,A,A,A,A,A,A,10(:,A10))
 9002 format(1X,A,250(:,A))
 9003 format(1X,A)
 9004 format(1X,A,1X,A,1X,A,1X,A,F10.3,F10.3,3X,F6.2,1X,F6.2,1X,F6.1,
     &                                               1X,F6.2,1X,F6.1)
 9005 format(F7.0,1X)
 9006 format(F7.3,1X)
 9007 format(E7.2,1X)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
