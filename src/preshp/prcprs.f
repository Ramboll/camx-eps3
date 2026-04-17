C**** PRCPRS.F 
c
      subroutine prcprs( ierr )
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the PRESHP module.  A record is read 
c    and the emissions are gridded based on the two seperate gridding
c    schemes. First, the cells along the centerline are discovered and
c    the emissions are gridded along these cells based on link length.
c    Then for each cell along the centerline the cells running along
c    the perpindicular are gridded as a single polynomial and the 
c    centerline cell emissions are spread across the cells in this 
c    polygon based on ratio of area of cell to area of the polygon.
c    An EMBR record is written for each cell that the shipping channel 
c    intersects.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c       mmj june 2014 corrected invesl() index
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
      include 'iopreshp.inc'
      include 'prshpdat.inc'
      include 'preshptot.inc'
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
      character*(5*MXSTR) string
      integer*4           irec, ndays, ibegcl, iendcl, ibegjl, iendjl
      integer*4           ibeghr, iendhr, idxfip, idxpol, jerr, nships
      integer*4           kcell(MXLKCL), idxshp, idum, nvert, i, j
      integer*4           nclnk, ilnk, icllnk, jcllnk, idxlast
      real*4              tmparea(MXCELX,MXCELY)
      real*4              channel_dist, slope_center, slope_perp
      real*4              angle, emsship, emstmp, emscel
      real*4              wtmp, xtmp, ytmp
      real*4              xvert(MXVERT), yvert(MXVERT), frac(MXLKCL)
      real*4              cell_dist, xclbeg, xclend, yclbeg, yclend
      real*4              tot_frac, celwidbeg, celwidend, channel_area
      real*4              rdummy1, rdummy2, rdummy3, rdummy4
      logical*4           leapyr, l6000, l6001, l6002, l6003, lwarn
      logical*4           ldummy
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
      lwarn = .FALSE.
c
c   --- read the first SHIPS record ---
c
      nships = 0
      irec = 0
      stkid = ' '
      point = ' '
  111 continue
      irec = irec + 1
      read(IORSHP,'(A)',ERR=7000,END=222) string
      read(string,8000,ERR=7001) infips(0), insbrg(0), inscc(0), 
     &                           invesl(0), iperod, ibegdt(0), 
     &                           ienddt(0), xlocbeg(0), ylocbeg(0), 
     &                           widebeg(0), xlocend(0), ylocend(0), 
     &                           wideend(0), inzone(0), inpol, emsship
      idxlast = idxfip
c
c  --- if there are no existing ships, set the variables ---
c
      nrec(IDXIN) = nrec(IDXIN) + 1
      call rgtjst( inscc(0) )
      call lftjst( invesl(0) )
      call lftjst( insbrg(0) )
      call lftjst( iperod )
c
c   --- swap the coordinates, if necessary ---
c
      if( xlocbeg(0) .GT. xlocend(0) ) then
         xtmp = xlocend(0)
         ytmp = ylocend(0)
         wtmp = wideend(0)
         xlocend(0) = xlocbeg(0)
         ylocend(0) = ylocbeg(0)
         wideend(0) = widebeg(0)
         xlocbeg(0) = xtmp
         ylocbeg(0) = ytmp
         widebeg(0) = wtmp
      endif
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
     &                           emsship = emsship / FLOAT(ndays)
      emstmp = emsship
c
c   --- skip if county not in region or emissions insignificant ---
c
      idxfip = fndhash_fips ( infips(0) )
      if( idxfip .LE. 0 ) goto 6002
c
c   --- store input emissions in arrays ---
c
      call totprs( jerr,IDXIN,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotcty( jerr,ICDIN,idxfip,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotasc( jerr,ICDIN,inscc(0),idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotpid( jerr,ICDIN,idxfip,invesl(0),stkid,point,
     &                                           idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotfcd( jerr,ICDIN,idxfip,inscc(0),idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothrs( jerr,ibegdt(0),ienddt(0),ICDIN,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtothcd( jerr,inscc(0),ibegdt(0),ienddt(0),ICDIN,
     &                                                idxpol,0,emstmp)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- convert coordinates if they are UTM with different zone ---
c
      if( inzone(0) .NE. izone ) then
          call maputg( xlocbeg(0), ylocbeg(0), inzone(0), ytmp, xtmp )
          call mapgtu( ytmp, xtmp, izone, idum, xlocbeg(0), ylocbeg(0) )
          call maputg( xlocend(0), ylocend(0), inzone(0), ytmp, xtmp )
          call mapgtu( ytmp, xtmp, izone, idum, xlocend(0), ylocend(0) )
      endif
      inzone(0) = izone
c
c   --- check if this is a different shipping channel ----
c
      if( infips(0) .NE. infips(1) .OR.
     &       insbrg(0) .NE. insbrg(1) .OR.
     &          xlocbeg(0) .NE. xlocbeg(1) .OR.
     &              ylocbeg(0) .NE. ylocbeg(1) .OR.
     &                widebeg(0) .NE. widebeg(1) .OR.
     &                   xlocend(0) .NE. xlocend(1) .OR.
     &                      ylocend(0) .NE. ylocend(1) .OR.
     &                              wideend(0) .NE. wideend(1) ) then
c
c   --- write out the existing data records ---
c
         if( nships .GT. 0 ) then
            if( idxlast .EQ. 0 ) idxlast = idxfip
            call wrrprs( jerr,
     &                          nships, idxlast )
            if( jerr .NE. ISUCES ) goto 9999
         endif
c
c   --- if this shipping channel is entered as a "point source",
c       write a warning and put all emissions into the
c       cell where the source is located ---
c
        if( xlocbeg(0) .EQ. xlocend(0) .AND. 
     &                 ylocbeg(0) .EQ. ylocend(0) .AND. 
     &                             widebeg(0) .EQ. wideend(0) ) then
            if( .NOT. lwarn ) then
               write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
               write(ioerr,'(3A)',ERR=9999) 'WARNING: SHIPS file ',
     &                       'contains records that have a shipping ',
     &                                        'channel with zero area.'
               write(ioerr,'(2A)',ERR=9999) 'All such records will ',
     &                                    'be gridded as point sources.'
               write(ioerr,'(2A,/)',ERR=9999) 'The first occurance of ',
     &                            'this type of record is listed below.'
               write(ioerr,'(A,/,I5)',ERR=9999) 'Line number: ', irec
               write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                         string(:strmin(string))
               write(ioerr,'(//,A)',ERR=9999) 
     &                                    'Shipping Channel Definition'
               write(ioerr,'(A,T20,A,2F15.5)',ERR=9999) 
     &                  'Coords of Beginning',':',xlocbeg(0), ylocbeg(0)
               write(ioerr,'(A,T20,A,F15.5)',ERR=9999) 
     &                               'Width at Beginning',':',widebeg(0)
               write(ioerr,'(A,T20,A,2F15.5)',ERR=9999) 
     &                     'Coords of Ending',':',xlocend(0), ylocend(0)
               write(ioerr,'(A,T20,A,F15.5)',ERR=9999) 
     &                                  'Width at Ending',':',wideend(0)
               write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
               lwarn = .TRUE.
               nwarn = nwarn + 1
            endif
            call setij(iclxbeg, jclybeg, xlocbeg(0), ylocbeg(0), 0. )
c
c  --- if this "point" is outside the domain, skip it ---
c
            if( iclxbeg .LE. 0 .OR. iclxbeg .GT. nxcell .OR. 
     &            jclybeg .LE. 0 .OR. jclybeg .GT. nycell ) goto 6003
c
c  --- otherwise set the gridding parameters ----
c
            iclxend = iclxbeg 
            jclyend = jclybeg 
            do i=1,MXCELX
              do j=1,MXCELY
                 gridarea(i,j) = 0.0
              enddo
            enddo
            gridarea(iclxbeg,jclybeg) = 1.0
        else
c
c   --- find the cells crossed by the centerline of the channel ---
c
            call gline(jerr,nclnk,kcell,frac,
     &                  xlocbeg(0),ylocbeg(0),xlocend(0),ylocend(0))
            if( jerr .NE. ISUCES ) goto 9999
c
c   ---  call vertices routine (this is just to get the distance
c        of centerline segment and the slope of the perindicular) ----
c
            call mkverts(nvert, xvert, yvert, shiparea, channel_dist, 
     &                            slope_center, slope_perp, angle,    
     &                             xlocbeg(0), ylocbeg(0), widebeg(0),
     &                              xlocend(0), ylocend(0), wideend(0) )
c
c   --- call routine to grid this channel ---
c
            call grarea( ierr, iclxbeg, iclxend, 
     &                       jclybeg, jclyend, gridarea,
     &                              xvert, yvert, nvert, shiparea )
c
c  --- only skip if it is completely outside the domain ----
c
            if( ierr .EQ. ISKIP ) goto 6003
            if( ierr .NE. ISUCES ) goto 7002
c
c   ---- initialize beginning of "cell" channel ----
c
            xclbeg = xlocbeg(0)
            yclbeg = ylocbeg(0)
            celwidbeg = widebeg(0)
            tot_frac = 0. 
            do i=1,nxcell
              do j=1,nycell
                gridarea(i,j) = 0.
              enddo
            enddo
c
c   --- walk down the cells crossed by the centerline ----
c
            do ilnk = 1,nclnk
               emstmp = emsship * frac(ilnk)
               cell_dist = channel_dist * frac(ilnk)
               tot_frac = tot_frac + frac(ilnk)
               celwidend = wideend(0) * tot_frac + 
     &                                 (widebeg(0) * (1.0-tot_frac) )
               icllnk = INT( kcell(ilnk)/1000 )
               jcllnk = MOD( kcell(ilnk), 1000 )
c
c   --- call routine to get the new distance and widths for 
c       the part of the channel that is crosses this cell ---
c
               call mkchannel( xclend, yclend, 
     &                          cell_dist, xclbeg, yclbeg, 
     &                              slope_center, slope_perp, angle )
c
c   --- call routine to calculate the vertices of the polygon that
c       makes the part of the channel intersecting this cell ---
c
               call mkverts(nvert, xvert, yvert, channel_area, 
     &                    rdummy1, rdummy2, rdummy3, rdummy4, 
     &                                 xclbeg, yclbeg, celwidbeg, 
     &                                      xclend, yclend, celwidend )
c
c   --- call routine to grid this channel ---
c
               call grarea( ierr, iclxbeg, iclxend, 
     &                       jclybeg, jclyend, tmparea,
     &                              xvert, yvert, nvert, channel_area )
c
c  --- only skip if it is completely outside the domain ----
c
               if( ierr .EQ. ISKIP ) then
                  nships = 0
                  goto 6003
               endif
               if( ierr .NE. ISUCES ) goto 7002
c
c  --- accumulate the gridding fractions, adjusting for 
c      the centerline distance
c
               do i=iclxbeg,iclxend
                 do j=jclybeg,jclyend
                    gridarea(i,j) = gridarea(i,j) + 
     &                        frac(ilnk) * tmparea(i,j) / channel_area 
                 enddo
               enddo
c
c   --- set beginning for this cell to end of last cell ---
c
               xclbeg = xclend
               yclbeg = yclend
               celwidbeg = celwidend
c
c   --- next cell along the centerline ----
c
            enddo 
        endif
c
c   --- reset array counter ----
c
         nships = 1
         idxshp = 1
         goto 555
      endif
c
c   --- same channel, look for existing ship (search backward in
c       array for faster processing when data is grouped by ship ---
c
         idxshp = 0
         do i=nships,1,-1
             if( inscc(0) .EQ. inscc(i) .AND.
     &                 invesl(0) .EQ. invesl(i) .AND.
     &                              ibegdt(0) .EQ. ibegdt(i) .AND.
     &                                ienddt(0) .EQ. ienddt(i) ) then
                 idxshp = i
                 goto 555
             endif
         enddo
c
c   --- incrament counter for channel ---
c
         nships = nships + 1
         if( nships .GT. MXSHIP ) goto 7003
         idxshp = nships
c
c   --- fill arrays for current channel ---
c
  555 continue
      infips(1) = infips(0)
      insbrg(1) = insbrg(0)
      inscc(idxshp) = inscc(0)
c   --- corrected index of (1) to (idxshp) jun 2014 mmj---
      invesl(idxshp) = invesl(0)
      ibegdt(idxshp) = ibegdt(0)
      ienddt(idxshp) = ienddt(0)
      xlocbeg(1) = xlocbeg(0)
      ylocbeg(1) = ylocbeg(0)
      widebeg(1) = widebeg(0)
      xlocend(1) = xlocend(0)
      ylocend(1) = ylocend(0)
      wideend(1) = wideend(0)
      inzone(1) = inzone(0)
      crtpol(idxpol,idxshp) = crtpol(idxpol,idxshp) + emsship
c
c  --- get next record ---
c
      goto 111
c
c  --- done processing file, write current array values ---
c
  222 continue
      if( idxlast .EQ. 0 ) idxlast = idxfip
      call wrrprs(jerr,
     &                  nships, idxlast )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- write number of records to message file ---
c
      write(IOWMSG,'(/,A,I8)',ERR=7004) 
     &                 'Number of input records read: ',nrec(IDXIN)
      write(IOWMSG,'(/,A,I8)',ERR=7004)
     & 'Number of input records written to error file: ',nrec(IDXERR)
      write(IOWMSG,'(/,A,I8)',ERR=7004)
     &                 'Number of input records skipped: ',nrec(IDXSKP)
      write(IOWMSG,'(/,A,I8)',ERR=7004)
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
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  SHIPS file contains ',
     &                                          'invalid period flags.'
         write(ioerr,'(A,/,I5)',ERR=9999) 'Line number: ', irec
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
      nrec(IDXERR) = nrec(IDXERR) + 1
      call wreprs(jerr,string)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6001 continue
      if( .NOT. l6001 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING: There are pollutants',
     &        ' that have no match in the /CRITERIA POLLUTANT/ packet.'
         write(ioerr,'(A,/,I5)',ERR=9999) 'Line number: ', irec
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(A,I10)',ERR=9999) 'Unmatched code: ',inpol
         write(ioerr,'(5X,2A)',ERR=9999) 'Record will be skipped. ',
     &                       'Check error file for all skipped records.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6001 = .TRUE.
      nrec(IDXERR) = nrec(IDXERR) + 1
      call wreprs(jerr,string)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6002 continue
      if( .NOT. l6002 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  SHIPS file contains ',
     &                           'counties that are not in the USERIN.'
         write(ioerr,'(A,/,I5)',ERR=9999) 'Line number: ', irec
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,2A)',ERR=9999) 'Unlisted FIPS: ',infips(0)
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ',
     &                                    'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6002 = .TRUE.
      nrec(IDXSKP) = nrec(IDXSKP) + 1
      call totprs( jerr,IDXIN,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotcty( jerr,ICDIN,MXFIP+1,idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call addtotasc( jerr,ICDIN,inscc(0),idxpol,0,emstmp )
c
      call addtotpid( jerr,ICDIN,MXFIP+1,invesl(0),' ',' ', 
     &                                           idxpol,0,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
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
      call totprs( jerr,IDXSKP,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call totprs( jerr,IDXREG,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
 6003 continue
      if( .NOT. l6003 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  SHIPS file contains ',
     &         'shipping lanes that are completely outside the domain.'
         write(ioerr,'(A,/,I5)',ERR=9999) 'Line number: ', irec
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(//,A)',ERR=9999)
     &                                    'Shipping Channel Definition'
         write(ioerr,'(A,T20,A,2F15.5)',ERR=9999)
     &                  'Coords of Beginning',':',xlocbeg(0), ylocbeg(0)
         write(ioerr,'(A,T20,A,F15.5)',ERR=9999)
     &                               'Width at Beginning',':',widebeg(0)
         write(ioerr,'(A,T20,A,2F15.5)',ERR=9999)
     &                     'Coords of Ending',':',xlocend(0), ylocend(0)
         write(ioerr,'(A,T20,A,F15.5,/)',ERR=9999)
     &                                  'Width at Ending',':',wideend(0)
         write(ioerr,'(A)',ERR=9999) 'All such records will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6003 = .TRUE.
      nrec(IDXERR) = nrec(IDXERR) + 1
      nrec(IDXSKP) = nrec(IDXSKP) + 1
      call wreprs(jerr,string)
      if( jerr .NE. ISUCES ) goto 9999
c
      call totprs( jerr,IDXSKP,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
c
      call totprs( jerr,IDXREG,idxpol,emstmp )
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &                'ERROR:  Reading input SHIPS file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',shipsfl(:strmin(shipsfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &                'ERROR:  Reading input SHIPS file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',shipsfl(:strmin(shipsfl))
         write(ioerr,'(A,/,I5)',ERR=9999) 'Line number: ', irec
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                         string(:strmin(string))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &                'ERROR:  Cannot grid the shipping channel.'
         write(ioerr,'(A,/,I5)',ERR=9999) 'Line number: ', irec
      write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                         string(:strmin(string))
      write(ioerr,'(//,A)',ERR=9999) 'Shipping Channel Definition'
      write(ioerr,'(A,T20,A,2F15.5)',ERR=9999) 'Coords of Beginning',
     &                                      ':',xlocbeg(0), ylocbeg(0)
      write(ioerr,'(A,T20,A,F15.5)',ERR=9999) 'Width at Beginning',
     &                                                  ':',widebeg(0)
      write(ioerr,'(A,T20,A,2F15.5)',ERR=9999) 'Coords of Ending',
     &                                      ':',xlocend(0), ylocend(0)
      write(ioerr,'(A,T20,A,F15.5)',ERR=9999) 'Width at Ending',
     &                                                  ':',wideend(0)
      write(ioerr,'(//,A)',ERR=9999) 'Vertices Calculated'
      do i=1,nvert
         write(ioerr,'(10X,2F15.5)',ERR=9999) xvert(i), yvert(i)
      enddo
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &       'ERROR:  Maximum number of ships per channel exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSHIP
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSHIP ',
     &                                                  'and recompile.'
      goto 9999
c
 7004 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c
 8000 format(A5,1X,A5,1X,A10,1X,A10,1X,A2,1X,I8,1X,I8,6(1X,F10.0),
     &       1X,I2,1X,I5,1X,F10.0)

c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
