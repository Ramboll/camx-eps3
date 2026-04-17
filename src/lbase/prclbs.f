C**** PRCLBS.F 
c
      subroutine prclbs( ierr )
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the LBASE module.  Each link record
c    is read and a search is made in the disag data arrays for the
c    disaggragation of the input ASC code to the EPS2 motor vehicle
c    ASC codes.  Then the cells crossed by the link are calculated and
c    the emissions is distributed to these cells based on the total
c    fraction of link contain in the cell.  For each grid cell
c    crossed by a link, a gridded EMBR record is written for each EPS2
c    motor vehicle source category.
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
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'iolbase.inc'
      include 'usrlbase.inc'
      include 'lbasetot.inc'
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
c   mapgtu   converts geodesic coordinates to UTM coordinates
c
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   caljul      I   converts calander date to julian date
c   fndchr      I   returns the index of a string in an array of strings
c   strmin      I   returns the actual length of a string
c   fndhash_asc I   returns the index of the ASC code in hash table for
c                   totals arrays
c
      integer*4 caljul
      integer*4 fndchr
      integer*4 strmin
      integer*4 fndhash_asc
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   asclnk   A   ASC code read from links record
c   emslnk   R   temporary variable for criteria emissions
c   fiplnk   A   fips code read from links record
c   frac     R   fraction of total link distance in cell
c   ibegcl   I   inventory beginning date as calander date
c   ibeghr   I   inventory beginning hour
c   ibegjl   I   inventory beginning date as julian date
c   idxfip   I   index of fips code in county list
c   iemtyp   I   emissions type read from record
c   iendcl   I   inventory ending date as calander date
c   iendhr   I   inventory ending hour
c   iendjl   I   inventory ending date as julian date
c   irdtyp   I   road type read from links record
c   irec     I   number of link record read
c   ivclas   I   vehicle class read from links record
c   iznlnk   I   UTM zone of link
c   kcell    I   cells crossed by current link
c   nclnk    I   number of cells crossed by current link
c   siclnk   A   SIC code read from links record
c   string   C   link record as a character string
c   xcdbeg   R   beginning easting coordinate of link
c   xcdend   R   ending easting coordinate of link
c   ycdbeg   R   beginning northing coordinate of link
c   ycdend   R   ending northing coordinate of link
c
      character*(MXLRC) string
      character*10      asclnk
      character*5       fiplnk
      character*4       siclnk
      integer*4         irec, idxfip, iznlnk, ivclas
      integer*4         irdtyp, iemtyp, idxasc, nclnk, kcell(MXLKCL)
      integer*4         ibegcl, iendcl, ibegjl, iendjl, ibeghr, iendhr
      integer*4         icl, ndays, jerr, idum, i, j
      integer*4         nclcty(MXFIPCL), ijclcty(MXFIPCL,MXLCEL)
      integer*4         nperiod, idxper, ibgper(MXHOUR), ienper(MXHOUR)
      integer*4         idxcel
      real*4            xcdbeg, ycdbeg, xcdend, ycdend, emslnk(MXCRIT)
      real*4            alat, alon, frac(MXLKCL), total
      real*4            emscel(MXCRIT,MXLCEL,MXASCL,MXHOUR,MXFIPCL)
      logical*4         l6000, leapyr, lrepeat
c
c-----------------------------------------------------------------------
c   Local common block:
c-----------------------------------------------------------------------
c
       common /prclbsdat/ emscel
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- initialize the last variables ----
c
      fiplast = ' '
      xbeglast = -9.
      ybeglast = -9.
      xendlast = -9.
      yendlast = -9.
      ibeglast = -9
      iendlast = -9
c
c   --- initialize some global EMBR variables ---
c
      rectyp = 'M'
      plant = ' '
      stkid = ' '
      point = ' '
      segno = ' '
c
c   --- initialize local arrays ---
c
      nperiod = 0
      irec = 0
      do i=1,MXHOUR
        ibgper(i) = 0
        ienper(i) = 0
      enddo
      do i=1,MXFIPCL
        nclcty(i) = 0
        do j=1,MXLCEL
          ijclcty(i,j) = 0
        enddo
      enddo
      l6000 = .FALSE.
c
c   --- read the link record ---
c
  111 continue
      irec = irec + 1
      read(IORLNK,'(A)',ERR=7000,END=222) string
      read(string,8000,ERR=7001) fiplnk, siclnk, asclnk, ibegcl, iendcl,
     &                           xcdbeg, ycdbeg, xcdend, ycdend, iznlnk,
     &                           ivclas, irdtyp, iemtyp,
     &                           (emslnk(i), i=1,ncrthd)
      call rgtjst( asclnk )
      do i=1,ncrthd
        emslnk(i) = emslnk(i) * cvtunit
      enddo
c
c   ---- find the index of code in list, if not listed add it ---
c
      idxasc = fndhash_asc( jerr, asclnk )
      if( jerr .NE. ISUCES ) goto 9999
      if( idxasc .GT. MXASCL ) goto 7002
c
c   --- convert emissions to annual average day ---
c
      if( ibegcl .NE. ibeglast .OR. iendcl .NE. iendlast ) then
         ibeghr = MOD ( ibegcl,100)
         iendhr = MOD ( iendcl,100)
         ibegcl = INT( ibegcl /100 )
         iendcl = INT( iendcl /100 )
         ibegjl = caljul ( ibegcl )
         iendjl = caljul ( iendcl )
         ndays = iendjl - ibegjl + 1
         leapyr = .FALSE.
         if( MOD(INT(ibegcl/1000000),4) .EQ. 0 ) leapyr = .TRUE.
         if( ndays .GT. 365 .AND. .NOT. leapyr ) ndays = ndays - 635
         if( ndays .GT. 366 .AND. leapyr ) ndays = ndays - 634
         if( ndays .LT. 1 ) ndays = 1
         ibeglast = ibegcl
         iendlast = iendcl
         invbeg = ibegjl * 100 + ibeghr
         invend = iendjl * 100 + iendhr
      endif
      do i=1,ncrthd
         emslnk(i) = emslnk(i) / float(ndays)
      enddo
c
c  --- set some EMBR variables ---
c
      siccd = siclnk
c
c   --- find index of county in arrays ---
c
      if( fiplnk .NE. fiplast ) then
         fipin = fiplnk
         idxfip = fndchr (fiplnk,5,fipcd,ncdfip)
         if( idxfip .LE. 0 ) goto 6000
      endif
c
c   --- store input emissions in arrays ---
c
      call totlbs( jerr,IDXIN,idxfip,emslnk,asclnk,invbeg,invend )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- convert coordinates to kilometers ---
c
      xcdbeg = xcdbeg / 1000.
      ycdbeg = ycdbeg / 1000.
      xcdend = xcdend / 1000.
      ycdend = ycdend / 1000.
c
c   --- if not the same UTM zone, convert to modeling zone ---
c
      if( izone .NE. iznlnk ) then
          call maputg(xcdbeg, ycdbeg, iznlnk, alon, alat)
          call mapgtu(alon, alat, izone, idum, xcdbeg, ycdbeg)
          call maputg(xcdend, ycdend, iznlnk, alon, alat)
          call mapgtu(alon, alat, izone, idum, xcdend, ycdend)
      endif
c
c  --- call routine to get cells crossed by link ---
c      only do this is this is a new link segment ---
c
      lrepeat = .TRUE.
      if( xcdbeg .NE. xbeglast ) lrepeat = .FALSE.
      if( ycdbeg .NE. ybeglast ) lrepeat = .FALSE.
      if( xcdend .NE. xendlast ) lrepeat = .FALSE.
      if( ycdend .NE. yendlast ) lrepeat = .FALSE.
      if( .NOT. lrepeat ) then
          call gline(jerr,nclnk,kcell,frac,xcdbeg,ycdbeg,xcdend,ycdend)
          if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- find the index of this time period in the global arrays ---
c
      idxper = 0
      do i=1,nperiod
         if( invbeg .EQ. ibgper(i) .AND. 
     &                          invend .EQ. ienper(i) ) idxper = i
      enddo
      if( idxper .EQ. 0 ) then
         nperiod = nperiod + 1
         if( nperiod .GT. MXHOUR ) goto 7003
         idxper = nperiod
         ibgper(idxper) = invbeg
         ienper(idxper) = invend
      endif
c
c  --- loop through cells crossed by link ---
c
      do icl=1,nclnk
         ijcell = kcell(icl)
c
c  --- if this is a repeat link, use the array from last time ---
c
         if( fiplnk .EQ. fiplast .AND. lrepeat ) then
            idxcel = idxclast(icl)
c
c  --- otherwise, find the index for this cell in global array ---
c
         else
            idxcel = 0
            do i=1,nclcty(idxfip)
               if( ijclcty(idxfip,i) .EQ. ijcell ) idxcel = i
            enddo
            if( idxcel .EQ. 0 ) then
               nclcty(idxfip) = nclcty(idxfip) + 1
               if( nclcty(idxfip) .GT. MXLCEL ) goto 7004
               idxcel = nclcty(idxfip)
               ijclcty(idxfip,idxcel) = ijcell
            endif
         endif
c
c  --- calculate emissions in this cell and add to totals ---
c
         do i=1,ncrthd
            emout(i) = RMISS
            if( emslnk(i) .GT. 0. ) then
                emout(i) = emslnk(i) * frac(icl)
                emscel(i,idxcel,idxasc,idxper,idxfip) =
     &                 emscel(i,idxcel,idxasc,idxper,idxfip) + emout(i)
            endif
         enddo
c
c  --- fill the array for that last record processed ---
c
         idxclast(icl) = idxcel
      enddo
c
c  --- set the last record variables to the current --
c
      fiplast = fiplnk
      xbeglast = xcdbeg
      ybeglast = ycdbeg
      xendlast = xcdend
      yendlast = ycdend
c
c  --- get next record ---
c
      goto 111
c
c  --- set error flag to sucess ---
c
  222 continue
c
c  ---- entire file has been read, write out the aggregate EMBR records ---
c
      sbrgn = '     '
      siccd = '2201'
      period = 'S '
c
c   --- loop over counties, assign output FIPS code ---
c
      do idxfip=1,ncdfip
        fipin = fipcd(idxfip)
c
c   --- loop over cells in this county, assign cell ID ---
c
        do idxcel=1,nclcty(idxfip)
          ijcell = ijclcty(idxfip,idxcel)
c
c   --- loop over time periods, assign time stamps ---
c
           do idxper=1,nperiod
              invbeg = ibgper(idxper)
              invend = ienper(idxper)
c
c   --- loop over ASC codes, assign output code ---
c
              do idxasc=1,nmsgasc
                 scccd = msgasc(idxasc) 
c
c   --- if emissions is significant, write out a record ---
c
                 total = 0.0
                 do i=1,ncrthd
                   emout(i) = emscel(i,idxcel,idxasc,idxper,idxfip)
                   if( emout(i) .GT. 0. ) total = total + emout(i)
                 enddo
                 if( total .GT. 0 ) then
                    call wrembr( jerr,
     &                         IOWMBR, nrec(IDXOUT) )
                    if( jerr .NE. ISUCES ) goto 9999
                    call totlbs( jerr,IDXOUT,idxfip,emout,
     &                                          scccd,invbeg,invend )
                    if( jerr .NE. ISUCES ) goto 9999
                 endif
c
c    --- next ASC code ---
c
              enddo
c
c    --- next time period ---
c
           enddo
c
c    --- next grid cell ---
c
        enddo
c
c    --- next county ----
c
      enddo
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
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  AMS file contains ',
     &                           'counties that are not in the USERIN.'
         write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
         write(ioerr,'(10X,2A)',ERR=9999) 'Unlisted FIPS: ',fiplnk
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ',
     &                                    'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      call totlbs( jerr,IDXIN,MXFIP+1,emslnk,asclnk,invbeg,invend )
      if( jerr .NE. ISUCES ) goto 9999
      call totlbs( jerr,IDXSKP,MXFIP+1,emslnk,asclnk,invbeg,invend )
      if( jerr .NE. ISUCES ) goto 9999
      l6000 = .TRUE.
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &       'ERROR:  Reading input link emissions file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',linkfl(:strmin(linkfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &       'ERROR:  Reading input link emissions file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',linkfl(:strmin(linkfl))
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                         string(:strmin(string))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                'ERROR:  Processing input link emissions file.'
      write(ioerr,'(A)',ERR=9999)
     &                   'Number of source category codes exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed: ',MXASCL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXASCL ',
     &                                                  'and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                'ERROR:  Processing input link emissions file.'
      write(ioerr,'(A)',ERR=9999)
     &                 'Number of distinct times periods exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed: ',MXHOUR
      write(ioerr,'(A)',ERR=9999) 'Either separate the file by '
      write(ioerr,'(2A,//)',ERR=9999) 'day or increase the parameter ',
     &                                           'MXHOUR and recompile.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                'ERROR:  Processing input link emissions file.'
      write(ioerr,'(A)',ERR=9999)
     &                 'Number of grid cells in a county exceeds max.'
      write(ioerr,'(10X,2A)',ERR=9999) 'County code: ',fipcd(idxfip)
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed: ',MXLCEL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter ',
     &                                           'MXLCEL and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(A5,1X,A4,1X,A10,2(1X,I8),1X,4F10.0,4(1X,I2),1X,99(:,F10.0))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
