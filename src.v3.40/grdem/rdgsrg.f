C***** RDGSRG.F 
c
      subroutine rdgsrg( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the gridded surrogate file and stores
c    the data in common variables to be used by the GRDEM program.
c    Since the gridded surrogate file is by UTM coordinates the grid
c    cell for each surrogate is calculatd and added to the totals for
c    the county.
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
c   May 2008  mjimenez  Added a header record to input file that
c                       specifies the character code for each surg
c                       column
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamregn.inc'
      include 'cntylst.inc'
      include 'hash.inc'
      include 'iogrdem.inc'
      include 'usrgrdem.inc'
      include 'grdemsrg.inc'
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
c   strmin   I  returns actual length of string
c   fndchr   I  returns the index of an string in an array of strings
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c  FUZZ    I  fizz for comparing floats
c
      real*4 FUZZ
c
      parameter( FUZZ = 0.001 )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   butme    R   minumum UTM easting corrdinate
c   butmn    R   minumum UTM easting corrdinate
c   eutme    R   minumum UTM easting corrdinate
c   eutmn    R   minumum UTM easting corrdinate
c   fiptmp   C   FIPS code read from file
c   icell    I   cell index of surrogate record in the X direction
c   iuzone   I   UTM zone of surrogate header data
c   iznsrg   I   UTM zone of surrogate record
c   jcell    I   cell index of surrogate record in the Y direction
c   surgin   R   percentages of county totals for each surrogate
c   xcdsrg   R   X coordinate of surrogate reocrd
c   xcelsz   R   easting cell size
c   ycdsrg   R   Y coordinate of surrogate record
c   ycelsz   R   northing cell size
c
      character*(5*MXSTR) string
      character*10        lngfip
      character*5         fiptmp, subtmp
      character*3         tmpcod
      integer*4           iuzone, irec, idxfip, icell, jcell, icl
      integer*4           iznsrg, idum, i, j, tmplen, jerr
      real*4              surgin(NSURG)
      real*4              xcdsrg, ycdsrg, butme, butmn, eutme, eutmn
      real*4              xcelsz, ycelsz, alat, alon
      logical*4           lerror, lcellij
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      nsrgrd = 0
      ncdsrg = 0
      irec = 0
      lcellij = .FALSE.
c
c   --- read the first line ---
c
   10 continue
      irec = irec + 1
      read(IORSRG,'(A)',ERR=7000,END=7001) string
      if ( string(1:1) .EQ. '#' )  goto 10
c
c   --- check if this surrogate file uses cell index ---
c
      if( string(1:5) .EQ. 'CELLS' ) then
         lcellij = .TRUE.
         string = string(6:)
      endif
c
c   --- read header records ---
c
   20 continue
      irec = irec + 1
      read(string,'(4F10.0,I10,2F10.0)',ERR=7002) butme, butmn, eutme, 
     &                                  eutmn, iuzone, xcelsz, ycelsz
      if ( string(1:1) .EQ. '#' )  goto 20
      butme = butme / 1000.
      butmn = butmn / 1000.
      eutme = eutme / 1000.
      eutmn = eutmn / 1000.
      xcelsz = xcelsz / 1000.
      ycelsz = ycelsz / 1000.
c
c   --- if zone is different, convert to modelling zone ---
c
      if( iuzone .NE. izone ) then
         call maputg(butme, butmn, iuzone, alon, alat)
         call mapgtu(alon, alat, izone, idum, butme, butmn)
         call maputg(eutme, eutmn, iuzone, alon, alat)
         call mapgtu(alon, alat, izone, idum, eutme, eutmn)
      endif
c
c   --- verify that regions are compatible ---
c
      lerror = .FALSE.
      if( xorig .LT. butme-FUZZ ) lerror = .TRUE. 
      if( yorig .LT. butmn-FUZZ ) lerror = .TRUE.
      if( (xorig + (nxcell-1)*dxcell) .GT. eutme + FUZZ) lerror = .TRUE.
      if( (yorig + (nycell-1)*dycell) .GT. eutmn + FUZZ) lerror = .TRUE.
      if( lerror ) goto 7003
c
      if( dxcell .LT. xcelsz-FUZZ ) lerror = .TRUE.
      if( dycell .LT. ycelsz-FUZZ ) lerror = .TRUE.
      if( lerror ) goto 7004
c
c   --- read surrogate code names ---
c
   30 continue
      irec = irec + 1
      read(IORSRG,'(A)',ERR=7000,END=7001) string
      if ( string(1:1) .EQ. '#' )  goto 30
c
c   --- check if this surrogate file uses cell index ---
c
      ncodes = 0
      if( string(1:5) .EQ. 'CODES' ) then
         lcodhdr = .TRUE.
         string = string(6:)
         call lftjst ( string )
         call low2up ( string )
  100    continue
         call token (jerr, tmpcod, string)
         if( jerr .EQ. IEOD ) goto 111
         ncodes = ncodes + 1
         if ( ncodes .GT. NSURG ) goto 7010
         codnm(ncodes) = tmpcod
         goto 100
      else
c
c     --- no header record found, set to numeric equivalent of column number --- 
c     --- set number of codes to the number of columns of factors ---
c
         lcodhdr = .FALSE.
         string = string(41:)
         tmplen = strmin( string )
         ncodes = INT( tmplen / 10 )
         if ( ncodes .GT. NSURG ) goto 7010
         do i = 1, ncodes
            write(codnm(i),'(I3)') i
            call lftjst ( codnm(i) )
         enddo
         backspace(IORSRG)
         irec = irec - 1
      endif
c
c   --- read next record ---
c
  111 continue
      irec = irec + 1
      read(IORSRG,'(A)',ERR=7005,END=222) string
      if ( string(1:1) .EQ. '#' )  goto 111
      read(string,8000,ERR=7006) fiptmp, subtmp, xcdsrg, ycdsrg, 
     &                                     iznsrg, (surgin(i),i=1,NSURG)
c
c   --- skip record if county is not in county list ---
c
      idxfip = fndchr ( fiptmp, 5, fipcd, ncdfip )
      if( idxfip .LE. 0 ) goto 111
c
c   --- if this file already has cell index, just set it ---
c 
      if( lcellij ) then
         icell = INT( xcdsrg )
         jcell = INT( ycdsrg )
c
c   --- otherwise calculate using the coordinates ---
c 
      else
         xcdsrg = xcdsrg / 1000.
         ycdsrg = ycdsrg / 1000.
c
c   --- if not the same utm zone, convert to modeling zone ---
c
         if( izone .NE. iznsrg ) then
             call maputg(xcdsrg, ycdsrg, iznsrg, alon, alat )
             call mapgtu(alon, alat, izone, idum, xcdsrg, ycdsrg )
         endif
c
c   --- call routine to get grid cell ---
c
         call setij(icell, jcell,
     &                         xcdsrg, ycdsrg, 0. )
      endif
c
c   --- find long FIPS code in list ---
c
      lngfip(1:5) = fiptmp
      lngfip(6:10) = subtmp
      idxfip = fndchr ( lngfip, 10, fipsrg, ncdsrg )
      if( idxfip .LE. 0 ) then
         ncdsrg = ncdsrg + 1
         if( ncdsrg .GT. MXFIP ) goto  7007
         fipsrg(ncdsrg) = lngfip
         idxfip = ncdsrg
      endif
c
c   --- this is the first cell in this county, initialize ---
c
      if( nsgcty(idxfip) .LE. 0 ) then
          nsrgrd = nsrgrd + 1
          if( nsrgrd .GT. MXSRG ) goto 7008
          nsgcty(idxfip) = 1
          ibegsg(idxfip) = nsrgrd
          iendsg(idxfip) = nsrgrd
          iclsrg(nsrgrd) = icell * 1000 + jcell
          do j=1,NSURG
             srgfrc(j,nsrgrd) = surgin(j)
          enddo
          goto 111
      endif
c
c   --- check if this cell is already in list (could have many surrogate
c       cells per modeling cell) ---
c
      do i=ibegsg(idxfip),iendsg(idxfip)
         if( iclsrg(i) .EQ. icell*1000+jcell ) then
             do j=1,NSURG
                srgfrc(j,i) = srgfrc(j,i) + surgin(j)
             enddo
             goto 111
         endif
      enddo
c
c   --- new cell, store data in array ---
c
      nsrgrd = nsrgrd + 1
      if( nsrgrd .GT. MXSRG ) goto 7008
      nsgcty(idxfip) = nsgcty(idxfip) + 1
      iendsg(idxfip) = nsrgrd
      iclsrg(nsrgrd) = icell * 1000 + jcell
      do j=1,NSURG
         srgfrc(j,nsrgrd) = surgin(j)
      enddo
c
c   --- read next record ---
c
      goto 111
c
c   --- processed entire file, echo to message file ---
c
  222 continue
      write(IOWMSG,'(A)',ERR=7009)
      write(IOWMSG,'(A)',ERR=7009)
      do i=1,ncdsrg
         write(IOWMSG,'(T10,3A,I8)',ERR=7009) 
     &      'Number of grid cells in county ',fipsrg(i),':',nsgcty(i)
c
c   --- add up the total of reach surrogate in each county
c       and save for later comparison ---
c
         do j=1,NSURG
            srgtot(i,j) = 0.0
         enddo
         do icl=ibegsg(i),iendsg(i)
           do j=1,NSURG
             srgtot(i,j) = srgtot(i,j) + srgfrc(j,icl)
           enddo
         enddo
      enddo
      write(IOWMSG,'(A)',ERR=7009)
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
      write(ioerr,'(/,A)',ERR=9999)
     &              'ERROR:  Reading header of surrogate data file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',srgfl(:strmin(srgfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A)',ERR=9999)
     &              'ERROR:  Reading header of surrogate data file.'
      write(ioerr,'(2A)',ERR=9999)  'File: ',srgfl(:strmin(srgfl))
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A)',ERR=9999)
     &              'ERROR:  Reading header of surrogate data file.'
      write(ioerr,'(2A)',ERR=9999) 'File: ',srgfl(:strmin(srgfl))
      write(ioerr,'(A)',ERR=9999) 'Line read: '
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A)',ERR=9999)
     &              'ERROR:  Reading header of surrogate data file.'
      write(ioerr,'(2A)',ERR=9999) 'Region covered by gridded ',
     &          'surrogate file does not contain the modeling domain.'
      write(ioerr,9000,ERR=9999) 'Surrogates','Modeling Domain'
      write(ioerr,9001,ERR=9999) 'Begin: (',butme,',',butmn,')',
     &                                  '      (',xorig,',',yorig,')'
      write(ioerr,9001,ERR=9999) '  End: (',eutme,',',eutmn,')',
     &    '      (',xorig + (nxcell-1)*dxcell,',',
     &                                  yorig + (nycell-1)*dycell,')'
      goto 9999
c
 7004 continue
      if( dxcell .LT. xcelsz+FUZZ ) lerror = .TRUE.
      if( dycell .LT. ycelsz+FUZZ ) lerror = .TRUE.
      write(ioerr,'(/,A)',ERR=9999)
     &              'ERROR:  Reading header of surrogate data file.'
      write(ioerr,'(2A)',ERR=9999)
     &          'Size of grid cell in modeling domain is smaller ',
     &                          'than cell size of gridded surrogates.'
      write(ioerr,9000,ERR=9999) 'Surrogates','Modeling Domain'
      write(ioerr,9001,ERR=9999) '       (',xcelsz,',',ycelsz,')',
     &                                  '      (',dxcell,',',dycell,')'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &           'ERROR:  Reading surrogate data file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',srgfl(:strmin(srgfl))
      goto 9999
c
 7006 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &        'ERROR:  Reading surrogate data values at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',srgfl(:strmin(srgfl))
      write(ioerr,'(A)',ERR=9999) 'Line read: '
      write(ioerr,'(A,/)',ERR=9999) string(:strmin(string))
      if ( irec .EQ. 2 ) write(ioerr,'(A,/)',ERR=9999) 
     &      'The header codes record must begin with keyword CODES.'
      goto 9999
c
 7007 continue
      write(ioerr,'(/,A)',ERR=9999)
     &           'ERROR:  Reading surrogate data file at line.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of counties ',
     &                           'in the surrogate file exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXFIP
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXFIP ',
     &                                                  'and recompile.'
      goto 9999
c
 7008 continue
      write(ioerr,'(/,A)',ERR=9999)
     &           'ERROR:  Reading surrogate data file at line.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of gridded ',
     &                                  'surrogate cells exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSRG
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSRG ',
     &                                                  'and recompile.'
      goto 9999
c
 7009 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7010 continue
      write(ioerr,'(/,A)',ERR=9999)
     &              'ERROR:  Reading header of surrogate data file.'
      write(ioerr,'(2A)',ERR=9999) 'File: ',srgfl(:strmin(srgfl))
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of gridded ',
     &                                'surrogate codes exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',NSURG
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter NSURG ',
     &                                                  'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(A5,1X,A5,2F10.0,I10,300(:,F10.0))
 9000 format(23X,A,T53,A)
 9001 format(10X,A,2(F12.3,A),A,2(F12.3,A))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
