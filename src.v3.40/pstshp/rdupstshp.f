c**** RDUPSTSHP.F
c
      subroutine rdupstshp( ierr )
c
c-----------------------------------------------------------------------
c
c    Calls routines to read the USERIN packets required by PSTSHP.
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
c   jan 2018  gwilson added option for 3D emissions in NetCDF output
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
       include 'layers.inc'
       include 'iopstshp.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   jerr    I   error flag for subroutine calls
c
      character*(MXSTR)   string, projtype
      character*40        cdate
      character*10        module
      integer*4           jerr, i, irec
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data module /'PSTSHP'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- initialize error code ---
c
      ierr = IFAIL
      irec = 0
c
c   --- call routine to read species list packet ---
c
      call rdspec( jerr,
     &                   IORINP, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read date packet ---
c
      call rddate( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read region definition ---
c
      call rdregn( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read county list ---
c
      call rdcnty( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to get the output tables requested ---
c
      call rdotbl( jerr,
     &                   IORINP, module, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- echo the data to message file ---
c
      call getime( cdate )
      write(IOWMSG,9001,ERR=7000) VERSON, cdate
      write(IOWMSG,9002,ERR=7000) 'File note',fnote(:strmin(fnote))
      write(IOWMSG,9008,ERR=7000) 'Episode date (Calander)',ibgcal
      write(IOWMSG,9003,ERR=7000) 'Episode date (Julian)',ibgdat
      write(IOWMSG,9003,ERR=7000) 'Beginning hour',INT(begtim/100.)
      write(IOWMSG,9003,ERR=7000) 'Ending hour',INT(endtim/100.)
      write(IOWMSG,9005,ERR=7000) 'Grid origin (km)',xorig,yorig
      write(IOWMSG,9003,ERR=7000) 'UTM zone',izone
      write(IOWMSG,9005,ERR=7000) 'Grid cell width (km)',dxcell,dycell
      write(IOWMSG,9006,ERR=7000) 'Number of cells',nxcell,nycell
      projtype = 'Not supplied.'
      if( lutm ) projtype = "Universal Transverse Mercator"
      if( llatlon ) projtype = "Geodetic"
      if( lrpolar ) projtype = "Rotated Polar Stereographic"
      if( lambrt ) projtype = "Lambert Conformal"
      if( lpolar ) projtype = "Polar Stereographic"
      if( lmerc ) projtype = "Mercator"
      write(IOWMSG,9010,ERR=7000) 'Projection type',
     &                                 projtype(:strmin(projtype))
      if( lrpolar .OR. lambrt .OR. lpolar .OR. lmerc ) then
         write(IOWMSG,9011,ERR=7000) 'Longitude Pole',polelon
         write(IOWMSG,9011,ERR=7000) 'Latitude Pole',polelat
      endif
      if( lambrt .OR. lpolar .OR. lmerc ) then
         write(IOWMSG,9011,ERR=7000) '1st True Latitude',tlat1
         write(IOWMSG,9011,ERR=7000) '2nd True Latitude',tlat2
      endif
      write(IOWMSG,9003,ERR=7000) 'Number of counties',ncdfip
c
c   --- call routine to read /LAYERS/ packet ---
c
      call rdlayers( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c     --- write layers to message file ---
c
      write(IOWMSG,'(/)',ERR=7000) 
      do i = 1, nlayer
         if ( i .EQ. 1 )  then
          write(IOWMSG,9007,ERR=7000) 'Layer AGL Ht (m)',
     &                                 aglht(i), i
         else
          write(IOWMSG,9007,ERR=7000) ' ',aglht(i), i
         endif
      enddo
c
c     --- write output species to file ---
c
      write(IOWMSG,'(/)',ERR=7000) 
      do i = 1, nspecs
         if ( i .EQ. 1 )  then
          write(IOWMSG,9002,ERR=7000) 'Species output list',
     &                                 spcnam(i)
         else
          write(IOWMSG,9002,ERR=7000) ' ',spcnam(i)
         endif
      enddo
c
c   ---- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      ierr = IFAIL
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9001 format(/,T20,A,A40,/)
 9002 format(T10,A,T40,:,':',A)
 9003 format(T10,A,T40,:,':',I5,:,I5,:,I5)
 9005 format(T10,A,T40,:,': (',F10.3,',',F10.3,')')
 9006 format(T10,A,T40,:,': (',I5,',',I5,')')
 9007 format(T10,A,T40,:,':',F12.2,1X,'(',I2,')')
 9008 format(T10,A,T40,:,':',I6)
 9009 format(T40,:,' ',I5,2X,F8.1,1X,F8.3,1X,2(F8.1,1X))
 9010 format(T10,A,T40,:,': ',A)
 9011 format(T10,A,T40,:,':',F10.3)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
