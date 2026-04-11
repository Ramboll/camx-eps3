C**** RDUSPL.F
c
      subroutine rduspl( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file, looking for the /SPLTEM/ packet
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
c      08/11/97  --mmj--  original development
c      01/14/08  --mmj--  updated EPS3 compatible
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iospltem.inc'
      include 'spltem.inc'
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines called:
c-----------------------------------------------------------------------
c
c   fndkey  --  searches a file for a keyword (library)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strlen  I   returns the actual length of a string
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 strlen
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   TOGNAM  C   parameter for matching input emissions flag
c   VOCNAM  C   parameter for matching input emissions flag
c
      character*3 TOGNAM
      character*3 VOCNAM
c
      parameter( TOGNAM = 'TOG')
      parameter( VOCNAM = 'VOC')
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   ijtmp   I   temporary cell location
c   ilen    C   variable to store length of filename
c   jerr    I   error flag for subroutine calls
c   keywrd  C   temporary storage for seaching fo keyword
c   string  C   temporary character string
c
      character*(KEYLEN) keywrd, keyin
      character*(MXSTR)  string, tmpstr
      real*4             xtmp, ytmp
      integer*4          ilen, jerr, i, j, n, ijtmp
      logical*4          leps, lfound
c
c-----------------------------------------------------------------------
c   Data Statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/SPLTEM/'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume success ---
c
      ierr = IFAIL
      leps = .FALSE.
      lfound = .FALSE.
      lhcspl = .FALSE.
c
c   --- call routine to find /SPLTEM/ packet ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read criteria pollutant name to be split on output ---
c
      read(IORINP,'(A)',ERR=7002) string
      crtspl = string(21:MXSTR)
      call lftjst( crtspl )
      call low2up( crtspl )

c   --- check that it is a valid name in input embr header ---
c   --- and set flag for each embr criteria pollutant      ---
      do i = 1, ncrthd
         if( crtnam(i) .EQ. crtspl ) then
             lfound = .TRUE.
             lsplit(i) = .TRUE.
         else
             lsplit(i) = .FALSE.
         endif
      enddo
      if( .NOT.lfound ) then
         write(IOWSTD,'(/A,A)',ERR=9999)
     &     'WARNING: Criteria pollutant specified in /SPLTEM/ packet ',
     &                         'not found in EMBR header.'
         write(IOWSTD,'(A)',ERR=9999)
     &     'No splitting will be performed.'
         write(IOWSTD,'(A/)',ERR=9999)
     &     'No output table by Profile Code will be generated.'
         write(IOWMSG,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(IOWMSG,'(/A,A)',ERR=7001)
     &     'WARNING: Criteria pollutant specified in /SPLTEM/ packet ',
     &                         'not found in EMBR header.'
         write(IOWMSG,'(A)',ERR=7001)
     &     'No splitting will be performed.'
         write(IOWMSG,'(A/)',ERR=7001)
     &     'No output table by Profile Code will be generated.'
         write(IOWMSG,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      endif
      
      if( crtspl(1:3) .EQ. VOCNAM ) then
          lvocin = .TRUE.
          lhcspl = .TRUE.
      else if( crtspl(1:3) .EQ. TOGNAM ) then
          lvocin = .FALSE. 
          lhcspl = .TRUE.
      endif   
c
c   --- read output splits ---
c   --- optional - only needed if specified input split is VOC or TOG ---
c
      read(IORINP,'(A)',ERR=7002) string
      hcout = string(21:MXSTR)
      call lftjst( hcout )
      call low2up( hcout )

      if ( lhcspl )  then
         if( hcout(1:3) .EQ. VOCNAM ) then
             lvocout = .TRUE.
         else if( hcout(1:3) .EQ. TOGNAM ) then
             lvocout = .FALSE. 
         else
            goto 7006
         endif   
      endif   
c
c   --- read region to extract ---
c
      read(IORINP,8000,ERR=7002) illxcl
      read(IORINP,8000,ERR=7002) illycl
      read(IORINP,8000,ERR=7002) iurxcl
      read(IORINP,8000,ERR=7002) iurycl
c
      if ( (illxcl.GT.iurxcl) .OR. (illycl.GT.iurycl) )  goto 7003
      if ( (iurxcl.GT.nxrgn)  .OR. (iurycl.GT.nyrgn) )  goto 7004
c
c   --- read the optional tolerance value ---
c
      read(IORINP,'(A)',ERR=7002) string
c
c   --- return if /END/ keyword found ---
c
      keyin = string(1:20)
      call lftjst( keyin )
      call low2up( keyin )
      tmpstr = string(21:MXSTR)
      call lftjst( tmpstr )
      if( keyin .EQ. KEYEND .OR. strlen ( tmpstr ) .EQ. 0) then
          epslon = DEFTOL
      else
          read(string,'(20X,F10.0)',ERR=7002) epslon
          epslon = ABS(epslon)
          leps = .TRUE.
      endif
c
c   --- save list of grid cells to extract ---
c
      ncells = (iurxcl - illxcl + 1) * (iurycl - illycl + 1)
      if ( ncells .GT. MXCEL )  goto 7005
c
      n = 0
      do 20 j = illycl, iurycl
         do 10 i = illxcl, iurxcl
            n = n + 1
            ijtmp = i*1000 + j
            ijlst(n) = ijtmp
   10   continue
   20 continue
c
c   --- echo the data to message file ---
c
      write(IOWMSG,9000,ERR=7001) 
      if( lfound ) then
          write(IOWMSG,9009,ERR=7001) 'Criteria pollutant to split ',
     &                                 crtspl
      else
          write(IOWMSG,9009,ERR=7001) 'Criteria pollutant to split ',
     &                                 crtspl, '  Not Found'
      endif
      write(IOWMSG,9000,ERR=7001) 
      if( lhcspl ) then
          write(IOWMSG,9009,ERR=7001) 'Output HCs specified as ', 
     &                                 hcout
      endif
      write(IOWMSG,9000,ERR=7001)
      write(IOWMSG,9000,ERR=7001) 
      write(IOWMSG,9002,ERR=7001) 
     &                       'Grid cell boundary of region to extract -'
       xtmp = rgutme + ( illxcl * dxrgn )
       ytmp = rgutmn + ( illycl * dyrgn )
      write(IOWMSG,9004,ERR=7001) 'Lower left corner ',
     &                            xtmp, ytmp,
     &                            illxcl,illycl
       xtmp = rgutme + ( iurxcl * dxrgn )
       ytmp = rgutmn + ( iurycl * dyrgn )
      write(IOWMSG,9004,ERR=7001) 'Upper right corner ',
     &                            xtmp, ytmp,
     &                            iurxcl,iurycl
      write(IOWMSG,9000,ERR=7001) 
      if( leps ) then
          write(IOWMSG,9008,ERR=7001) 'Specified tolerance ', epslon
      else
          write(IOWMSG,9008,ERR=7001) 'Default tolerance ', epslon
      endif
      write(IOWMSG,9000,ERR=7001) 
c
c   ---- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,9000,ERR=9999)
     &              'ERROR:  This program requires the /SPLTEM/ packet.'
      write(IOWMSG,9000,ERR=7001)
     &              'ERROR:  This program requires the /SPLTEM/ packet.'
      goto 9999
c
 7001 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR: Writing message file: ',
     &                                        msgfl(:strmin(msgfl))
      goto 9999
c
 7002 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR: Reading /SPLTEM/ packet.'
      write(IOWMSG,9000,ERR=7001) 'ERROR: Reading /SPLTEM/ packet.'
      goto 9999
c
 7003 continue
      write(IOWSTD,9006,ERR=9999)
     &   'ERROR: Lower left XY cell must be <= upper right XY cell.',
     &          'Lower left = ', illxcl, illycl, 
     &          'Upper right = ', iurxcl,iurycl
      write(IOWMSG,9006,ERR=7001)
     &   'ERROR: Lower left XY cell must be <= upper right XY cell.',
     &          'Lower left = ', illxcl, illycl, 
     &          'Upper right = ', iurxcl,iurycl
      goto 9999
c
 7004 continue
      write(IOWSTD,9000,ERR=9999)
     &         'ERROR: Region to extract outside of modeling region.'
      write(IOWMSG,9000,ERR=7001)
     &         'ERROR: Region to extract outside of modeling region.'
      goto 9999
c
 7005 continue
      write(IOWSTD,9007,ERR=9999)
     &                'ERROR: Number of cells to extract ', ncells,
     &                 ' exceeds maximum.  MXCEL = ', MXCEL
      write(IOWMSG,9007,ERR=7001)
     &                'ERROR: Number of cells to extract ', ncells,
     &                 ' exceeds maximum.  MXCEL = ', MXCEL
      goto 9999
c
 7006 continue
      write(IOWSTD,9000,ERR=9999)
     &      'ERROR: HC criteria pollutant was specified to split.',
     &               ' Output HC (VOC or TOG) must be specified.'
      write(IOWSTD,9000,ERR=9999)
     &      '       Inputs indicate - ', hcout
      write(IOWMSG,9000,ERR=7001)
     &      'ERROR: HC criteria pollutant was specified to split.',
     &               ' Output HC (VOC or TOG) must be specified.'
      write(IOWMSG,9000,ERR=7001)
     &      '       Inputs indicate - ', hcout
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(20X,I10)
c
 9000 format(1X,A,A)
 9002 format(T10,A,T40,:,': (',F8.2,',',F8.2,')')
 9004 format(T10,A,T40,:,
     &        ': XY (',F8.2,',',F8.2,')','  IJ (',I5,',',I5,')')
 9006 format(1X,A,A,'(',I5,',',I5,')',A,'(',I5,',',I5,')')
 9007 format(1X,A,I5,A,I5)
 9008 format(T10,A,T40,:,': ',F7.3)
 9009 format(T10,A,T40,:,': ',A,A)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
