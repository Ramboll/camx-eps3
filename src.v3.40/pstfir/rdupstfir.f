c**** RDUPSTFIR.F
c
      subroutine rdupstfir( ierr )
c
c-----------------------------------------------------------------------
c
c    Calls routines to read the USERIN packets required by PSTFIR.
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
c     22 sep 2008 modified to read TMPRL packet in order to
c                          get modeling time zone (needed for hourly
c                          vertical distribution)  -mjimenez
c     03 jul 2012 modified to read optional PSTFIR packet in order to
c                          determine if all fire emissions should be
c                          written to the elevated points file
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
       include 'param.inc'
       include 'msgtot.inc'
       include 'uamcntl.inc'
       include 'uamregn.inc'
       include 'cntylst.inc'
       include 'firebin.inc'
       include 'layers.inc'
       include 'iopstfir.inc'
       include 'usrpstfir.inc'
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
c   respon  C   character string for temporary storage
c
      character*(MXSTR)   string, projtype
      character*(KEYLEN)  keywrd
      character*(MXSTR)   respon
      character*40        cdate
      character*10        module
      integer*4           jerr, i, irec
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/PSTFIR/'/
      data module /'PSTFIR'/
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
c  --- disable the tables not supported by this module ---
c
      if( lcrtfac(IDXFMT) .OR. lspcfac(IDXFMT) .OR.
     &             lcrtfac(IDXCSV) .OR. lspcfac(IDXCSV) ) then
        write(ioerr,'(//,10x,40A,/)',ERR=9999) ('/\\',i=1,40)
        write(ioerr,'(10x,3A)',ERR=9999) 
     &          'WARNING: The table by Facility was requested.'
        write(ioerr,'(10x,3A)',ERR=9999) 'That table is not ',
     &          'supported by this module and will not be produced.'
        write(ioerr,'(/,10x,40A,//)',ERR=9999) ('/\\',i=1,40)
        nwarn = nwarn + 1
        lcrtfac(IDXFMT) = .FALSE.
        lcrtfac(IDXCSV) = .FALSE.
        lspcfac(IDXFMT) = .FALSE.
        lspcfac(IDXCSV) = .FALSE.
      endif
      if( lcrtstk(IDXFMT) .OR. lspcstk(IDXFMT) .OR.
     &             lcrtstk(IDXCSV) .OR. lspcstk(IDXCSV) ) then
        write(ioerr,'(//,10x,40A,/)',ERR=9999) ('/\\',i=1,40)
        write(ioerr,'(10x,3A)',ERR=9999) 
     &       'WARNING: The table by Facility and Stack was requested.'
        write(ioerr,'(10x,3A)',ERR=9999) 'That table is not ',
     &          'supported by this module and will not be produced.'
        write(ioerr,'(/,10x,40A,//)',ERR=9999) ('/\\',i=1,40)
        nwarn = nwarn + 1
        lcrtstk(IDXFMT) = .FALSE.
        lcrtstk(IDXCSV) = .FALSE.
        lspcstk(IDXFMT) = .FALSE.
        lspcstk(IDXCSV) = .FALSE.
      endif
      if( lcrtpnt(IDXFMT) .OR. lspcpnt(IDXFMT) .OR.
     &             lcrtpnt(IDXCSV) .OR. lspcpnt(IDXCSV) ) then
        write(ioerr,'(//,10x,40A,/)',ERR=9999) ('/\\',i=1,40)
        write(ioerr,'(10x,3A)',ERR=9999) 
     &              'WARNING: The table by Facility,',
     &                            ' Stack and Point was requested.'
        write(ioerr,'(10x,3A)',ERR=9999) 'That table is not ',
     &          'supported by this module and will not be produced.'
        write(ioerr,'(/,10x,40A,//)',ERR=9999) ('/\\',i=1,40)
        nwarn = nwarn + 1
        lcrtpnt(IDXFMT) = .FALSE.
        lcrtpnt(IDXCSV) = .FALSE.
        lspcpnt(IDXFMT) = .FALSE.
        lspcpnt(IDXCSV) = .FALSE.
      endif
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
      if( lutm ) projtype = "Universal Transveerse Mercator"
      if( llatlon ) projtype = "Geodetic"
      if( lrpolar ) projtype = "Rotated Polar Stereographic"
      if( lambrt ) projtype = "Lambert Conformal"
      if( lpolar ) projtype = "Polar Stereographic"
      if( lmerc ) projtype = "Mercator"
      write(IOWMSG,9011,ERR=7000) 'Projection type',
     &                                 projtype(:strmin(projtype))
      if( lrpolar .OR. lambrt .OR. lpolar .OR. lmerc ) then
         write(IOWMSG,9012,ERR=7000) 'Longitude Pole',polelon
         write(IOWMSG,9012,ERR=7000) 'Latitude Pole',polelat
      endif
      if( lambrt .OR. lpolar .OR. lmerc ) then
         write(IOWMSG,9012,ERR=7000) '1st True Latitude',tlat1
         write(IOWMSG,9012,ERR=7000) '2nd True Latitude',tlat2
      endif
      write(IOWMSG,9003,ERR=7000) 'Number of counties',ncdfip
c
c   --- read the TMPRL packet and extract modeling time zone ---
c
      call rdmtz( jerr, invtzn )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to find /PSTFIR/ packet --- 
c   --- added july 2012 ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
c
c   --- read the packet variables --- 
c
      if( jerr .NE. ISUCES ) then
         if ( lsrffl ) then
            lelvall = .FALSE.
         else
            lelvall = .TRUE.
         endif
      else
         irec = irec + 1 
         read(IORINP,'(A)',ERR=7002,END=7003) string
         respon = string(21:MXSTR)
         call low2up( respon )
         call lftjst( respon )
         if( respon(1:3) .EQ. YES ) then
             lelvall = .TRUE.
         else if( respon(1:3) .EQ. NO ) then 
             lelvall = .FALSE.
         else
             goto 7004
         endif
      endif
      if (lelvall) then
          write(IOWMSG,9002,ERR=7000) 
     &      'All emissions to elevated file ',YES
      else
          write(IOWMSG,9002,ERR=7000) 
     &      'All emissions to elevated file ',NO
      endif
c
c  --- check that output files were specified ---
c
      if (lelvall) then
         if ( .NOT.lelvfl .AND. .NOT.lptsfl ) goto 7005
      else
         if ( .NOT.lsrffl) goto 7006
      endif
c
c   --- call routine to read /LAYERS/ packet ---
c
      call rdlayers( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- write layers to message file ---
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
c  --- call routine to read the optional FIRE BIN packet ---
c
      call rdbins( jerr,
     &                 IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- echo to message file ---
c
      if ( lbindef ) then
        write(IOWMSG,9002,ERR=7000) 'Specified Fire Bins',
     &                              ' None - Defaults Assigned'
      else
        write(IOWMSG,9002,ERR=7000) 'Specified Fire Bins',
     &                              ' '

      endif
      write(IOWMSG,9010,ERR=7000) ' ',
     &           ' Class     Size       BE  PtopMax  PbotMax'
c
      do i = 1, nfbin
         write(IOWMSG,9009,ERR=7000) i,
     &                               sizfbin(i),
     &                               befbin(i),
     &                               topfbin(i),
     &                               botfbin(i)
      enddo
c
c  --- write output species to file ---
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
c  ---- set error flag to sucess ---
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
 7002 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &             keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999)
     &             'Invalid response to all elevated points flag: ',
     &                                      string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: User requested ',
     &             'only elevated outputs but did not provide the ',
     &             'required output file name(s).'
      goto 9999
c
 7006 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: User requested ',
     &             'surface outputs but did not provide the ',
     &             'required output file name.'
      goto 9999

c
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
 9010 format(T40,:,A,A)
 9011 format(T10,A,T40,:,': ',A)
 9012 format(T10,A,T40,:,':',F10.3)
c
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
