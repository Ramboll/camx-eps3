c**** RDUFIR.F
c
      subroutine rdufir( ierr )
c
c-----------------------------------------------------------------------
c
c    Calls routines to read the USERIN packets required by PREFIR/
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
       include 'msgtot.inc'
       include 'uamcntl.inc'
       include 'uamregn.inc'
       include 'cntylst.inc'
       include 'ioprefir.inc'
       include 'firebin.inc'
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
      character*40        cdate
      character*10        module
      integer*4           jerr, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data module /'PREFIR'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c     --- initialize error flag ---
c
      ierr = IFAIL
c
c     --- call routine to read criteria pollutants packet ---
c
      call rdcrit( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c     --- call routine to read date packet ---
c
      call rddate( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c     --- call routine to read region definition ---
c
      call rdregn( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c     --- call routine to read county list ---
c
      call rdcnty( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c     --- call routine to get the output tables requested ---
c
      call rdotbl( jerr,
     &                   IORINP, module, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- disable the tables not supported by this module ---
c
      if( lcrtfac(IDXFMT) .OR. lspcfac(IDXFMT) .OR.
     &             lcrtfac(IDXCSV) .OR. lspcfac(IDXCSV) ) then
        write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
        write(ioerr,'(3A)',ERR=9999) 'WARNING: The table by Facility ',
     &                                                 'was requested.'
        write(ioerr,'(3A)',ERR=9999) 'That table is not supported by ',
     &                         'this module and will not be produced.'
        write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
        nwarn = nwarn + 1
        lcrtfac(IDXFMT) = .FALSE.
        lcrtfac(IDXCSV) = .FALSE.
        lspcfac(IDXFMT) = .FALSE.
        lspcfac(IDXCSV) = .FALSE.
      endif
      if( lcrtstk(IDXFMT) .OR. lspcstk(IDXFMT) .OR.
     &             lcrtstk(IDXCSV) .OR. lspcstk(IDXCSV) ) then
        write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
        write(ioerr,'(3A)',ERR=9999) 'WARNING: The table by Facility ',
     &                                      'and Stack was requested.'
        write(ioerr,'(3A)',ERR=9999) 'That table is not supported by ',
     &                         'this module and will not be produced.'
        write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
        nwarn = nwarn + 1
        lcrtstk(IDXFMT) = .FALSE.
        lcrtstk(IDXCSV) = .FALSE.
        lspcstk(IDXFMT) = .FALSE.
        lspcstk(IDXCSV) = .FALSE.
      endif
      if( lcrtpnt(IDXFMT) .OR. lspcpnt(IDXFMT) .OR.
     &             lcrtpnt(IDXCSV) .OR. lspcpnt(IDXCSV) ) then
        write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
        write(ioerr,'(3A)',ERR=9999) 'WARNING: The table by Facility,',
     &                            ' Stack and Point was requested.'
        write(ioerr,'(3A)',ERR=9999) 'That table is not supported by ',
     &                         'this module and will not be produced.'
        write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
        nwarn = nwarn + 1
        lcrtpnt(IDXFMT) = .FALSE.
        lcrtpnt(IDXCSV) = .FALSE.
        lspcpnt(IDXFMT) = .FALSE.
        lspcpnt(IDXCSV) = .FALSE.
      endif
c
c     --- echo the data to message file ---
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
      write(IOWMSG,9003,ERR=7000) 'Number of counties',ncdfip
c
c     --- call routine to read the optional FIRE BIN packet ---
c
      call rdbins( jerr,
     &                 IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c     --- echo to message file ---
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
c     --- call routine to read the PREFIR packet ---
c
      call rdprfp( jerr )
      if( jerr .NE. ISUCES ) goto 9999
c
c     ---- set error flag to sucess ---
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
 9008 format(T10,A,T40,:,':',I6)
 9009 format(T40,:,' ',I5,2X,F8.1,1X,F8.3,1X,2(F8.1,1X))
 9010 format(T40,:,A,A)
c
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
