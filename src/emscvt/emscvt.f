C**** EMSCVT 
c
      program emscvt
c
c-----------------------------------------------------------------------
c
c    EMSCVT computes the splitting factors that will be used to convert
c    from grams of criteria pollutant to moles of CBIV species.
c    The weight profiles, which represent the chemical makeup of one
c    gram of criteria pollutant for a given speciation profile, are read
c    from one of the input files.  This data is used, along with the
c    chemical compound data base, to calculate the splitting factors.
c    If default speciatiion data is supplied, the new data will replace
c    the existing values.
c
c    Calling Sequence:
c
c
c            EMSCVT
c              |----- opnevt
c              |----- rduevt
c              |----- rdcdbs
c              |----- rdxref
c              |----- rdespl
c              |----- rdprof
c              |----- rdctlf
c              |----- prcevt
c              |----- wrexrf
c              |----- wrespl
c              |----- clsabr
c
c   clsevt  -- closes all file units
c   opnevt  -- reads filenames and opens all files
c   prcevt  -- processed each weight profile
c   rdcdbs  -- reads compound data base file
c   rdespl  -- reads the existing splits file
c   rdxref  -- reads the existing cross reference file (library)
c   rdprof  -- reads the profile weights file
c   rduevt  -- reads user input file
c   wrespl  -- writes the new splits file
c   wrexrf  -- writes the new cross reference file
c
c-----------------------------------------------------------------------
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
      include 'ioemscvt.inc'
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c     ierr  I   error flag for subroutine calls
c     leof  L   true if end of file for profiles file
c
      integer*4 ierr
      logical*4 leof
c
c-----------------------------------------------------------------------
c    Entry Point:
c-----------------------------------------------------------------------
c
c  --- call routine to get I/O filenames and open I/O files
c
      call opnevt(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rduevt(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- read the chemical compound data base ---
c
      call rdcdbs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read ASC-SCC cross reference file ---
c
      if( lxref ) then
         call rdxref(ierr,IORXRF)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine to read existing splits file ---
c
      if( lsplit ) then
         call rdespl(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine to read control factors file ---
c
      if( lctlfl ) then
         call rdctlf(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- read existing profile ---
c
      leof = .FALSE.
 111  continue
      call rdprof(ierr)
      if( ierr .EQ. IEOD ) leof = .TRUE.
      if( ierr .NE. ISUCES .AND. ierr .NE. IEOD ) goto 9999
c
c  --- call routine to process this profile ---
c
      call prcevt(ierr)
      if( ierr .NE. ISUCES ) goto 9999
      if( .NOT. leof ) goto 111
c
c  --- call routine to write new cross refernce file ---
c
      if( loxrfl ) then
         call wrexrf(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine to write new splits file ---
c
      call wrespl(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- looks successful, write message and reset flag ---
c
      write(IOWSTD,'(//,2A)',ERR=9999) 'Successful completion of ',
     &                                                       VERSON
      if( nwarn .EQ. 0 ) then
          write(IOWSTD,'(2A,//)',ERR=9999) 'There were no warning ',
     &                                             'messages written.'
      else if( nwarn .EQ. 1 ) then
          write(IOWSTD,'(2A,//)',ERR=9999) 'There was 1 warning ',
     &                                             'message written.'
      else
          write(IOWSTD,'(A,I2,A,//)',ERR=9999) 'There were ',nwarn,
     &                                   ' warning messages written.'
      endif
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Exit Point:
c-----------------------------------------------------------------------
c
 9999 continue
      call clsevt()
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
