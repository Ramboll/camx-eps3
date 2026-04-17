C**** PREAM.F 
c
      program pream
c
c-----------------------------------------------------------------------
c
c    PREAM is an entry module for EPS2.  it reads an airs area
c    and mobile source workfile and converts it to 2 EMBR files,
c    one for area and one for mobile.
c    The records are screened so that only those counties in the region
c    are written to the EMBR files.
c    Mobile sources are identified as such by their inclusion
c    in the /DISAG/ packet from the mvadj file.
c    Link-based emissions total are subtracted if we find a /PREAM/
c    packet in the user input file.
c
c
c    Calling Sequence:
c
c
c            PREAM
c              |----- init_hash_msgtot
c              |----- opnpra
c              |----- rdupra
c              |         |----- rdregn
c              |         |----- rddate
c              |         |----- rdcnty
c              |         |----- fndkey
c              |
c              |----- hdrpra
c              |         |----- getime
c              |         |----- wrhdr
c              |
c              |----- prcpra
c              |         |----- totpra
c              |         |----- wrepra
c              |         |----- wrrpra
c              |                  |----- totpra
c              |                  |----- wrembr
c              |
c              |----- trapra
c              |----- wtlpra
c              |         |----- wrctbl
c              |
c              |----- clspra
c
c    clspra  -- closes all file units
c    fndkey  -- searches a file for a keyword (library)
c    getime  -- fills the date and time string (library)
c    hdrpra  -- creatses the new EMBR headers
c   init_hash_msgtot  -- initializes hash table indexes
c    opnpra  -- reads all filenames and opens all file
c    prcpra  -- main processing routine, reads and processes AMS file
c    rddate  -- reads the /DATE/ packet of USERIN (library)
c    rdcnty  -- reads the /COUNTY/ packet of USERIN (library)
c    rdregn  -- reads the /UAMREGN/ packet of USERIN (library)
c    rdupra  -- reads the USERIN file
c    totpra  -- stores emissions in totals arrays
c    trapra  -- writes the new EMBR trailer data
c    wrctbl  -- writes one table of processed emissions (library)
c    wrembr  -- writes one EMBR data record (library)
c    wrhdr   -- writes the EMBR header (library)
c    wrepra  -- writes the EMAR error records
c    wrrpra  -- writes the EMBR records
c    wtlpra  -- creates tables of processed emissions
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
      include 'iopream.inc'
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c     ierr  I   error flag for subroutine calls
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Entry Point:
c-----------------------------------------------------------------------
c
c
c  --- call routine to initialize the hash table indexes ---
c
      call init_hash_msgtot( )
c
c  --- call routine to get I/O filenames and open I/O files
c
      call opnpra(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rdupra(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write the EMBR header ---
c
      call hdrpra(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to process each AMS record ---
c
      call prcpra(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write trailer ---
c
      call trapra(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals of tables ---
c
      call wtlpra(ierr)
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
c
c-----------------------------------------------------------------------
c   Exit Point:
c-----------------------------------------------------------------------
c
c  --- call routine to close files ---
c
 9999 continue
      call clspra( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.' 
      endif
      end
