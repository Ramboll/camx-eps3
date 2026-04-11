C**** PREPNT.F 
c
      program prepnt
c
c-----------------------------------------------------------------------
c
c    PREPNT is an entry module for EPS.  It reads an airs facility
c    subsystem workfile and converts it to an EMBR file.  The records
c    are screened so that only those facilities in the region are
c    written to the EMBR file.  Also, the effective plume height for
c    each stack is calculated and the records are flagged with either
c    an "E" for elevated or an "L" for low level points.
c
c
c    Calling Sequence:
c
c            PREPNT
c              |----- init_hash_msgtot
c              |----- opnprp
c              |----- rduprp
c              |         |----- rddate
c              |         |----- rdregn
c              |         |----- rdcnty
c              |         |----- fndkey
c              |
c              |----- rdxref
c              |----- hdrprp
c              |         |----- getime
c              |         |----- wrhdr
c              |
c              |----- prcprp
c              |         |----- totprp
c              |         |----- mapgtu
c              |         |----- setij
c              |         |----- beh072
c              |         |----- wrtstk
c              |         |----- wrrprp
c              |         |        |----- wrembr
c              |         |
c              |         |----- wreprp
c              |
c              |----- traprp
c              |----- wtlprp
c              |         |----- wrctbl
c              |
c              |----- clsprp
c
c  beh072 -- computes the effective plume height of stack
c  clsprp -- closes all file units
c  fndkey -- searches a file for a keyword (library)
c  getime -- creates the current date and time string
c  hdrprp -- creates the new EMBR header
c  init_hash_msgtot  -- initializes hash table indexes
c  mapgtu -- converts geodesic coordinates to UTM coordinates (library)
c  opnprp -- reads filenames and opens files
c  prcprp -- main processing routine
c  rddate -- reads the /DATE/ packet of USERIN (library)
c  rdcnty -- reads the /COUNTY/ packet of USERIN (library)
c  rdxref -- reads the ASC-SCC/VOC cross reference file (library)
c  rdregn -- reads the /UAMREGN/ packet of USERIN (library)
c  rduprp -- reads USERIN file
c  setij  -- finds the grid cell containing a coordinate pair (library)
c  totprp -- stores emissions in totals arrays
c  traprp -- writes the EMBR trailer data
c  wrctbl -- writes one table of totals
c  wrembr -- writes one data record to EMBR file (library)
c  wrhdr  -- writes the header to the EMBR file
c  wreprp -- writes record to error file
c  wrrprp -- writes the data to EMBR file
c  wrtstk -- wriits the stack report file
c  wtlprp -- writes the tables of processed emissions
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
      include 'ioprepnt.inc'
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
c  --- call routine to initialize the hash table indexes ---
c
      call init_hash_msgtot( )
c
c  --- call routine to get I/O filenames and open I/O files
c
      call opnprp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rduprp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write the EMBR header ---
c
      call hdrprp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to process each AFS record ---
c
      call prcprp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write trailer ---
c
      call traprp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals of tables ---
c
      call wtlprp(ierr)
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
      call clsprp( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
