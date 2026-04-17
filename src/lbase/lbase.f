C**** LBASE.F 
c
      program lbase
c
c-----------------------------------------------------------------------
c
c    This module processes link based emissions.  The link records
c    are read and the cells crossed by the link are computed. 
c
c    Calling Sequence:
c
c
c            LBASE
c              |----- init_hash_msgtot
c              |----- opnlbs
c              |         |----- getime
c              |
c              |----- rdulbs
c              |         |----- rddate
c              |         |----- rdcnty
c              |         |----- rdregn
c              |         |----- fndkey
c              |
c              |----- hdrlbs
c              |         |----- getime
c              |         |----- wrhdr
c              |
c              |----- prclbs
c              |         |----- totlbs
c              |         |----- mapgtu
c              |         |----- maputg
c              |         |----- gline
c              |         |----- wrembr
c              |
c              |----- tralbs
c              |----- wtllbs
c              |         |----- wrctbl
c              |
c              |----- clslbs
c
c   clslbs --  closes all file units
c   fndkey --  searches a file for a certain string (library)
c   getime --  gets date and time from system (library)
c   gline  --  computes cells corssed by a link (library)
c   init_hash_msgtot  -- initializes hash table indexes
c   hdrlbs --  creates the new EMBR header
c   mapgtu --  converts geodesic coordinates to UTM (library)
c   maputg --  converts UTM coordinates to geodesic (library)
c   opnlbs --  reads all filenames and opens all files
c   prclbs --  main processing routine, processes each link record
c   rddate --  reads the /DATE/ packet of USERIN (library)
c   rdcnty --  reads the /COUNTY/ packet of USERIN (library)
c   rdregn --  reads the /UAMREGN/ packet of USERIN (library)
c   rdulbs --  reads USERIN file
c   totlbs --  adds emissions to totals arrays
c   tralbs --  creates the new EMBR trailer
c   wrctbl --  writes one table of processed emissions (library)
c   wrembr --  writes one record of EMBR data (library)
c   wrhdr  --  writes the new EMBR header (library)
c   wtllbs --  writes the tables of total processed emissions
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
      include 'iolbase.inc'
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
      call opnlbs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rdulbs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the EMBR header ---
c
      call hdrlbs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to process the data ---
c
      call prclbs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write trailer ---
c
      call tralbs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals of tables ---
c
      call wtllbs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
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
c  --- call routine to close files ---
c
 9999 continue
      call clslbs()
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
