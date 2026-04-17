C**** SPCEMS.F 
c
      program spcems
c
c-----------------------------------------------------------------------
c
c    SPCEMS takes an EMBR file, applies speciation and creates an EMBR
c    file. 
c    To do this, SPCEMS uses a profile cross reference file, 
c    speciation factors file, and optional conversion factors file.
c
c    Calling Sequence:
c
c            SPCEMS
c              |----- init_hash_msgtot
c              |----- opnspc
c              |----- rdhspc
c              |         |----- rdhdr
c              |
c              |----- rduspc
c              |         |----- rdspec
c              |         |----- rdcnty
c              |         |----- rdotbl
c              |         |----- fndkey
c              |
c              |----- rdsspc
c              |----- cmpspc
c              |----- hdrspc
c              |         |----- getime
c              |         |----- filhdr
c              |
c              |----- rdxspc
c              |----- prcspc
c              |         |----- rdembr
c              |         |----- totspc
c              |         |----- fndxrf
c              |         |----- wrembr
c              |         |----- wrespc
c              |                  |----- wremar
c              |
c              |----- traspc
c              |         |----- rdtral
c              |
c              |----- wtlspc
c              |         |----- wrctbl
c              |
c              |----- clsspc
c
c   opnspc  -- reads the filenames and open all files
c   rdhspc  -- stores input EMBR header variables
c   rdhdr   -- reads the input EMBR header (library)
c   rduspc  -- reads the USERIN file
c   rdspec  -- reads the /SPECIES LIST/ packet (library)
c   rdcnty  -- reads the /COUNTY/ packet (library)
c   fndkey  -- searches a file for a keyword (library)
c   rdsspc  -- reads the splitting factors file
c   cmpspc  -- compares the species list in USERIN to the EMBR species list
c   hdrspc  -- writes the header data to the output EMBR file
c   init_hash_msgtot  -- initializes hash table indexes
c   getime  -- sets the date and time string (library)
c   filhdr  -- reads and writes header data
c   rdxspc  -- reads the profile cross reference file w/criteria poll
c   prcspc  -- main processing routine
c   rdembr  -- reads one EMBR data record (library)
c   totspc  -- stores emissions in totals arrays
c   wrembr  -- writes one EMBR data record (library)
c   wrespc  -- writes the EMAR formatted error file
c   wremar  -- writes one EMAR data record
c   traspc  -- writes the EMBR trailer
c   rdtral  -- reads and writes the existing EMBR trailer
c   wtlspc  -- writes the tables of processed emissions
c   wrctbl  -- writes a table of totals (library)
c   clsspc  -- closes all file units
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iospcems.inc'
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
c  --- call routine to get I/O filenames and open I/O files ---
c
      nwarn = 0
      call opnspc(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read header of EMBR file ---
c
      call rdhspc(ierr)
      if ( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rduspc(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the speciation factors file ---
c
      call rdsspc(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the pollutant conversion factor file ---
c
      call rdcspc(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write the EMBR header ---
c
      call hdrspc(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read ASC-SCC profile cross reference file ---
c
      call rdxspc(ierr,IORXRF)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to process each EMBR record ---
c
      call prcspc(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write trailer ---
c
      call traspc(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals of tables ---
c
      call wtlspc(ierr)
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
c  --- call routine to close files ---
c
 9999 continue
      call clsspc( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
