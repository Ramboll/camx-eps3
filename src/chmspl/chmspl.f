C**** CHMSPL.F 
c
      program chmspl
c
c-----------------------------------------------------------------------
c
c    CHMSPL takes an EMBR file that has not been speciated.
c    CHMSPL does the speciation, creating an EMBR file.
c    To do this, CHMSPL uses the SCC cross reference file and
c    the splitting factors files created by emscvt.
c
c    CHMSPL compares the species list from the /SPECIES LIST/ packet
c    of the user input file with the species list in the splits
c    file.  If a species is listed in the /SPECIES LIST/ packet but
c    not the splits file, execution terminates.  In the reverse
c    situation, where the species exists in the splits file but
c    not the /SPECIES LIST/ packet, the user can specify whether the
c    program continues or terminates.
c
c    Calling Sequence:
c
c            CHMSPL
c              |----- init_hash_msgtot
c              |----- opnchm
c              |----- rdhchm
c              |         |----- rdhdr
c              |
c              |----- rduchm
c              |         |----- rdspec
c              |         |----- rdcnty
c              |         |----- fndkey
c              |
c              |----- rdschm
c              |----- cmpspc
c              |----- hdrchm
c              |         |----- getime
c              |         |----- filhdr
c              |
c              |----- rdxref
c              |----- prcchm
c              |         |----- rdembr
c              |         |----- totchm
c              |         |----- fndxrf
c              |         |----- wrembr
c              |         |----- wrechm
c              |                  |----- wremar
c              |
c              |----- trachm
c              |         |----- rdtral
c              |
c              |----- wtlchm
c              |         |----- wrctbl
c              |
c              |----- clschm
c
c   opnchm  -- reads the filenames and open all files
c   rdhchm  -- stores input EMBR header variables
c   rdhdr   -- reads the input EMBR header (library)
c   rduchm  -- reads the USERIN file
c   rdspec  -- reads the /SPECIES LIST/ packet (library)
c   rdcnty  -- reads the /COUNTY/ packet (library)
c   fndkey  -- searches a file for a keyword (library)
c   rdschm  -- reads the splitting factors file
c   cmpspc  -- compares the species list in USERIN to the EMBR species list
c   hdrchm  -- writes the header data to the output EMBR file
c   init_hash_msgtot  -- initializes hash table indexes
c   getime  -- sets the date and time string (library)
c   filhdr  -- reads and writes header data
c   rdxref  -- reads the ASC-SCC/profiles cross reference file (library)
c   prcchm  -- main processing routine
c   rdembr  -- reads one EMBR data record (library)
c   totchm  -- stores emissions in totals arrays
c   wrembr  -- writes one EMBR data record (library)
c   wrechm  -- writes the EMAR formatted error file
c   wremar  -- writes one EMAR data record
c   trachm  -- writes the EMBR trailer
c   rdtral  -- reads and writes the existing EMBR trailer
c   wtlchm  -- writes the tables of processed emissions
c   wrctbl  -- writes a table of totals (library)
c   clschm  -- closes all file units
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
      include 'iochmspl.inc'
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
      call opnchm(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read header of EMBR file
c      terminate if this file has already been speciated ---
c
      call rdhchm(ierr)
      if ( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rduchm(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the splits file ---
c
      if( lspltfl ) then
         call rdschm(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine which compares the species lists in
c      the splits file and the user input file ---
c
      call cmpspc(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write the EMBR header ---
c
      call hdrchm(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read ASC-SCC cross reference file ---
c
      if( lxrffl ) then
         call rdxref(ierr,IORXRF)
        if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine to process each EMBR record ---
c
      call prcchm(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write trailer ---
c
      call trachm(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals of tables ---
c
      call wtlchm(ierr)
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
      call clschm( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
