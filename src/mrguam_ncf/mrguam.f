C**** MRGUAM.F 
c
      program mrguam
c
c-----------------------------------------------------------------------
c
c    Merges up to ten UAM emissions files together to create a single
c    emissions file.  Only those species in the species list in
c    the USERIN file will be included.  The emsissions for each species
c    will be adjusted by the multiplicative factor in the /MRGUAM/
c    packet of USERIN.
c
c
c    Calling Sequence:
c
c            MRGUAM
c              |----- opnmrgu
c              |----- hdrmrgu
c              |         |----- rddate
c              |         |----- rdspec
c              |         |----- rdregn
c              |         |----- rdehdr
c              |         |----- ckhdr
c              |         |----- wrthdr
c              |
c              |----- rdmrgu
c              |         |----- fndkey
c              |
c              |----- prcmrgu
c              |         |----- rdemis
c              |         |----- wrtout
c              |
c              |----- clsmrgu
c
c   ckhdr   ---   verifys the consistency of header data
c   clsmrgu ---   closes all file units
c   fndkey  ---   searches a file for a string
c   header  ---   process the header records of the input files
c   opnmrgu ---   prompt for input files, open them
c   prcmrgu ---   read the non-header records, calculate sums
c   rddate  ---   reads the /DATE/ packet of USERIN (library)
c   rdspec  ---   reads the /SPECIES LIST/ packet of USERIN (library)
c   rdemis  ---   reads one hour of emissions
c   rdehdr   ---  reads the UAM header data
c   rdmrgu  ---   reads the /MRGUAM/ packet of USERIN
c   rdregn  ---   reads the /UAMREGN/ packet of USERIN (library)
c   wrctbl  ---   writes one table of emissions totals
c   wrthdr  ---   writes the new UAM header
c   wrtout  ---   writes one hpur of emissions to merged file
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iomrguam.inc'
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   ierr I  error code; passed to each subroutine
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- call routine to get I/O filenames and open I/O files
c      return the number of files which we will be merging --
c
      call opnmrgu(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the headers from all input files
c      check for conformance with USERIN (time and region)
c      if okay, write new header (use species list from USERIN) ---
c
      call hdrmrgu(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to process all files
c      calculate sums
c      note to message file for excluded species ----
c
      call prcmrgu(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write the output tables ---
c
       call wtlmrgu( ierr )
       if( ierr .NE. ISUCES ) goto 9999
c
c  --- looks successful, write message and reset flag ---
c
      write(IOWSTD,'(//,2A)',ERR=9999) 'Successful completion of ',
     &                                                   TRIM(VERSON)
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
      call clsmrgu( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
