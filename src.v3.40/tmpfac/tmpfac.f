C**** TMPFAC.F 
c
      program tmpfac
c
c-----------------------------------------------------------------------
c
c    This program reads an AFS input file and creates the two temporal 
c    profiles used by the tmprl program.  The first of these is the 
c    cross reference file and the other is the actual profiles file.  
c    The cross reference and profile data is read from existing files 
c    and any AFS records with profiles not already existing in these 
c    input files are added to the list and written to the newly created 
c    files.
c
c
c    Calling Sequence:
c
c            TMPFAC
c              |----- opnfac
c              |----- rdfxrf
c              |        |----- setfxf
c              |
c              |----- rdfprf
c              |        |----- fndkey
c              |
c              |----- prcafs
c              |        |----- fndfxf
c              |        |----- setwek
c              |        |----- setmon
c              |        |----- setday
c              |
c              |----- wrexrf
c              |----- wrfprf
c              |----- clsfac
c
c  clsfac -- closes all file units
c  fndkey -- searches a file for a keyword
c  fndfxf -- returns the best match of data in cross reference file
c  opnfac -- reads all filenames and opens all files
c  prcafs -- main processing routine for AFS file
c  rdfprf -- reads the existing temporal profiles file
c  rdfxrf -- reads existing cross reference file
c  setday -- finds code for diurnal profile or adds new one to array
c  setmon -- finds code for monthly profile or adds new one to array
c  setwek -- finds code for day of week profile or adds new one to array
c  setfxf -- returns an exact match of data in cross reference file
c  wrfprf -- writes new temporal profiles file
c  wrfxrf -- writes new cross reference file
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
      include 'iotmpfac.inc'
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   ierr  I  error flag for subroutine calls
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Entry Point:
c-----------------------------------------------------------------------
c
c  --- call routine to get I/O filenames and open I/O files
c
      call opnfac(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the existing XREF file ---
c
      call rdfxrf(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the existing XREF file ---
c
      if( lprof ) then
         call rdfprf(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- process the AFS records ---
c
      if( lafs ) then
         call prcafs(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routines to write newly created data ---
c
      call wrfxrf(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
      call wrfprf(ierr)
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
c
c  --- call routine to close files ---
c
 9999 continue
      call clsfac()
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
