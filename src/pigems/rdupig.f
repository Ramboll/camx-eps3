C**** RDUPIG.F version 2.0 09/01/2007
c
      subroutine rdupig( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the various
c    packets used by the PIGEMS program.
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
      include 'uamcntl.inc'
      include 'cntylst.inc'
      include 'uamregn.inc'
      include 'iopigems.inc'
      include 'usrpigems.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c   rddate  --  reads the /DATE/ packet (library)
c   rdregn  --  reads the /REGION/ packet (library)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(KEYLEN) module
      character*40       cdate
      character*10       name
      integer*4          jerr, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- call routine to read control packet ---
c
      call rddate( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read criteria pollutants ---
c
      call rdcrit( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read species list packet ---
c
      call rdspec( jerr,
     &                   IORINP, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read region packet ---
c
      call rdregn( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read the threshold packet ---
c
      if( .NOT. lmaster ) then
          call rdthresh( jerr )
         if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read the combine stacks packet ---
c
          call rdcomb( jerr )
          if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- echo the data to message file ---
c
      call getime( cdate )
      write(IOWMSG,9000,ERR=7000) VERSON, cdate
      write(IOWMSG,9001,ERR=7000) 'File note',fnote(:strmin(fnote))
      write(IOWMSG,9003,ERR=7000) 'Episode date (Calander)',ibgcal
      write(IOWMSG,9002,ERR=7000) 'Episode date (Julian)',ibgdat
      write(IOWMSG,9002,ERR=7000) 'Beginning hour',INT(begtim/100.)
      write(IOWMSG,9002,ERR=7000) 'Ending hour',INT(endtim/100.)
      write(IOWMSG,9004,ERR=7000) 'Grid origin (km)',xorig,yorig
      write(IOWMSG,9002,ERR=7000) 'UTM zone',izone
      write(IOWMSG,9004,ERR=7000) 'Grid cell width (km)',dxcell,dycell
      write(IOWMSG,9005,ERR=7000) 'Number of cells',nxcell,nycell
      write(IOWMSG,9005,ERR=7000) 
      write(IOWMSG,9010,ERR=7000) 'Colocation dist (m)',colodist
c
c  --- echo the threshold values that were read ---
c
      write(IOWMSG,9006,ERR=7000) 
     &               'Threshold Code Values read from USERIN file.'
      write(IOWMSG,9007,ERR=7000) 'Code','Species','Level (tons/day)'
      do i=1,nthresh
         name = thrshspc(i)
         call rgtjst(name)
         write(IOWMSG,9008,ERR=7000) ithrshcd(i), name, thrshval(i)
      enddo
c
c  --- echo the mappings that were read ---
c
      write(IOWMSG,9006,ERR=7000) 
     &               'Threshold Code Mappings read from USERIN file.'
      write(IOWMSG,9007,ERR=7000) 'FIPS Code','Threshold Code'
      do i=1,nthrshmap
         write(IOWMSG,9009,ERR=7000) thrshcty(i), ithrshmap(i)
      enddo
c
c  --- set error flag to sucess ---
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
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(/,T20,A,A40,/)
 9001 format(T10,A,T40,:,':',A)
 9002 format(T10,A,T40,:,':',I5,I5)
 9003 format(T10,A,T40,:,':',I6)
 9004 format(T10,A,T40,:,': (',F10.3,',',F10.3,')')
 9005 format(T10,A,T40,:,': (',I5,',',I5,')')
 9006 format(//,T10,A)
 9007 format(T10,3X,A,3X,A,3X,A)
 9008 format(T10,3X,I3,X,A,X,F10.2)
 9009 format(T10,5X,A,4X,I3)
 9010 format(T10,A,T40,:,': ',F10.5)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
