C**** RDUPTS2EMS.F 
c
      subroutine rdupts2ems( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in globalvariables.
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
      include 'uamregn.inc'
      include 'uamcntl.inc'
      include 'embrdat.inc'
      include 'iopts2ems.inc'
      include 'pts2emstot.inc'
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
c   rdregn  --  reads the /UAMREGN/ packet (library)
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
      character*40 cdate
      integer*4    jerr, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
c   --- call routine to read region packet ---
c
      call rdregn( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- echo the data to message file ---
c
      call getime( cdate )
      write(IOWMSG,9000,ERR=7000) VERSON, cdate
      write(IOWMSG,9001,ERR=7000) 'Grid origin (km)',xorig,yorig
      write(IOWMSG,9002,ERR=7000) 'UTM zone',izone
      write(IOWMSG,9001,ERR=7000) 'Grid cell width (km)',dxcell,dycell
      write(IOWMSG,9003,ERR=7000) 'Number of cells',nxcell,nycell
c
c  --- assume all species are PM with molecular weight of 1 ---
c
      lgas_out = .TRUE.
      lgas = .TRUE.
      molwt = 1.0
      molwt_out = 1.0
c
c  --- call routine to read species list for molecular weight and 
c
      call rdspec( jerr,
     &                   IORINP, .FALSE. )
      if( jerr .EQ. ISUCES ) then
         nspecs_pkt = nspecs
         do i=1,nspecs_pkt
            spcnam_pkt(i) = spcnam(i)
            molwt_out(i) = molwt(i)
         enddo
      endif
c
c  --- set error flag to sucess ---
c
  222 continue
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
 9001 format(T10,A,T40,:,': (',F10.3,',',F10.3,')')
 9002 format(T10,A,T40,:,':',I5)
 9003 format(T10,A,T40,:,': (',I6,',',I6,')')
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
