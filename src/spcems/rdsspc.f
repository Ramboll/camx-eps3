C**** RDSSPC.F
c
      subroutine rdsspc( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the speciation factors file.
c    Store records only needed for current EMBR file; 
c          criteria pollutants are specified in EMBR header
c          AQM compounds specified in /SPECIES LIST/
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c     25july2012 mjimenez  moved the check for AQM to follow section
c                          where the profile ID is stored.  This allows
c                          all profiles to be stored where previously
c                          if a profile only included AQM that were not
c                          in the species list then the profile was not
c                          stored...or found in look up.
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
      include 'uamcntl.inc'
      include 'iospcems.inc'
      include 'spcems.inc'
      include 'factors.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   fndchr  I   returns the index of an character string in an array
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 fndchr
      integer*4 strmin
c
c-----------------------------------------------------------------------
c   Local Parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(5*MXSTR) string
      character*(LENPRF)  prftmp
      character*10        poltmp, aqmtmp
      integer*4           i, irec
      integer*4           idxprf, idxcrt, idxspc
      real*4              numtmp, dentmp, mastmp
      logical*4           lfirst
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      nprf = 0
      irec = 0
      lfirst = .TRUE.
c
c  --- initialize species list for checking AQM parents ---
c
      do i = 1, MXSPEC
         iparnt(i) = 0
         ncntpar(i) = 0
      enddo
c
c  --- read factors to end of file ---
c  --- any line that starts with a # in column 1 is a comment line ---
c
  111 continue
      irec = irec + 1
      read(IORSPL,'(A)',ERR=7005,END=222) string
      if ( string(1:1) .EQ. '#' )  goto 111
      call lftjst ( string )
      if ( string .EQ. ' ' )  goto 111

      read(string,*,ERR=7006) prftmp, poltmp, aqmtmp,
     &                        numtmp, dentmp, mastmp

      call lftjst( prftmp )
      call lftjst( poltmp )
      call low2up( poltmp )
      call lftjst( aqmtmp )
      call low2up( aqmtmp )
c
      if ( strmin(poltmp) .GT. 10) goto 7002
      if ( strmin(aqmtmp) .GT. 10) goto 7003
c
c     --- check that this criteria is in our inventory ---
c     --- or if applying conversion factors if it matches TO pollutant ---
c
      idxcrt = fndchr ( poltmp, 10, crtnam, ncrthd )
      if ( lconv ) then
         if ( poltmp .EQ. tocrt ) idxcrt = idxfrm
         if ( poltmp .EQ. fromcrt ) idxcrt = 0
      endif
      if ( idxcrt .EQ. 0 ) goto 200
c
cmmj moved to follow profile index
c     --- check that this AQM is requested output ---
c
cmmj      idxspc = fndchr ( aqmtmp, 10, spcnam, nspecs )
cmmj      if ( idxspc .EQ. 0 ) goto 200
c
c     --- check for profile index ---
c     --- if new profile, add to list ---
c
      idxprf = fndchr ( prftmp, LENPRF, prffac, nprf )
      if ( idxprf .EQ. 0 )  then
          nprf = nprf + 1
          if( nprf .GT. MXSPL ) goto 7004
c
          prffac(nprf) = prftmp
          idxprf = nprf
      endif
c
c     --- check that this AQM is requested output ---
c
      idxspc = fndchr ( aqmtmp, 10, spcnam, nspecs )
      if ( idxspc .EQ. 0 ) goto 200
c
c
c  --- store the fractions, if denominator is zero, set to 1.0 ---
c
       if (dentmp .LE. 0.) then
          dentmp = 1.0
          write(ioerr,'(/,2A,I8)',ERR=9999)
     &         'WARNING:  Setting a zero or less denominator to 1.0 ',
     &                    'for speciation factors at line: ', irec
       endif
       spcfac(idxprf,idxcrt,idxspc) = numtmp/dentmp
c  --- set the check flag for conversion factors for this profile ---
       if ( lconv .AND.  poltmp .EQ. tocrt ) lchkcnv(idxprf) = .TRUE.
c
c  --- keep track of which parent criteria generate each AQM compound ---
c
      if ( iparnt(idxspc) .EQ. 0 )  then
          iparnt(idxspc) = idxcrt
          ncntpar(idxspc) = 1
      else if ( iparnt(idxspc) .NE. idxcrt )  then
          ncntpar(idxspc) = 2
      endif
c
c  --- get next record ---
c
  200 continue
c
c  --- if this is the first record, write out the note about
c      profile code length ---
c
      if( lfirst ) goto 6000
      goto 111
c
  222 continue
c
c  --- write count to message file ---
c
      irec = irec - 1
      write(IOWMSG,'(/,1X,A,I8)',ERR=7007)
     &    'Number of speciation factor records read:', irec
      write(IOWMSG,'(/,1X,A,I8)',ERR=7007)
     &    'Number of unique speciation profiles stored:', nprf
cqa     --- for QA ---
cqa      do i = 1, MXSPEC
cqa      write(*,*) spcnam(i),spcrit(i),crtnam(iparnt(i)), ncntpar(i)
cqa      enddo
cqa      do i = 1, nprf
cqa         write (*,*) prffac(i), lchkcnv(i)
cqa      enddo
cqa        do idxcrt = 1, ncrthd
cqa           do idxspc = 1, nspecs
cqa          write(*,*) i, crtnam(idxcrt), 
cqa     &                   spcnam(idxspc), spcfac(i,idxcrt,idxspc)
cqa      enddo
cqa      enddo
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning Messages:
c-----------------------------------------------------------------------
c
 6000 continue
      write(ioerr,'(/,5X,70A)',ERR=9999) ('*',i=1,70)
      write(ioerr,'(5X,A,T75,A)',ERR=9999) '*','*'
      write(ioerr,'(5X,3A,I2,T75,A)',ERR=9999) '*','  NOTE: This ',
     &   'version is compiled for profile codes of length: ',LENPRF,'*'
      write(ioerr,'(5X,3A,T75,A)',ERR=9999) '*','  Please verify ',
     &             'that this is the correct length for your data.','*'
      write(ioerr,'(5X,3A,T75,A)',ERR=9999) '*','  The first record ',
     &           'of your speciation factors file is shown below: ','*'
      write(ioerr,'(5X,A,T75,A)',ERR=9999) '*','*'
      write(ioerr,'(5X,70A,/)',ERR=9999) ('*',i=1,70)
      write(ioerr,'(/,A,/)',ERR=9999) string(:strmin(string))
      lfirst = .FALSE.
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7002 continue
      write(ioerr,'(/,A,I10)',ERR=9999) 
     &       'ERROR:  Reading speciation factors file at line ', irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      write(ioerr,'(2A)',ERR=9999) 'ERROR:  Criteria pollutant name ',
     &                       'exceeds character width allowed of 10.'
      write(ioerr,'(A)',ERR=9999) 'Record will be skipped.'
      write(ioerr,'(A//)',ERR=9999) string
      goto 111
c
 7003 continue
      write(ioerr,'(/,A,I10)',ERR=9999) 
     &       'ERROR:  Reading speciation factors file at line ', irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      write(ioerr,'(2A)',ERR=9999) 'ERROR:  AQM compound name ',
     &                       'exceeds character width allowed of 10.'
      write(ioerr,'(A)',ERR=9999) 'Record will be skipped.'
      write(ioerr,'(A//)',ERR=9999) string
      goto 111
c
 7004 continue
      write(ioerr,'(/,A)',ERR=9999) 
     &       'ERROR:  Reading speciation factors file.'
      write(ioerr,'(2A)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      write(ioerr,'(2A)',ERR=9999) 'ERROR:  Number of profiles ',
     &                            'in speciation file exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSPL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSPL ',
     &                                                  'and recompile.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     & 'ERROR:  Reading speciation factors file at line: ',
     &    irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      goto 9999
c
 7006 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     & 'ERROR:  Reading data of speciation factors at line: ',
     &    irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',spltfl(:strmin(spltfl))
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                         string(:strmin(string))
      goto 9999
c
 7007 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
