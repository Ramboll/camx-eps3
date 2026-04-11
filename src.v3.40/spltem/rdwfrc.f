C**** RDWFRC.F
c
      subroutine rdwfrc( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the weight fractions file.  Calls RDPROF routine from the 
c    EMSCVT module to read and store one profile at a time.  Moves
c    single profile of weight fractions to multidimensioned arrays.
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
c      08/12/97  --mmj--  original development
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iospltem.inc'
      include 'spltem.inc'
      include 'sccxref.inc'
      include 'emcvtprf.inc'
      include 'wgtfrac.inc'
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
c    rgtjst  --  right justifies a character string (library)
c    rdprof  --  reads a single profile of weight fractions
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c    fndint  I   returns index of an integer in list of integers (library)
c
      integer*4 fndint
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 idxsrd
      integer*4 jerr, ilen, i
      logical*4 leof
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c  --- initialize counters ---
c
      nprfwf = 0
      do 100 i = 1, MXWGT
         nwtswf(i) = 0
  100 continue
c
c
c  --- read a weight profile ---
c
      leof = .FALSE.
 111  continue
      call rdprof(jerr)
      if ( jerr .EQ. IEOD ) leof = .TRUE.
      if ( jerr .NE. ISUCES .AND. jerr .NE. IEOD ) goto 9999
c
c  --- move to multi-profile arrays ---
c
      nprfwf = nprfwf + 1
      if ( nprfwf .GT. MXPRF )  goto 7000
      profwf(nprfwf) = prfcod
      nwtswf(nprfwf) = nwate
      do 150 i = 1, nwate
         isrdwf(nprfwf,i) = icmpcd(i)
         idxsrd = fndint ( icmpcd(i), isrdcnt, isrdlst )
         if ( idxsrd .LE. 0 )  then
            isrdcnt = isrdcnt + 1
            if ( isrdcnt .GT. MXSRD )  goto 7002
            isrdlst(isrdcnt) = icmpcd(i)
         endif
         wgtswf(nprfwf,i) = wtfrac(i)
  150 continue
      if ( .NOT. leof ) goto 111
c
c   --- write summary to message file ---
c
      write(IOWMSG,9001,ERR=7001) 
     &             'Number of weight fraction profiles read: ', nprfwf
c
c   --- set error flag to sucess ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR: Exceed maximum number of ', 
     &                            'profile codes.  MXPRF = ', MXPRF
      write(IOWMSG,9000,ERR=7001) 'ERROR: Exceed maximum number of ',
     &                            'profile codes.  MXPRF = ', MXPRF
      goto 9999
c
 7001 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR: writing message file.'
      goto 9999
c
 7002 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR: Exceed maximum number of ', 
     &                           'SAROAD codes.  MXSRD = ', MXSRD
      write(IOWMSG,9000,ERR=7001) 'ERROR: Exceed maximum number of ',
     &                           'SAROAD codes.  MXSRD = ', MXSRD
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(1X,A,A,I5)
 9001 format(/,T10,A,I6)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
