C**** RDCSPC.F
c
      subroutine rdcspc( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the pollutant conversion factors file.
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
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
      character*(LENPRF)  tmpprf
      character*(10)      tmpfrm, tmpto
      integer*4           i, irec, idxuse, nomatch
      real*4              tmpfac
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      ncnv = 0
      irec = 0
c
c  --- check if user wants to apply conversion factors ---
c  --- if not, skip the read                           ---
c  --- if yes, verify the file is specified/opened     ---
c
      if ( .NOT. lconv ) then
         if ( lcnvfl ) then
             write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
             write(ioerr,'(1X,2A)',ERR=9999) 
     &              'WARNING: User provided NO ',
     &              ' Conversion Factor pollutants in /SPCEMS/.'
             write(ioerr,'(1X,2A)',ERR=9999) 
     &              '         A factors file was ',
     &                 'specified but not used.'
             write(ioerr,'(1X,2A)',ERR=9999)  
     &              '         File: ',cnvfl(:strmin(cnvfl))
             write(ioerr,'(/40A,//)',ERR=9999) ('/\\',i=1,40)
         endif
         goto 222
      endif

      if ( .NOT. lcnvfl )  goto 7008
c
c     --- initialize conversion factor profile found array ---
c
      do i = 1, nprf
         lcnvset(i) = .FALSE.
      enddo
c
c     --- read factors to end of file ---
c     --- any line that starts with a # in column 1 is a comment line ---
c     --- skip any blank lines ---
  111 continue
      irec = irec + 1
      read(IORCNV,'(A)',ERR=7005,END=222) string
      if ( string(1:1) .EQ. '#' )  goto 111
      call lftjst ( string )
      if ( string .EQ. ' ' )  goto 111
c
      read(string,*,ERR=7006) tmpfrm, tmpto, tmpprf, tmpfac
      call lftjst( tmpfrm )
      call low2up( tmpfrm )
      call lftjst( tmpto )
      call low2up( tmpto )
      call lftjst( tmpprf )

c     --- skip if FROM and TO pollutant names do not match userin ---
      if ( tmpfrm .NE. fromcrt 
     &       .OR. tmpto .NE. tocrt ) goto 111

c     --- skip if not in speciation profiles list ---
      idxuse = fndchr ( tmpprf, LENPRF, prffac, nprf )
      if ( idxuse .EQ. 0 )  goto 111

      ncnv = ncnv + 1
      cnvftr(idxuse) = tmpfac
      lcnvset(idxuse) = .TRUE.
c
c  --- get next record ---
c
      goto 111
c
  222 continue
      if ( irec .EQ. 0 ) irec = 1
c
c  --- end of file read ---
c  --- if applying conversion factors check that each profile which ---
c  --- requires a conversion factor has one ---
c
cqa       do i = 1, nprf
cqa          write(*,*) prffac(i), lcnvset(i)
cqa       enddo
       nomatch = 0
       if ( lconv ) then
          do i = 1, nprf
             if ( lchkcnv(i)) then
                if ( .NOT. lcnvset(i) ) then
                   if ( nomatch .EQ. 0 ) then
                      write(ioerr,'(/,2A)',ERR=9999)
     &                'ERROR:  Missing conversion factors for',
     &                        ' the following profiles:'
                   endif
                   nomatch = nomatch + 1
                   write(ioerr,'(2X,A)',ERR=9999) prffac(i)
                endif
             endif
          enddo
       endif
       if ( nomatch .GT. 0 ) goto 7009
c
c  --- write count to message file ---
c
      write(IOWMSG,'(/,1X,A,I8)',ERR=7007)
     &  'Number of pollutant conversion factor records read:',
     &     irec-1
      write(IOWMSG,'(/,1X,A,I8)',ERR=7007)
     &  'Number of pollutant conversion factors stored:',
     &     ncnv
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7005 continue
      write(ioerr,'(/,2A,I8)',ERR=9999)
     & 'ERROR:  Reading pollutant conversion factors file',
     & ' at line: ', irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',cnvfl(:strmin(cnvfl))
      goto 9999
c
 7006 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR:  Reading data ',
     &      'of pollutant conversion factors at line: ', irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',cnvfl(:strmin(cnvfl))
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
 7008 continue
      write(ioerr,'(/,2A)',ERR=9999) 
     &                 'ERROR: User provided pollutants ',
     &                 'to Apply Conversion Factors in /SPCEMS/.'
      write(ioerr,'(2A//)',ERR=9999) 
     &                 '       No conversion factors file was ',
     &                 'specified for reading.'
      goto 9999
c
 7009 continue
      write(ioerr,'(/,A)',ERR=9999) 
     &            'ERROR: Required conversion factors not provided.'
      write(ioerr,'(A)',ERR=9999) 
     & '       Add the missing profiles to the conversion factor file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',cnvfl(:strmin(cnvfl))
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
