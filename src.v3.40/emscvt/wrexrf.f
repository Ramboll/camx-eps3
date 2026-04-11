C**** WREXRF.F 
c

      subroutine wrexrf( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the ASC-SCC/profiles cross reference file, including the
c    newly computes and update cross reference data
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
      include 'sccxref.inc'
      include 'ioemscvt.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External functions
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- loop over all cross refernce records ---
c
      do i=1,npfxrf
c
c   --- read next record ---
c
          write(IOWXRF,'(A5,1X,3(A10,1X),A)',ERR=7000) fipxrf(i), 
     &                      facxrf(i),stkxrf(i), sccxrf(i), prfxrf(i)
      enddo
c
c   --- processed entire file, echo to message file ---
c
      write(IOWMSG,'(/,2A,I8)',ERR=7001) 'Number of ASC-SCC/profile',
     &                    ' cross reference records written: ',npfxrf
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing the output',
     &                  ' ASC-SCC/profile code cross reference file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',oxrfl(:strmin(oxrfl))
      goto 9999
c
 7001 continue
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
