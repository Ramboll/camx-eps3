C**** RDMASK.F version 2.0 09/01/2007
c
      subroutine rdmask( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the mask file for the PIGEMS program.  This file contains a
c    value for each grid cell in the domain.  The value indicates a
c    code in the /THRESHOLD/ packet that maps to a threshold value
c    which will determine if a stack within the cell should be PiGged. 
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
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string
c   fndint  I   returns the index of an integer value in an array
c
      integer*4 strmin
      integer*4 fndint
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*1000 string
      integer*4      irowvl(MXCELX), irow, irec, idxmsk, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- if the mask file was not supplied, just exit ---
c
      if( .NOT. lmaskfl ) then
          ierr = ISUCES
          goto 9999
      endif 
c
c
c  ---- loop over rows (Y direction), go backwards since the ASCII map
c       is a grid image ---
c
         irec = 0
         do irow=nycell,1,-1
           irec = irec + 1
           read(IORMSK,'(A)',ERR=7000,END=7001) string
           read(string,'(999(:,I3))',ERR=7002) (irowvl(i),i=1,nxcell)
c
c  --- make sure each code is valid ---
c
           do i=1,nxcell
              idxmsk = fndint( irowvl(i), nthresh, ithrshcd )
              if( idxmsk .LE. 0 ) goto 7003
c
c  --- load into the global array ---
c      
              imskval(i,irow) = irowvl(i)
           enddo
         enddo
c
c  --- entire file read ---
c
         ierr = ISUCES
         goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I4)',ERR=9999) 
     &           'ERROR: Reading input mask file at record: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File read: ',
     &                                          maskfl(:strmin(maskfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I4)',ERR=9999) 
     &           'ERROR: Reading input mask file at record: ',irec
      write(ioerr,'(A)',ERR=9999) 'Premature end-of-file reached.'
      write(ioerr,'(2A)',ERR=9999) 'Make sure the mask file is for ',
     &                 'the correct domain definition.'
      write(ioerr,'(2A,//)',ERR=9999) 'File read: ',
     &                                          maskfl(:strmin(maskfl))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A,I4)',ERR=9999)
     &           'ERROR: Reading input mask file at record: ',irec
      write(ioerr,'(2A,/)',ERR=9999) 'File read: ',
     &                                          maskfl(:strmin(maskfl))
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A,I4)',ERR=9999) 
     &           'ERROR: Reading input mask file at record: ',irec
      write(ioerr,'(2A)',ERR=9999) 'The mask file includes a value ',
     &                        'that is not in the /THRESHOLD/ packet'
      write(ioerr,'(A)',ERR=9999) 'of the USERIN file.'
      write(ioerr,'(T10,A,I3)',ERR=9999) 
     &                              'Value from mask file: ',irowvl(i)
      write(ioerr,'(2A,/)',ERR=9999) 'File read: ',
     &                                          maskfl(:strmin(maskfl))
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                          string(:strmin(string))
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
