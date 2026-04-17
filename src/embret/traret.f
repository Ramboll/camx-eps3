C**** TRARET.F 
c 
      subroutine traret( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the trailer for the EMAR file after processing of the
c    EMBRET program
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
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
      include 'embrdat.inc'
      include 'ioembret.inc'
      include 'embretot.inc'
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
c   strmin   I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   KEYTRA  C   keyword for trailer
c   NIN     I   number of input files
c   NOUT    I   number of output files
c
      character*(KEYLEN) KEYTRA
      integer*4          NIN
      integer*4          NOUT
c
      parameter( KEYTRA = '/TRAILER/' )
      parameter( NIN = 2 )
      parameter( NOUT = 2 )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   string  C   character string for temporary storage
c
      character*(LENREC) string
      integer*4          i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- write keyword ---
c
      write(IOWEMR,'(A)',ERR=7000) KEYTRA
c
c   --- write number of records in this file ---
c
      read(IORMBR,ERR=7001,END=222) string
      write(IOWEMR,'(I10)',ERR=7000) nrec(IDXOUT)
c
c   --- write exinsting trailer ---
c
  111 continue
      read(IORMBR,ERR=7001,END=222) string
      write(IOWEMR,'(A)',ERR=7000) string(:strmin(string))
      goto 111
c
c   --- write the name of this module, and the number of input files
c       and output files ---
c
  222 continue
      write(IOWEMR,'(A30,2I4)',ERR=7000) VERSON, NIN, NOUT
      if( lusrin ) then
         string = 'User Input          ' // uinpfl(:strmin(uinpfl))
      else
         string = 'User Input          ' // 'No user input file.'
      endif
      write(IOWEMR,'(A)',ERR=7000) string(:strmin(string))
      string = 'Input EMBR          ' // embrfl(:strmin(embrfl))
      write(IOWEMR,'(A)',ERR=7000) string(:strmin(string))
      string = 'Message             ' // msgfl(:strmin(msgfl))
      write(IOWEMR,'(A)',ERR=7000) string(:strmin(string))
      string = 'Output EMAR         ' // emarfl(:strmin(emarfl))
      write(IOWEMR,'(A)',ERR=7000) string(:strmin(string))
c
c  --- write emissions totals ---
c
      write(IOWEMR,9000,ERR=7000) 'Input Emissions',
     &                                   (totems(IDXIN,i),i=1,ncrthd)
      write(IOWEMR,9000,ERR=7000) 'Output Emissions',
     &                                   (totems(IDXOUT,i),i=1,ncrthd)
c
c   --- set erorr flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c
c-----------------------------------------------------------------------
c   Error Statements:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Writing TRAILER section ',
     &                              'of output EMAR file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',emarfl(:strmin(emarfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading TRAILER section ',
     &                              'of input EMBR file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',embrfl(:strmin(emarfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(A20,200(:,1X,E10.4,1X))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
