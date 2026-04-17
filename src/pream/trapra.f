C**** TRAPRA.F 
c
      subroutine trapra( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the trailer for both the mobile and area
c    EMBR file after processing of the PREAM program
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
      include 'msgtot.inc'
      include 'iopream.inc'
      include 'preamtot.inc'
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
c   NIN    I  number of input files
c   NOUT   I  number of output files
c   TRALER C  keyword for /TRAILER/ keyword
c
      character*(KEYLEN) TRALER
      integer*4          NIN
      integer*4          NOUT
c
      parameter( TRALER = '/TRAILER/')
      parameter( NIN  = 2 )
      parameter( NOUT = 3 )
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
c   --- write trailer keyword ---
c
      string = TRALER
      write(IOWMBR,ERR=7000) string
c
c   --- write number of records in this file ---
c
      write(string,'(I10)',ERR=7000) nrec(IDXOUT)
      write(IOWMBR,ERR=7000) string
c
c   --- write the name of this module, and the number of input files
c       and output files ---
c
      write(string,'(A30,2I4)',ERR=7000) VERSON, NIN, NOUT
      write(IOWMBR,ERR=7000) string
c
c   --- write each input file ---
c
      string = 'USERIN              ' // uinpfl(:strmin(uinpfl))
      write(IOWMBR,ERR=7000) string
c
      string = 'AMS                 ' // amsfl(:strmin(amsfl))
      write(IOWMBR,ERR=7000) string
c
c   --- write each output file ---
c
      string = 'MESSAGE             ' // msgfl(:strmin(msgfl))
      write(IOWMBR,ERR=7000) string

      string =  'EMBROUT            ' // outmfl(:strmin(outmfl))
      write(IOWMBR,ERR=7000) string
c
      string = 'ERROR               ' // errfl(:strmin(errfl))
      write(IOWMBR,ERR=7000) string
c
c  --- write emissions totals ---
c
      write(string,9000,ERR=7000) 'Input Emissions',
     &                                   (totems(IDXIN,i),i=1,ncrthd)
      write(IOWMBR,ERR=7000) string
c
      write(string,9000,ERR=7000) 'Output Emissions',
     &                                   (totems(IDXOUT,i),i=1,ncrthd)
      write(IOWMBR,ERR=7000) string
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error statements:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Writing TRAILER section ',
     &                              'of output EMBR file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',outmfl(:strmin(outmfl))
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
