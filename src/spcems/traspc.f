C**** TRASPC.F 
c
      subroutine traspc( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the trailer for the EMBR file after processing of the
c    SPCEMS program.
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
      include 'iospcems.inc'
      include 'factors.inc'
      include 'totspc.inc'
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
c     rdtral  --  reads the input EMBR trailer (library)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin   I   returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   NIN   I  number of input files
c   NOUT  I  number of output files
c
      integer*4 NIN
      integer*4 NOUT
c
      parameter( NIN  = 5 )
      parameter( NOUT = 3 )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   jerr    I   error flag for subroutine calls
c   string  C   character string for temporary storage
c
      character*(LENREC) string
      integer*4          jerr, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- call routine to read and write current trailer ---
c
      call rdtral(jerr,
     &                 IOREMB, IOWEMB, nrec(IDXOUT) )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- write the name of this module, and the number of input files
c       and output files ---
c
      write(string,'(A30,2I4)',ERR=7000) VERSON, NIN, NOUT
      write(IOWEMB,ERR=7000) string
c
c   --- write each input file ---
c
      string = 'USERIN              ' // uinpfl(:strmin(uinpfl))
      write(IOWEMB,ERR=7000) string
c
      string = 'EMBRIN              ' // infl(:strmin(infl))
      write(IOWEMB,ERR=7000) string
c
      string = 'XREF                ' // xrffl(:strmin(xrffl))
      write(IOWEMB,ERR=7000) string
c
      string = 'SPECFAC             ' // spltfl(:strmin(spltfl))
      write(IOWEMB,ERR=7000) string
c
      if ( lcnvfl )  then
         string = 'CNVFAC              ' // cnvfl(:strmin(cnvfl))
      else
         string = 'CNVFAC              ' // 'Not Supplied'
      endif
      write(IOWEMB,ERR=7000) string
c
c   --- write each output file ---
c
      string = 'MESSAGE             ' // msgfl(:strmin(msgfl))
      write(IOWEMB,ERR=7000) string
c
      string = 'EMBROUT             ' // outfl(:strmin(outfl))
      write(IOWEMB,ERR=7000) string
c
      string = 'EMAR                ' // errfl(:strmin(errfl))
      write(IOWEMB,ERR=7000) string
c
c  --- write emissions totals ---
c
      write(string,9000,ERR=7000) 'Input Emissions     ',
     &                                   (totems(IDXIN,i),i=1,ncrthd)
      write(IOWEMB,ERR=7000) string
      write(string,9000,ERR=7000) 'Output Emissions    ',
     &                                   (totems(IDXOUT,i),i=1,ncrthd)
      write(IOWEMB,ERR=7000) string
c
c  --- if we have written EMAR records, write the trailer section of
c      the EMAR file ---
c
      if( nrec(IDXERR) .GT. 0 ) then
          write(IOWERR,'(A)',ERR=7001) '/TRAILER/'
          write(IOWERR,'(I10)',ERR=7001) nrec(IDXERR)
      endif
c
c   --- set error flag to sucess ---
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
     &                              'of output EMBR file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',outfl(:strmin(outfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Writing TRAILER section ',
     &                              'of output EMAR file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',errfl(:strmin(errfl))
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
