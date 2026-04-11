c*** IOPSTFIR.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for PSTFIR program
c
c-----------------------------------------------------------------------
c    LOG:  June 2015 mjimenez
c          Added additional output file - CAMx binary ptsrce file
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c    Program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   C   character string paramater containing version of the
c                PSTFIR program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 PSTFIR module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORMBR   I   unit number for input EMBR file
c   IORINP   I   unit number for user input file
c   IOWELV   I   unit number for output elevated points emissions file
c   IOWERR   I   unit number for EMAR error file
c   IOWHDR   I   unit number for output elevated points header file
c   IOWMSG   I   unit number for output message file
c   IOWPTS   I   unit number for output binary ptsrce file
c   IOWSRF   I   unit number for output surface emissions file
c
      integer*4 IORMBR
      integer*4 IORINP
      integer*4 IOWERR
      integer*4 IOWELV
      integer*4 IOWHDR
      integer*4 IOWPTS
      integer*4 IOWSRF
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IORMBR = 11 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWELV = 21 )
      parameter ( IOWHDR = 22 )
      parameter ( IOWSRF = 23 )
      parameter ( IOWPTS = 24 )
      parameter ( IOWERR = 30 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   errfl       C   EMAR error filename
c   imbrfl      C   input EMBR filename
c   msgfl       C   message filename
c   oelvfl      C   output elevated points emissions filename
c   ohdrfl      C   output elevated points header filename
c   osrffl      C   output surface emissions filename
c   optsfl      C   output binary ptsrce filename
c   uinpfl      C   user input filename
c
c   lelvfl      L   TRUE if elevated point source file requested
c   lptsfl      L   TRUE if binary ptsrce file requested
c   lsrffl      L   TRUE if surface file requested
c   ncf_iowemis I   file handle for output NetCDF file
c   ncf_emsfile C   name of output NetCDF file
c   l3demiss    L   TRUE if writing 3D NetCDF output file
c
c
      character*(MXSTR) errfl
      character*(MXSTR) imbrfl
      character*(MXSTR) msgfl
      character*(MXSTR) oelvfl
      character*(MXSTR) ohdrfl
      character*(MXSTR) osrffl
      character*(MXSTR) optsfl
      character*(MXSTR) uinpfl
      character*(MXSTR) ncf_emsfile
c
      logical*4 lelvfl
      logical*4 lsrffl
      logical*4 lptsfl
      logical*4 l3demiss
      integer*4 ncf_iowemis
c
      common /iofil/ uinpfl, imbrfl, msgfl, oelvfl, ohdrfl, osrffl, 
     &               optsfl, errfl, ncf_emsfile
      common /ioflg/ lelvfl, lsrffl, lptsfl, ncf_iowemis, l3demiss
