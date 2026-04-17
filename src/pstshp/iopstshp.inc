c*** IOPSTSHP.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for PSTSHP program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
c    jan  2018 gwilson  added 3D emissions in NetCDF format
c
c-----------------------------------------------------------------------
c    Program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   C   character string paramater containing version of the
c                PSTSHP program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 PSTSHP module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORMBR   I   unit number for input EMBR file
c   IORINP   I   unit number for user input file
c   IORVSL   I   unit number for user input file
c   IOWELV   I   unit number for output elevated points emissions file
c   IOWERR   I   unit number for EMAR error file
c   IOWHDR   I   unit number for output elevated points header file
c   IOWMSG   I   unit number for output message file
c   IOWSRF   I   unit number for output surface emissions file
c
      integer*4 IORMBR
      integer*4 IORINP
      integer*4 IORVSL
      integer*4 IOWERR
      integer*4 IOWELV
      integer*4 IOWHDR
      integer*4 IOWSRF
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IORMBR = 11 )
      parameter ( IORVSL = 12 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWELV = 21 )
      parameter ( IOWHDR = 22 )
      parameter ( IOWSRF = 23 )
      parameter ( IOWERR = 30 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   errfl       C   EMAR error filename
c   imbrfl      C   input EMBR filename
c   vslmapfl    C   input EMBR filename
c   msgfl       C   message filename
c   oelvfl      C   output elevated points emissions filename
c   ohdrfl      C   output elevated points header filename
c   osrffl      C   output surface emissions filename
c   uinpfl      C   user input filename
c
c   lelvfl      L   TRUE if elevated point source file requested
c   lsrffl      L   TRUE if surfac file requested
c
c   ncf_iowemis I   file handle for output NetCDF file
c   ncf_emsfile C   name of output NetCDF file
c   l3demiss    L   TRUE if writing 3D NetCDF output file
c
      character*(MXSTR) errfl
      character*(MXSTR) imbrfl
      character*(MXSTR) vslmapfl
      character*(MXSTR) msgfl
      character*(MXSTR) oelvfl
      character*(MXSTR) ohdrfl
      character*(MXSTR) osrffl
      character*(MXSTR) uinpfl
      character*(MXSTR) ncf_emsfile
c
      logical*4 lelvfl
      logical*4 lsrffl
      logical*4 l3demiss
      integer*4 ncf_iowemis
c
      common /iofil/ uinpfl, imbrfl, vslmapfl, msgfl, oelvfl, ohdrfl, 
     &               osrffl, errfl, ncf_emsfile
      common /ioflg/ lelvfl, lsrffl, ncf_iowemis, l3demiss
