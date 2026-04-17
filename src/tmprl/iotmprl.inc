c*** IOTMPRL.INC 
c
c-----------------------------------------------------------------------
c
c    include file for I/O parameters and variables for TMPRL program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   C   character string paramater containing version of the
c                TMPRL program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 TMPRL module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORINP   I   unit number for user input file
c   IORMBR   I   unit number for input EMBR file
c   IORPRF   I   unit number for temporal profiles file
c   IORXRF   I   unit number for ASC-SCC cross reference file
c   IOWERR   I   unit number for EMAR error file
c   IOWMBR   I   unit number for output EMBR file
c   IOWMSG   I   unit number for message file
c
      integer*4 IORINP
      integer*4 IORMBR
      integer*4 IORPRF
      integer*4 IORXRF
      integer*4 IOWERR
      integer*4 IOWMBR
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IORMBR = 11 )
      parameter ( IORXRF = 12 )
      parameter ( IORPRF = 13 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWMBR = 21 )
      parameter ( IOWERR = 30 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   errfl    C   EMAR formatted error file
c   imbrfl   C   input EMBR filename
c   msgfl    C   message filename
c   ombrfl   C   output EMBR filename
c   prffl    C   temporal profiles filename
c   uinpfl   C   user input filename
c   xrffl    C   ASC-SCC cross reference filename
c
      character*(MXSTR) errfl
      character*(MXSTR) imbrfl
      character*(MXSTR) msgfl
      character*(MXSTR) ombrfl
      character*(MXSTR) prffl
      character*(MXSTR) uinpfl
      character*(MXSTR) xrffl
c
      common /iofil/ uinpfl, imbrfl, xrffl, prffl, msgfl, ombrfl, errfl
