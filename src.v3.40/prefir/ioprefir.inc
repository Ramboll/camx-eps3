c*** IOPREFIR.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for PREFIR program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c    Program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   C   character string paramater containing version of the
c                PREFIR program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 PREFIR module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORAFS   I   unit number for input AFS file
c   IORINP   I   unit number for user input file
c   IOWERR   I   unit number for EMAR error file
c   IOWMBR   I   unit number for output EMBR file
c   IOWMSG   I   unit number for message file
c
      integer*4 IORAFS
      integer*4 IORINP
      integer*4 IOWERR
      integer*4 IOWMBR
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IORAFS = 11 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWMBR = 21 )
      parameter ( IOWERR = 30 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   afsfl    C   input AFS filename
c   errfl    C   EMAR error filename
c   msgfl    C   message filename
c   ombrfl   C   output EMBR filename
c   uinpfl   C   user input filename
c
      character*(MXSTR) afsfl
      character*(MXSTR) errfl
      character*(MXSTR) msgfl
      character*(MXSTR) ombrfl
      character*(MXSTR) uinpfl
c
      common /iofil/ uinpfl, afsfl, msgfl, ombrfl, errfl
