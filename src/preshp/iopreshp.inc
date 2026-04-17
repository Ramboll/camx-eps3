c*** IOPRESHP.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for PRESHP program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c    Program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   C   character string paramater containing version of the
c                SHPPNT program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 PRESHP module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORSHP   I   unit number for input file
c   IORINP   I   unit number for user input file
c   IOWERR   I   unit number for EMAR error file
c   IOWMBR   I   unit number for output EMBR file
c   IOWMSG   I   unit number for message file
c   IOWSTK   I   unit number for stack report file
c
      integer*4 IORSHP
      integer*4 IORINP
      integer*4 IOWERR
      integer*4 IOWMBR
      integer*4 IOWMSG
      integer*4 IOWSTK
c
      parameter ( IORINP = 10 )
      parameter ( IORSHP = 11 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWMBR = 21 )
      parameter ( IOWSTK = 22 )
      parameter ( IOWERR = 30 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   uinpfl   C   USERIN file
c   shipsfl  C   input SHIP data filename
c   errfl    C   EMAR error filename
c   msgfl    C   message filename
c   ombrfl   C   output EMBR filename
c
      character*(MXSTR) uinpfl
      character*(MXSTR) shipsfl
      character*(MXSTR) errfl
      character*(MXSTR) msgfl
      character*(MXSTR) ombrfl
c
      common /iofil/ uinpfl, shipsfl, msgfl, ombrfl, errfl
