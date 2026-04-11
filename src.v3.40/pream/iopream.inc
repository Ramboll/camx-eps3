c*** IOPREAM.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for PREAM program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   C   character string paramater conatining version of the
c                PREAM program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 PREAM module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORAMS   I   unit number for input AMS file
c   IORINP   I   unit number for user input file
c   IOWMBR   I   unit number for output mobile EMBR file
c   IOWERR   I   unit number for EMAR error file
c   IOWMSG   I   unit number for message file
c
      integer*4 IORAMS
      integer*4 IORINP
      integer*4 IOWMBR
      integer*4 IOWERR
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IORAMS = 11 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWMBR = 21 )
      parameter ( IOWERR = 30 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   amsfl    C   input AMS filename
c   errfl    C   EMAR error filename
c   msgfl    C   message filename
c   outmfl   C   output EMBR filename
c   uinpfl   C   user input filename
c
      character*(MXSTR) amsfl
      character*(MXSTR) errfl
      character*(MXSTR) msgfl
      character*(MXSTR) outmfl
      character*(MXSTR) uinpfl
c
      common /iofil/ uinpfl, amsfl, msgfl, outmfl, errfl
c
c-----------------------------------------------------------------------
c    Other parameters:
c-----------------------------------------------------------------------
c
c   MXAMS    I   length of AMS record as a character string
c
      integer*4 MXAMS
c
      parameter ( MXAMS = 280 )
