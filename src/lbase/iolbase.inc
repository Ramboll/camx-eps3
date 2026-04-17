c*** IOLBASE.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for LBASE program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Program Version Parameter:
c-----------------------------------------------------------------------
c
c   VERSON   c   character string paramater conatining version of the
c                lbase program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 LBASE module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORINP   I   unit number for user input file
c   IORLNK   I   unit number for input links file
c   IOWMBR   I   unit number for output emissions file
c   IOWMSG   I   unit number for message file
c
      integer*4 IORINP
      integer*4 IORLNK
      integer*4 IOWMBR
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IORLNK = 11 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWMBR = 21 )
c
c-----------------------------------------------------------------------
c   I/O Filenames:
c-----------------------------------------------------------------------
c
c   embrfl   C   output EMBR filename
c   linkfl   C   input link emissions file
c   msgfl    C   message filename
c   uinpfl   C   user input filename
c
      character*(MXSTR) embrfl
      character*(MXSTR) linkfl
      character*(MXSTR) msgfl
      character*(MXSTR) uinpfl
c
      common /iofil/ uinpfl, linkfl, msgfl, embrfl
c
c-----------------------------------------------------------------------
c   Parameters for record length:
c-----------------------------------------------------------------------
c
c   MXLRC   I  maximum record length of links record
c
      integer*4 MXLRC
c
      parameter( MXLRC = 150 )
