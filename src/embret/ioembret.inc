c*** IOEMBRET.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for EMBRET program
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
c                EMBRET program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 EMBRET utility v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORINP   I   unit number for user input file
c   IORMBR   I   unit number for input EMBR file
c   IOWEMR   I   unit number for output EMAR file
c   IOWMSG   I   unit number for message file
c
      integer*4 IORINP
      integer*4 IORMBR
      integer*4 IOWEMR
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IORMBR = 11 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWEMR = 21 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   emarfl   C   EMAR error filename
c   embrfl   C   input EMBR filename
c   msgfl    C   message output file
c   uinpfl   C   user input filename
c
      character*(MXSTR) emarfl
      character*(MXSTR) embrfl
      character*(MXSTR) msgfl
      character*(MXSTR) uinpfl
c
      common /iofil/ uinpfl, embrfl, msgfl, emarfl
c
c-----------------------------------------------------------------------
c   Other variables:
c-----------------------------------------------------------------------
c
c   lusrin   L    true if using a user input file
c
      logical lusrin
c
      common /retdat/ lusrin
