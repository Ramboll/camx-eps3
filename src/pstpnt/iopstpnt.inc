c*** IOPSTPNT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for PSTPNT program
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
c                PSTPNT program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 PSTPNT module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORINP   I   unit number for user input file
c   IORMBR   I   unit number for input EMBR file
c   IOWEMS   I   unit number for output emissions file
c   IOWHDR   I   unit number for output stack header file
c   IOWSTK   I   unit number for output stack/scc file (binary)
c   IOWERR   I   unit number for EMAR error file
c   IOWMSG   I   unit number for message file
c
      integer*4 IORINP
      integer*4 IORMBR
      integer*4 IOWEMS
      integer*4 IOWHDR
      integer*4 IOWSTK      
      integer*4 IOWERR
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IORMBR = 11 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWEMS = 21 )
      parameter ( IOWHDR = 22 )
      parameter ( IOWSTK = 23 )
      parameter ( IOWERR = 30 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   errfl    C   EMAR error filename
c   imbrfl   C   input EMBR filename
c   msgfl    C   message filename
c   oemsfl   C   output emissions filename
c   ohdrfl   C   output stack header filename
c   stksfl   C   output STACKS/SCC filename
c   uinpfl   C   user input filename
c
c   lstksfl  L   .TRUE. if STACKS/SCC file is requested
c
      character*(MXSTR) errfl
      character*(MXSTR) imbrfl
      character*(MXSTR) msgfl
      character*(MXSTR) oemsfl
      character*(MXSTR) ohdrfl
      character*(MXSTR) stksfl
      character*(MXSTR) uinpfl
c
      logical*4         lstksfl
c
      common /iofil/ uinpfl, imbrfl, msgfl, oemsfl, ohdrfl, stksfl,
     &               errfl
      common /ioflg/ lstksfl
