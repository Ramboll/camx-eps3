c*** CHMSPL.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for CHMSPL program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c   user options:
c-----------------------------------------------------------------------
c
c   lovrid   C   flag to determine if execution should continue of
c                species lists are incompatible
c   lprtpf   C   flag to determine if tables by profile should be printed
c   lreqpf   C   flag to determine if all profiles must match an entry
c                in the splits file of if a default profile can be used
c
      logical*4 lovrid
      logical*4 lprtpf
      logical*4 lreqpf
c
      common /datchm/ lovrid, lprtpf, lreqpf
