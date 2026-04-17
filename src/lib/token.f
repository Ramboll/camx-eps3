**** TOKEN.F 
c 
      subroutine token( ierr, word, string )
c
c-----------------------------------------------------------------------
c
c     This routine parses the string and pulls of the first token 
c     (unix speak for word) and stores in the argument variable.
c     The first word is then chopped off of the string, which is
c     ready for the next word.
c
c   Arguments:
c
c     Inputs/Outputs:
c       string   C   string holding words (gets modified in process)
c     Outputs:
c       ierr     I   error code
c       word     C   string to hold first token
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
      character*10  word
      integer*4     ierr
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c       BLANK    C  BLANK character
c
      character*1 BLANK
      parameter( BLANK=' ' )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize the output to blank ---
c
      ierr = ISUCES
      word = ' '
c
c   ---- if the string is blank, just set error code and return ---
c
      if( string .EQ. ' ' ) then
         ierr = IEOD
         goto 9999
      endif
c
c   ---- check from end of string to beginning for non-blank ----
c
      do i=1,LEN(string)
         if( string(i:i) .EQ. BLANK ) then
             word = string(:i-1)
c
c   --- chop off the first word ---
c
             string = string(i:)
             call lftjst( string )
             goto 9999
         endif
      enddo
c
c   --- I guess no words found, should not get here ---
c
      ierr = IEOD
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
