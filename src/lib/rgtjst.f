C**** RGTJST.F 
c 
      subroutine rgtjst( string )
c
c-----------------------------------------------------------------------
c
c     This routine right justifies a string, .ie. removes the trailing
c     blanks.
c
c   Arguments:
c
c     Inputs/Outputs: (serves as both input and output)
c       string   C   string to left justify
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c   strlen  I   returns the actual length of a string
c
      integer*4 strlen
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c       BLANK    C  BLANK character
c
      character*1 BLANK
      parameter(BLANK=' ')
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   ibeg  I   last non-blank character of string
c
      integer*4 iend, ilen, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- already left justified so just return ----
c
      ilen = LEN(string)
      if( string(ilen:ilen) .NE. BLANK ) goto 9999
c
c   ---- find last non-blank character ---
c
      iend = strlen ( string )
      if( iend .LT. 1 ) goto 9999
c
c   ---- move all characters to end ---
c
      do i=ilen,ilen-iend+1,-1
         string(i:i) = string(i-ilen+iend:i-ilen+iend)
      enddo
c
c   ---- blank out leftmost characters ---
c
      do i=1,ilen-iend
         string(i:i) = BLANK
      enddo
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
