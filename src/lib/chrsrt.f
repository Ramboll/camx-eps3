C**** CHRSRT.F 
c
      subroutine chrsrt(array,ilen,ncount,itglst)

c-----------------------------------------------------------------------
c
c   Sorts an array of character strings lexicographically.  The sorted
c   order of elements of the array is stored in the array itglst.
c   That is, the value of the ith element of the hypothetical,
c   sorted version of array is array(itaglst(i))
c
c    Argument description:
c     Inputs:
c         array    C    array of character strings to sort
c         ilen     I    declared length of character string
c         ncount   I    number of elements in array
c     Outputs:
c         itglst   I    array of indexes of original array elements in
c                       sorted order
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
c   Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4     ncount
      character*(*) array(ncount)
      integer*4     ilen
      integer*4     itglst(ncount)
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
c   string   C   string for temporary storage
c
      character*(MXSTR) string
      integer           i, j
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- start with 1 element presumed sorted
c                  (ncount_th data element) ---
c
      itglst(ncount) = ncount
c
c   --- then insertion sort the rest, changing the indexes only ---
c
      do i=ncount-1,1,-1
          if (array(i)(1:ilen) .LE. array(itglst(i+1))(1:ilen) ) then
              itglst(i) = i
          else
              string(1:ilen) = array(i)(1:ilen)
              j = i + 1
  111         continue
              itglst(j-1) = itglst(j)
              j = j + 1
              if (j .LE. ncount .AND.
     &            array(itglst(j))(1:ilen) .LT. string(1:ilen)) goto 111
              itglst(j-1) = i
          endif
      enddo
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
      return
      end
