
*> code taken from https://craftofcoding.wordpress.com/2021/03/23/coding-cobol-a-bubblesort/
identification division.
program-id. stats.

environment division.
input-output section.
file-control.
select input-file assign to dynamic fname-inp
       organization is line sequential.

data division.
file section.
fd input-file.
01 sample-input     pic x(80).

working-storage section.
77 n          pic 9999 value 0.
77 feof       pic A(1).
77 temp       pic s9(14)v9(4) usage is computational-3.
77 fname-inp  pic x(30).
77 i          pic 9999.
77 j          pic 9999.
77 jp1        pic 9999.
77 jp2        pic 9999.

01 array-area.
   02 x pic s9(14)v9(4) usage is computational-3
      occurs 1000 times.

01 input-value.
   02 in-x   pic s9(14)v9(4).
   02 filler pic x(62).
   
procedure division.
   display "Input filename? "
   accept fname-inp.
   open input input-file.

   perform input-loop until feof='Y'
   perform bubblesort.
   perform print-nums.
   perform finish.

input-loop.
   read input-file into input-value
      at end move 'Y' to feof
      not at end
         add 1 to n
         move in-x to x(n)
   end-read.

bubblesort.
   perform varying i from 1 by 1 until i is greater than n
      compute jp1 = n - i
      perform varying j from 1 by 1 until j is greater than jp1
         compute jp2 = j + 1
         if (x(j) > x(jp2))
            move x(j) to temp
            move x(jp2) to x(j)
            move temp to x(jp2)
         end-if
      end-perform
   end-perform.
   
print-nums.
   move 1 to i.
   perform until i > n
      display i "->"x(i)
      add 1 to i
   end-perform.

finish.
   close input-file.
   stop run.