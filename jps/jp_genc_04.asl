func main ()
   var a, b : int
   var A : array [2] of int
   var x , y : float

   x = 3;
   read b;
   a = b! / b;
   A[0] = 8;
   A[1] = 9;
   y = a! * x;
   while x < 100 do
      if ( -b! + x > (A[0]/2)!*a! ) then
         x = A[1]! - b;
         A[1] = (b+1)!!;
      else
         read x;
      endif
      write "loop ";
      write A[1]; write " ";
      write x; write "\n";
   endwhile
   write b-12; write "\n";
   a = (b-12)! * 5;   // Here execution must halt
   write "end ";
   write y; write "\n";
endfunc
