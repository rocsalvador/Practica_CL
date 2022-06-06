func main ()
   var a, b : int
   var A : array [2] of int
   var x , y : float

   x = 3;
   a = 1;
   read b;
   y = b! + x;
   write y; write "\n"; 
   A[0] = 8;
   A[1] = 9;
   y = a! * x;     
   if ( -b! + x <= (A[0]/2)!*a! ) then
      x = A[1]! - b;
      A[1] = (b-2)!!;
      write "A[1]="; write A[1];
      write "\n";
   endif
   write a; write " ";
   write b; write "\n";
   write "A: ";
   write A[0]; write " ";
   write A[1]; write "\n";
   write x; write " ";
   write y; write "\n";
endfunc
