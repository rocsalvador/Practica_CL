func main ()
   var i,j: int
   var M: matrix [10,15] of float
   var t : float

   // fill
   i = 0;
   while i<10 do
      j = 0;
      while j<15 do
         M[i,j] = i+j;
         j = j+1;
      endwhile
      i = i+1;
   endwhile

   // print
   i = 0;
   while i<15 do
      j = 0;
      while j<10 do
         write M[i,j];
         write " ";
         j = j + 1;
      endwhile
      write "\n";
      i = i+1;
   endwhile

endfunc
