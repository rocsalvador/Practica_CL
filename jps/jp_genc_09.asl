func main ()
   var i,j: int
   var M,K : matrix [10,15] of float
   var t : float

   // fill
   i = 0;
   while i<10 do
      j = 0;
      while j<15 do
         read M[i,j];
         j = j+1;
      endwhile
      i = i+1;
   endwhile

   // copy
   K = M; 

   // traspose submatrix
   i = 3;
   while i<7 do
      j = 3;
      while j<i do
         t = K[i,j];
         K[i,j] = K[j,i];
         K[j,i] = t;
         j = j + 1;
      endwhile
      i = i+1;
   endwhile

   // print 
   i = 0;
   while i<10 do
      j = 0;
      while j<15 do
         write K[i,j];
         write " ";
         j = j + 1;
      endwhile
      write "\n";
      i = i+1;
   endwhile

endfunc
