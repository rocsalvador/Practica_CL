func main ()
   var nz,a,b, i,j : int
   var sum: float
   var A : array [2] of int
   var M : matrix [10,20] of float

   read a;
   read b;

   i = 0;
   while i<10 do
      j = 0;
      while j<20 do
         M[i,j] = a;
         a = a - b;
         j = j+1;
      endwhile
      i = i+1;
   endwhile

   nz = 0;
   sum = 0;
   i = 0;
   while i<10 do
      j = 0;
      while j<20 do
         if M[i,j] == 100 then
            nz = nz + 1;
         endif
         sum = sum + M[i,j];
         if (10*i+j)%12 == b then
            write sum; write "\n";
         endif
         j = j + 1;
      endwhile
      i = i+1;
   endwhile

   A[0] = nz;
   A[1] = 23;

   write "nz="; write nz; write "\n";
   write "sum="; write sum; write "\n";
   write "A: ["; write A[0]; write ",";
   write A[1]; write "]\n";
endfunc
