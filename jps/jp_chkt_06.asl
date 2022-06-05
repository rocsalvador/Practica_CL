func main ()
   var a,b, i,j : int
   var t : bool
   var A : array [2] of int
   var B : array [2] of bool
   var M : matrix [10,20] of float

   A[0] = t;
   A[1] = 7;
   i = 0;
   while i<10 do
      j = 0;
      while j<20 do
         M[i,j] = A[i%2] + B[j%2];
         t = M[i-1,j+1] and f;
         j = j+1;
      endwhile
      i = i+1;
      a[i+j] = M[a,t] + (B[i-1] or not b);
   endwhile
   
   write M[5,8]; write "\n";
   write M[t,12]*A[8,1]; write "\n";
endfunc
