function Y = downsampling (X,l)
k=0;
for i=1:length(X)
    if rem(i,l)==0
        k=k+1;
        Y(k) = X(i);
   end
end
       