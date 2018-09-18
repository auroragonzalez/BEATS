function YY = weeklyProfile(min,maxx,weekDayStart)

L = length(maxx);

Y =  min*ones(size(maxx));

weekDayStart
delta = (8-weekDayStart)*24;

for i=1:floor(L/24/7)
    Y((i-1)*24*7+[8:19]+delta) = maxx((i-1)*24*7+[8:19]+delta);
    Y((i-1)*24*7+24+[8:19]+delta) = maxx((i-1)*24*7+24+[8:19]+delta);
    Y((i-1)*24*7+2*24+[8:19]+delta) = maxx((i-1)*24*7+2*24+[8:19]+delta);
    Y((i-1)*24*7+3*24+[8:19]+delta) = maxx((i-1)*24*7+3*24+[8:19]+delta);
    Y((i-1)*24*7+4*24+[8:19]+delta) = maxx((i-1)*24*7+4*24+[8:19]+delta);
end



if rem(L,i*24*7)>0
    Y((i-1)*24*7+[8:19]+delta) = maxx((i-1)*24*7+[8:19]+delta);
    Y((i-1)*24*7+24+[8:19]+delta) = maxx((i-1)*24*7+[8:19]+delta);
    Y((i-1)*24*7+2*24+[8:19]+delta) = maxx((i-1)*24*7+[8:19]+delta);
    Y((i-1)*24*7+3*24+[8:19]+delta) = maxx((i-1)*24*7+[8:19]+delta);
    Y((i-1)*24*7+4*24+[8:19]+delta) = maxx((i-1)*24*7+[8:19]+delta);
  
 end
  
  
%Remove redundant data
if length(Y)>L
    Y(24*days+1:end) = []; %aurorax: where is coming the days variable from???
end

YY(1) = Y(1);
YY(length(Y)) = Y(end);
%Smooth down
for i=2:length(Y)-1
  YY(i) = (Y(i-1)+Y(i+1))/2;
end


