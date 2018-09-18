load Data_TransProb
data(1:10,:)

P = transprob(data)

mc = dtmc(P)
X = simulate(mc,10);

mc = dtmc(P,'StateNames',["Regime 1" "Regime 2" "Regime 3" "Regime 4"]);
figure;
graphplot(mc,'ColorNodes',true,'ColorEdges',true)








sample = TsetClean(1:10000,1:100);
[nrow ncol] = size(sample);
t = datestr(timeSorted(1:nrow))

%t1 = datetime(2013,11,1,8,0,0);
%t2 = datetime(2013,11,5,12,0,0);
%t = t1:hours(1):t2
time = repmat(cellstr(t),1,ncol);

Rating = sample(:);
Rating(isnan(Rating))=0;
Rating2 = string(Rating);
Rating2(find(Rating2=='19')) = 'A';
Rating2(find(Rating2=='20')) = 'A';
Rating2(find(Rating2=='21')) = 'A';
Rating2(find(Rating2=='22')) = 'B';
Rating2(find(Rating2=='23')) = 'B';
Rating2(find(Rating2=='24')) = 'C';
Rating2(find(Rating2=='25')) = 'C';
Rating2(find(Rating2=='26')) = 'D';
Rating2(find(Rating2=='27')) = 'D';
Rating2(find(Rating2=='28')) = 'E';
Rating2(find(Rating2=='29')) = 'E';
Rating2(find(Rating2=='30')) = 'E';
Rating2(find(Rating2=='0')) = 'F';



ID = repelem(1:ncol,nrow);
ID2 = ID';
%time2 = string(time')
%data2 = table(ID2, time', string(Rating))
%{ID2, time'}
%num2str(ID2,'%02d')

ID3 = cellstr(num2str(ID2,'%02d'));
time3 = cellstr(time(:));
Ratings3 = cellstr(Rating2);

data3 = table(ID3, time3, Ratings3);

P = transprob(data3, 'labels', {'A','B','C', 'D', 'E', 'F'});



mc = dtmc(P)
X = simulate(mc,100);

figure;
graphplot(mc);



