%%%
%%% VARIOGRAMA PARA PARKING
%%%

nodes= dlmread('parking2.csv');  %au
locations = dlmread('expendedores2.csv'); %au
locations = locations(:,2:3); %au

%%% Omnidirectional variograms.
%%% Distances classes are the same than above.


foo = zeros(size(locations,1)-1,1)
for i = 2:size(locations,1)
    foo(i-1) = pdist(locations([1,i],:),'euclidean')
end

max(foo)
min(foo)
mean(foo)

cl=(0:0.0002:0.013)';
[dsandAu,vsandAu,osandAu]=vario(locations,nodes(:,1),cl,'kron'); % This is just for the first column
plot(dsandAu,vsandAu); 


modelsand={'nuggetV','exponentialV'};
paramsand0={[10],[10 2000]};
modelplot(dsandAu,modelsand,paramsand0);

paramsand=modelfit(dsandAu,vsandAu,osandAu,modelsand,paramsand0);
%modelplot(dsand,modelsand,paramsand,'Color',[1 0 0]);
paramsand{1}
paramsand{2}



