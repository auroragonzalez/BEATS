% STATLIBtutorial           - tutorial for the statlib directory (Jan 1,2001)

%%% Clear memory content and set echo to on.

clear;
clc;
echo on;

%%% Load the data file called 'Falmagne.txt' and display 
%%% its content (see the Falmagne.txt file for the meaning
%%% of these variables).

[val,valname,filetitle]=readGeoEAS('Falmagne.txt');
ch=val(:,1:2);
sand=val(:,3);
silt=val(:,4);
clay=val(:,5);
code=val(:,6);
whos


nodes= dlmread('parking2.csv');  %au
locations = dlmread('expendedores2.csv'); %au
locations = locations(:,2:3); %au

%%% Type any key for continuing...

%%% Compute the omnidirectional variograms for the sand content.
%%% Distances classes are the same than above.


foo = zeros(size(ch,1)-1,1)
for i = 2:size(ch,1)
    foo(i-1) = pdist(ch([1,i],:),'euclidean')
end



cl=(0:500:10000)';
[dsand,vsand,osand]=vario(ch,sand,cl,'kron');
plot(dsand,vsand);

%%% Type any key for continuing...

pause;
clc;


foo = zeros(size(locations,1)-1,1)
for i = 2:size(locations,1)
    foo(i-1) = pdist(locations([1,i],:),'euclidean')
end

max(foo)
min(foo)
mean(foo)

%Un variograma representa la forma en que un punto tiene influencia sobre
%otro punto a diferentes distancias. Las distancias tienen que estar
%relacionadas con las distancias de los puntos que tenemos medidos, por
%ello este bucle nos ayudará a definir "cl"


%cl=(0:0.0001:0.01)'; 
cl=(0:0.0002:0.01)';
[dsandAu,vsandAu,osandAu]=vario(locations,nodes(:,1),cl,'kron'); %au
plot(dsandAu,vsandAu); %au


modelsand={'nuggetV','exponentialV'};
paramsand0={[10],[10 2000]};
modelplot(dsandAu,modelsand,paramsand0);

paramsand=modelfit(dsandAu,vsandAu,osandAu,modelsand,paramsand0);
modelplot(dsand,modelsand,paramsand,'Color',[1 0 0]);
paramsand{1}
paramsand{2}



%%% Plot a nested variogram models, composed of a nugget effect
%%% with a sill equal to 30, and an exponential model with a sill
%%% equal to 60 and a range equal to 3000.

modelsand={'nuggetV','exponentialV'};
paramsand0={[30],[60 100]};
modelplot(dsand,modelsand,paramsand0);

%%% Type any key for continuing...

pause;
clc;

%%% Fit by a weighted least squares method the estimated
%%% variogram using a nested model that includes a nugget
%%% effect and an exponential model, and display the results.
%%% The initial values for the parameters are as above.

paramsand=modelfit(dsand,vsand,osand,modelsand,paramsand0);
modelplot(dsand,modelsand,paramsand,'Color',[1 0 0]);
paramsand{1}
paramsand{2}

%%% Type any key for continuing...

pause;
clc;

%%% Compute the whole set of omnidirectional variograms and
%%% cross variograms for the raw values of the sand, silt
%%% and clay contents. Distance classes are the same than above.

options=1;
figure;
[d,V,o]=vario(ch,[sand silt clay],cl,'kron',options);

%%% Type any key for continuing...

pause;
clc;

%%% Fit by a weighted least squares method the whole set of
%%% variograms and cross variograms, using a nested model that
%%% includes a nugget effect and an exponential model with range
%%% equal to 3000. Display the estimated sill coefficients for
%%% the nugget effect and the exponential models

model={'nuggetV','exponentialV'};
param0={[30],[30 3000]};
options=1;
[param]=coregfit(d,V,o,model,param0,options);

param{1}{1}
param{2}{1}

%%% Type any key for continuing...

pause;
clc;

%%% Transform the sand, silt and clay content values toward
%%% zero mean unit variance distributed values and display the
%%% histograms and scatter plots for the transformed values.

sandT=other2gauss(sand,sandfileE,cdfsandfileE);
siltT=other2gauss(silt,siltfileE,cdfsiltfileE);
clayT=other2gauss(clay,clayfileE,cdfclayfileE);

figure;
histscatterplot([sandT siltT clayT]);

%%% Type any key for continuing...

pause;
clc;

%%% Compute the whole set of omnidirectional variograms and
%%% cross variograms for the Gaussian transformed sand, silt
%%% and clay contents. Distance classes are the same than above.

options=1;
figure;
[dT,VT,oT]=vario(ch,[sandT siltT clayT],cl,'kron',options);

%%% Type any key for continuing...

pause;
clc;

%%% Fit by a weighted least squares method the whole set of
%%% variograms and cross variograms, using a nested model that
%%% includes a nugget effect and an exponential model with range
%%% equal to 3000. Display the estimated sill coefficients for
%%% the nugget effect and the exponential models

modelT={'nuggetV','exponentialV'};
paramT0={[0.5],[0.5 3000]};
options=1;
[paramT]=coregfit(dT,VT,oT,modelT,paramT0,options);

paramT{1}{1}
paramT{2}{1}

%%% Type any key for continuing...

pause;
clc;

%%% Save all the variables to the 'STATLIBtutorial.mat' binary data
%%% file for subsequent use

save STATLIBtutorial

%%% End of the STATLIB tutorial

echo off;




