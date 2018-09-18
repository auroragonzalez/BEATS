load('variable2')
%Case 1: all sensors are hard sensors delete the one under study

motes_for_BME=[1:4 6:14 16:17 19:54];
locations = dlmread('locations.txt');

time = zeros(length(ns),1);
MAPE = zeros(length(ns),1);
RMSE = zeros(length(ns),1);
CVRMSE = zeros(length(ns),1);

deltaD = 0.1:0.01:0.9;
%controlled missing data: at observation = mote id (so mote 1 has a
%missing value in 1st position, mote 2 has a missing value in 2nd
%position... and so on)
RMSE = zeros(length(deltaD),1);
hard = [1 7 13 16 20 26 40 47 50 54];
soft = motes_for_BME(~ismember(motes_for_BME, hard));

for ix = 1:length(deltaD)
ix
delta = deltaD(ix);
rng('default');

%%%SOFT
for m = motes_for_BME %%%SOFT
eval(['mote_' num2str(m) '_temprature_actual_prev = mote_' num2str(m) '_temprature_actual;' ]);%%%SOFT
end%%%SOFT

for m = soft %%%SOFT
rng(m); %%%SOFT
eval(['mote_' num2str(m) '_temprature_actual = mote_' num2str(m) '_temprature_actual -delta + (delta+delta)*rand(1200,1);' ]); %%%SOFT
end %%%SOFT


rand('state',0); 
randn('state',0); 

N=54; %observations
matrixForNA=eye(54);
%csvwrite('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/NApos.csv', positions.*matrixForNA);


%M = (matrixForNA)'
NAPerSensor = sum(matrixForNA,2)';
[row,col] = find(matrixForNA);
sr = sortrows([row,col],1);
NAPositionsWithinSensors = sr(:,2)';
%rem(sort(elements), N+1)

%NAPerSensor = randi(10, 1, length(motes_for_BME));
%NAPositionsWithinSensors = randi(N, 1, sum(NAPerSensor))  %consider 1000 values




ck = locations(ismember(locations(:,1),motes_for_BME),2:3);
thesensor = motes_for_BME;

cs = [];

motes_for_BME2=[1:4 6:14 16:17 19:54];
   
%temp = zeros(50,sum(NAPerSensor));

mysize = length(motes_for_BME2)-1
mloc = zeros(50,2,length(thesensor));

v = zeros(1, length(thesensor));
temp2 = zeros(mysize, length(thesensor));

ssaux=1;
for(sens = 1:length(motes_for_BME))
%    motes = motes_for_BME2(~ismember(motes_for_BME2,motes_for_BME(sens))); % get rid of the one we want to predict 
    for k = 1:NAPerSensor(sens)
            motesWithNAsamePosition = motes_for_BME2(sr(find(sr(:,2)==NAPositionsWithinSensors(ssaux)),1)); %get rid of the ones that have a NA in the place and also the one we want to predict
            motes = motes_for_BME2(~ismember(motes_for_BME2,motesWithNAsamePosition));
            motesSoft = motes(ismember(motes, soft)); % from motes which ones are soft
            motesHard = motes(ismember(motes, hard)); % from motes which ones are hard
           for m = 1:length(motesSoft)
                tempS(m,ssaux) =eval(['mote_' num2str(motesSoft(m)) '_temprature_actual(NAPositionsWithinSensors(ssaux))']);
            end
            for m = 1:length(motesHard)
                tempH(m,ssaux) = eval(['mote_' num2str(motesHard(m)) '_temprature_actual(NAPositionsWithinSensors(ssaux))']);
            end
            
            locH = locations(ismember(locations(:,1), motesHard),2:3);
            vH(ssaux) = length(locH(:,1));
            mlocH(1:length(locH),:,ssaux) = locH;
            
            locS = locations(ismember(locations(:,1), motesSoft),2:3);
            vS(ssaux) = length(locS(:,1));
            mlocS(1:length(locS),:,ssaux) = locS;
           
            ssaux = ssaux+1;
    end
end


%temp(:,1) all sensors minus the first temperature in the position of the first NA 

tempReal = zeros(51,1);

ssaux=1
for(sens = 1:length(motes_for_BME))
    for k = 1:NAPerSensor(sens)
        tempReal(ssaux) =eval(['mote_' num2str(motes_for_BME(sens)) '_temprature_actual(NAPositionsWithinSensors(ssaux))']);
        ssaux = ssaux+1;
    end
end



temperature = temp;
a_temperature = tempS-0.5;
b_temperature = tempS+0.5;


modelCsand={'nuggetC','exponentialC'};
paramsand={-0.0795,[0.6656,22.3507]};
%nhmax=4;
nsmax=3;
total=nhmax+nsmax;
dmax=100;
order=0;

real_temperature = tempReal;



nhmax = 3;
tic
predict_temprature_BME7 = BMEintervalMode7(ck,mlocH,vH,cs,mlocS,vS,tempH,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
time = toc
%time(n) = toc


MAPE_now = sum(100*abs((real_temperature-predict_temprature_BME7)./real_temperature))/length(real_temperature)
RMSE_now = sqrt(mean((real_temperature - predict_temprature_BME7).^2))
CVRMSE_now = RMSE_now/mean(real_temperature)*100

RMSE(ix) = RMSE_now
load('variable2');
    
end


[deltaD' RMSE]



csvwrite('C:\Users\AGONZALEZVID\Desktop\Work_melb\actual\BMECode\deltaChangesBME.csv', [deltaD' RMSE])
