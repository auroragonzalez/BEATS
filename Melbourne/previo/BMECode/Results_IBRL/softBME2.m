load('variable2')
%Case 1: all sensors are hard sensors delete the one under study

motes_for_BME=[1:4 6:14 16:17 19:54];
locations = dlmread('locations.txt');

%n=10 %number of NAs = 10 % 

time = zeros(length(ns),1);
MAPE = zeros(length(ns),1);
RMSE = zeros(length(ns),1);
CVRMSE = zeros(length(ns),1);


N = 1200
ns = 1:85

%%%%%%%% soft added%%%%%

hard = [1 7 13 16 20 26 40 47 50 54];
soft = motes_for_BME(~ismember(motes_for_BME, hard));

%We save all the actual temperatures in another variable named ..._prev
for m = motes_for_BME
eval(['mote_' num2str(m) '_temprature_actual_prev = mote_' num2str(m) '_temprature_actual;' ]);
end

for m = soft
rng(m);
eval(['mote_' num2str(m) '_temprature_actual = mote_' num2str(m) '_temprature_actual -0.5 + (0.5+0.5)*rand(1200,1);' ]);
end




for n = ns
    
matrixForNA=zeros(51,N);
[nrow, ncol] = size(matrixForNA);
nNA = nrow*ncol*n/100;
msize = numel(matrixForNA);
rng(n);
elements = randperm(msize, nNA);
matrixForNA(elements)=1;
[rId, cId] = find(matrixForNA);
 sortrows([rId, cId],1);
 
positions = zeros(51,N);
for row= 1:51
positions(row,:) = 1:N;
end




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

NAPerSensor = sum(matrixForNA,2)';
[row,col] = find(matrixForNA);
sr = sortrows([row,col],1);
NAPositionsWithinSensors = sr(:,2)';

ck = repelem(locations(ismember(locations(:,1),motes_for_BME),2:3),NAPerSensor,1);
thesensor = repelem(motes_for_BME, NAPerSensor);

cs = [];

motes_for_BME2=[1:4 6:14 16:17 19:54];
   
%temp = zeros(50,sum(NAPerSensor));

mysize = length(motes_for_BME2)-1
mlocH = zeros(50,2,length(thesensor));
mlocS = zeros(50,2,length(thesensor));

vH = zeros(1, length(thesensor));
vS = zeros(1, length(thesensor));

tempS = zeros(mysize, length(thesensor));
tempH = zeros(mysize, length(thesensor));


ssaux=1
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
            vH(ssaux) = size(locH,1);
            mlocH(1:size(locH,1),:,ssaux) = locH;
            
            locS = locations(ismember(locations(:,1), motesSoft),2:3);
            vS(ssaux) = size(locS,1);
            mlocS(1:size(locS,1),:,ssaux) = locS;
           
            ssaux = ssaux+1;
    end
end


%temp(:,1) all sensors minus the first temperature in the position of the first NA 

tempReal = zeros(sum(NAPerSensor),1);

ssaux=1
for(sens = 1:length(motes_for_BME))
    for k = 1:NAPerSensor(sens)
        tempReal(ssaux) =eval(['mote_' num2str(motes_for_BME(sens)) '_temprature_actual_prev(NAPositionsWithinSensors(ssaux))']);
        ssaux = ssaux+1;
    end
end



temperature = temp;


modelCsand={'nuggetC','exponentialC'};
paramsand={-0.0795,[0.6656,22.3507]};
nhmax=3;
nsmax=2;
total=nhmax+nsmax;
dmax=100;
order=0;

real_temperature = tempReal;

a_temperature = tempS-0.5;
b_temperature = tempS+0.5;



tic
predict_temprature_BME7 = BMEintervalMode7(ck,mlocH,vH,cs,mlocS,vS,tempH,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
%time = toc
time(n) = toc


MAPE_now = sum(100*abs((real_temperature-predict_temprature_BME7)./real_temperature))/length(real_temperature)
RMSE_now = sqrt(mean((real_temperature - predict_temprature_BME7).^2));
CVRMSE_now = RMSE_now/mean(real_temperature)*100

MAPE(n) = MAPE_now

RMSE(n) = RMSE_now
CVRMSE(n) = CVRMSE_now


    

end






csvwrite('C:\Users\AGONZALEZVID\Desktop\Work_melb\actual\BMECode\Results\softBME2.csv', [ns',MAPE', RMSE', CVRMSE', time'])