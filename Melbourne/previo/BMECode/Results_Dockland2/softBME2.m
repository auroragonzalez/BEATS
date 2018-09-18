clear;
Nn = 1200
NofSensors = 4
load('DocklandDataforBMEExperiment.mat')
locations = mote_position

ns = 1;
MAPE = zeros(length(ns),1);
RMSE = zeros(length(ns),1);
CVRMSE = zeros(length(ns),1);
time = zeros(length(ns),1);


nodes = [506 509 510 511];

for m = 1:length(nodes)
eval(['node' num2str(m) '_prev= node_' num2str(nodes(m)) '_humidity_measured(1:Nn);' ]);
end

nodesR = [1 2 3 4]; % rename nodes

hard = [];
soft =[ 1 2 3 4];

for m = soft
eval(['node' num2str(m) '= node' num2str(m) '_prev -1 + (1+1)*rand(1,Nn);' ]);
end

for m = hard
eval(['node' num2str(m) '= node' num2str(m) '_prev;' ]);
end



for nx = 1:length(ns)
n = ns(nx)
matrixForNA=zeros(NofSensors,Nn);
[nrow, ncol] = size(matrixForNA);
nNA = nrow*ncol*n/100;
msize = numel(matrixForNA);
rng(n);
elements = randperm(msize, nNA);
matrixForNA(elements)=1;
[rId, cId] = find(matrixForNA);
 sortrows([rId, cId],1);

positions = zeros(NofSensors,Nn);
for row= 1:NofSensors
positions(row,:) = 1:Nn;
end
%csvwrite('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/NApos.csv', positions.*matrixForNA);


%RENAME the sensors

%M = (matrixForNA)'
NAPerSensor = sum(matrixForNA,2)';
[row,col] = find(matrixForNA);
sr = sortrows([row,col],1);
NAPositionsWithinSensors = sr(:,2)';
%rem(sort(elements), N+1)

%NAPerSensor = randi(10, 1, length(motes_for_BME));
%NAPositionsWithinSensors = randi(N, 1, sum(NAPerSensor))  %consider 1000 values




ck = repelem(locations,NAPerSensor,1);
thesensor = repelem(1:NofSensors, NAPerSensor);

cs = [];

%temp = zeros(50,sum(NAPerSensor));

mysize = NofSensors-1
mloc = zeros(NofSensors-1,2,length(thesensor));

v = zeros(1, length(thesensor));
temp2 = zeros(mysize, length(thesensor));


mlocH = zeros(3,2,length(thesensor));
mlocS = zeros(3,2,length(thesensor));

vH = zeros(1, length(thesensor));
vS = zeros(1, length(thesensor));

tempS = zeros(mysize, length(thesensor));
tempH = zeros(mysize, length(thesensor));


ssaux=1
for(sens = 1:NofSensors)
%    motes = motes_for_BME2(~ismember(motes_for_BME2,motes_for_BME(sens))); % get rid of the one we want to predict 
    for k = 1:NAPerSensor(sens)
            motesWithNAsamePosition = nodesR(sr(find(sr(:,2)==NAPositionsWithinSensors(ssaux)),1)); %get rid of the ones that have a NA in the place and also the one we want to predict
            motes = nodesR(~ismember(nodesR,motesWithNAsamePosition));
            motesSoft = motes(ismember(motes, soft)); % from motes which ones are soft
            motesHard = motes(ismember(motes, hard)); % from motes which ones are hard
           for m = 1:length(motesSoft)
                tempS(m,ssaux) =eval(['node' num2str(motesSoft(m)) '(NAPositionsWithinSensors(ssaux))']);
            end
            for m = 1:length(motesHard)
                tempH(m,ssaux) = eval(['node' num2str(motesHard(m)) '(NAPositionsWithinSensors(ssaux))']);
            end
            
            locH = locations(motesHard,:);  %%dock
            vH(ssaux) = size(locH,1);
            mlocH(1:size(locH,1),:,ssaux) = locH;
            
            locS = locations(motesSoft,:);  %%dock
            vS(ssaux) = size(locS,1);
            mlocS(1:size(locS,1),:,ssaux) = locS;
           
            ssaux = ssaux+1;
    end
end





%temp(:,1) all sensors minus the first temperature in the position of the first NA 

tempReal = zeros(sum(NAPerSensor),1);

ssaux=1
for(sens = 1:NofSensors)
    for k = 1:NAPerSensor(sens)
        tempReal(ssaux) =eval(['node' num2str(sens) '_prev(NAPositionsWithinSensors(ssaux))']);
        ssaux = ssaux+1;
    end
end



a_temperature=tempS-1;
b_temperature=tempS+5;


modelCsand={'nuggetC','exponentialC'};

paramsand={0.7265,[2.0467,22.3507]};


nhmax=3;
nsmax=3;
total=nhmax+nsmax;
dmax=1000;
order=0;

real_temperature = tempReal;


tic
predict_temprature_BME7 = BMEintervalMode7(ck,mlocH,vH,cs,mlocS,vS,tempH,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
%time = toc
time(n) = toc

MAPE_now = sum(100*abs((real_temperature-predict_temprature_BME7)./real_temperature))/length(real_temperature)
RMSE_now = sqrt(nanmean((real_temperature - predict_temprature_BME7).^2))
CVRMSE_now = RMSE_now/mean(real_temperature)*100 

MAPE(n) = MAPE_now;
RMSE(n) = RMSE_now
CVRMSE(n) = CVRMSE_now;

end


%csvwrite('C:\Users\AGONZALEZVID\Desktop\Work_melb\actual\BMECode\Results_Dockland2\softBME2now.csv', [MAPE, RMSE, CVRMSE, time])