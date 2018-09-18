clear;

nodes= dlmread('parking2.csv');
cluclu= dlmread('corClus.txt');

NofSensors=size(nodes,1);

for m = 1:NofSensors
eval(['node' num2str(m) '= nodes(' num2str(m) ',:)' ]);
end


max_epoch=100
rank=20
lr=0.001
mom=0.8
reg=0.25
norm =0 ;
Nn=1200;


R = nodes;


%%%%%%%%%%%%%%%%%%%%
%%%% EXPERIMENTS %%%
%%%%%%%%%%%%%%%%%%%%


clu = 10
CLU= dlmread(['BMECode/Previous_res/kmeansParkResR/k' num2str(clu) '/groupsk' num2str(clu) '.txt']);

RMSEhard = zeros(length(clu),1)
timehard = zeros(length(clu),1)
k=10 % 10 % of NA


ns=10
[N,M] = size(R);
matrixForNA=ones(N,M);
nNA = N*M*ns/100;
msize = numel(matrixForNA);
rng(ns)
elements = randperm(msize, nNA);
[rId, cId] = find(matrixForNA);
sortrows([rId, cId],1);
matrixForNA(elements) = 0;
RoI = R.*matrixForNA;
New = zeros(N,M);

tic
for index = 1:clu
    R2oI = RoI(CLU(find(CLU(:,2)==index),1)',:);
    R3 = pmf4(R2oI, 10, rank, lr, mom,reg,1);
    New(CLU(find(CLU(:,2)==index),1)',:) = R3;
end
time = toc
realTest = R(elements);
predTest = New(elements);
RMSEhard = sqrt(mean((realTest - predTest).^2))





%csvwrite('/home/aurorax/Git_repos/posgradoActual/Melbourne/RoI/testBeachPMF.csv', [RMSEsoft timesoft RMSEhard timehard])

matrixForNA(matrixForNA==1)=2;
matrixForNA(matrixForNA==0)=3;
matrixForNA(matrixForNA==2)=0;
matrixForNA(matrixForNA==3)=1;

NAPerSensor = sum(matrixForNA,2)';
[row,col] = find(matrixForNA);
sr = sortrows([row,col],1);
NAPositionsWithinSensors = sr(:,2)';


locations = dlmread('expendedores2.csv');
locations = locations(:,2:3);

ck = repelem(locations,NAPerSensor,1);
thesensor = repelem(1:NofSensors, NAPerSensor);

cs = [];

%temp = zeros(50,sum(NAPerSensor));

mysize = NofSensors-1
mloc = zeros(NofSensors-1,2,length(thesensor));

v = zeros(1, length(thesensor));
temp2 = zeros(mysize, length(thesensor));

ssaux=1
nodesR=1:NofSensors
for(sens = 1:NofSensors)
    for k = 1:NAPerSensor(sens)
            motesWithNAsamePosition = sr(find(sr(:,2)==NAPositionsWithinSensors(ssaux)),1); %get rid of the ones that have a NA in the place and also the one we want to predict
            motes = nodesR(~ismember(nodesR,motesWithNAsamePosition));
            %motes = motes_for_BME2(~ismember(motes_for_BME2,motes_for_BME(sens)));
            loc = locations(motes,:);
            v(ssaux) = length(loc(:,1));
            mloc(1:size(loc,1),:,ssaux) = loc;
            for m = 1:length(motes)
            temp2(m,ssaux) =R(motes(m),NAPositionsWithinSensors(ssaux));
            end
%            ch3{ssaux} = locations(ismember(locations(:,1), motes),2:3);

            ssaux = ssaux+1;
    end
end


%temp(:,1) all sensors minus the first temperature in the position of the first NA 
rank
tempReal = zeros(sum(NAPerSensor),1);


ssaux=1
for(sens = 1:NofSensors)
    for k = 1:NAPerSensor(sens)
        tempReal(ssaux) =R(sens, NAPositionsWithinSensors(ssaux));
        ssaux = ssaux+1;
    end
end



a_temperature=[];
b_temperature=[];


modelCsand={'exponentialC'};
paramsand={[100,100]};
nhmax=20;
nsmax=3;
total=nhmax+nsmax;
dmax=10000;
order=0;

real_temperature = tempReal';

    
tic
predict_temprature_BME6 = BMEintervalMode6(ck,mloc,v,cs,temp2,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
time = toc;
%time(n) = toc


MAPE_now = sum(100*abs((real_temperature-predict_temprature_BME6)./real_temperature))/length(real_temperature)
RMSE_now = sqrt(nanmean((real_temperature' - predict_temprature_BME6).^2))




%%%%%%%%
%%%%%%%%%
%%%%%%%%

matrixForNA(matrixForNA==1)=2;
matrixForNA(matrixForNA==0)=3;
matrixForNA(matrixForNA==2)=0;
matrixForNA(matrixForNA==3)=1;


matrixForNA = matrixForNA(cluclu==1,:)
NofSensors = sum(cluclu==1);
NAPerSensor = sum(matrixForNA,2)';
[row,col] = find(matrixForNA);
sr = sortrows([row,col],1);
NAPositionsWithinSensors = sr(:,2)';


locations = dlmread('expendedores2.csv');
locations = locations(cluclu==1,2:3);

ck = repelem(locations,NAPerSensor,1);
thesensor = repelem(1:NofSensors, NAPerSensor);

cs = [];

%temp = zeros(50,sum(NAPerSensor));

mysize = NofSensors-1
mloc = zeros(NofSensors-1,2,length(thesensor));

v = zeros(1, length(thesensor));
temp2 = zeros(mysize, length(thesensor));

ssaux=1
nodesR=1:NofSensors
for(sens = 1:NofSensors)
    for k = 1:NAPerSensor(sens)
            motesWithNAsamePosition = sr(find(sr(:,2)==NAPositionsWithinSensors(ssaux)),1); %get rid of the ones that have a NA in the place and also the one we want to predict
            motes = nodesR(~ismember(nodesR,motesWithNAsamePosition));
            %motes = motes_for_BME2(~ismember(motes_for_BME2,motes_for_BME(sens)));
            loc = locations(motes,:);
            v(ssaux) = length(loc(:,1));
            mloc(1:size(loc,1),:,ssaux) = loc;
            for m = 1:length(motes)
            temp2(m,ssaux) =R(motes(m),NAPositionsWithinSensors(ssaux));
            end
%            ch3{ssaux} = locations(ismember(locations(:,1), motes),2:3);

            ssaux = ssaux+1;
    end
end


%temp(:,1) all sensors minus the first temperature in the position of the first NA 
rank
tempReal = zeros(sum(NAPerSensor),1);


ssaux=1
for(sens = 1:NofSensors)
    for k = 1:NAPerSensor(sens)
        tempReal(ssaux) =R(sens, NAPositionsWithinSensors(ssaux));
        ssaux = ssaux+1;
    end
end



a_temperature=[];
b_temperature=[];


modelCsand={'nuggetC','exponentialC'};
paramsand={-0.0795,[0.1,22.3507]};
nhmax=20;
nsmax=3;
total=nhmax+nsmax;
dmax=10000;
order=0;

real_temperature = tempReal';

    
tic
predict_temprature_BME6 = BMEintervalMode6(ck,mloc,v,cs,temp2,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
time = toc;
%time(n) = toc

RMSE_now = sqrt(nanmean((real_temperature' - predict_temprature_BME6).^2))



