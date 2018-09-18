clear;

nodes= dlmread('beach.csv');

nodes = nodes(:,1:300)
NofSensors=6

for m = 1:NofSensors
eval(['node' num2str(m) '= nodes(' num2str(m) ',:)' ]);
end


max_epoch=100
rank=20
lr=0.001
mom=0.8
reg=0.25
norm =0 ;
Nn=300;
N=300



ns = 1:20;
MAPE = zeros(length(ns),1);
RMSE = zeros(length(ns),1);
CVRMSE = zeros(length(ns),1);
time = zeros(length(ns),1);

for nx = 1:length(ns)
n = ns(nx)
matrixForNA=zeros(NofSensors,N);
[nrow, ncol] = size(matrixForNA);
nNA = nrow*ncol*n/100;
msize = numel(matrixForNA);
rng(n);
elements = randperm(msize, nNA);
matrixForNA(elements)=1;
[rId, cId] = find(matrixForNA);
 sortrows([rId, cId],1);

positions = zeros(NofSensors,N);
for row= 1:NofSensors
positions(row,:) = 1:N;
end
%csvwrite('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/NApos.csv', positions.*matrixForNA);




%RENAME the sensors
%nodes = [506 509 510 511];

%for m = 1:length(nodes)
%eval(['node' num2str(m) '= node_' num2str(nodes(m)) '_humidity_measured(1:N);' ]);
%end

nodesR = [1 2 3 4 5 6]; % rename nodes

%M = (matrixForNA)'
NAPerSensor = sum(matrixForNA,2)';
[row,col] = find(matrixForNA);
sr = sortrows([row,col],1);
NAPositionsWithinSensors = sr(:,2)';
%rem(sort(elements), N+1)

%NAPerSensor = randi(10, 1, length(motes_for_BME));
%NAPositionsWithinSensors = randi(N, 1, sum(NAPerSensor))  %consider 1000 values

locations = [41.783094 -87.574542; 41.715017, -87.528433; 41.965684, -87.636338; 41.893466, -87.613126; 41.986387, -87.652020; 41.756430, -87.543957]


ck = repelem(locations,NAPerSensor,1);
thesensor = repelem(1:NofSensors, NAPerSensor);

cs = [];

%temp = zeros(50,sum(NAPerSensor));

mysize = NofSensors-1
mloc = zeros(NofSensors-1,2,length(thesensor));

v = zeros(1, length(thesensor));
temp2 = zeros(mysize, length(thesensor));

ssaux=1
for(sens = 1:NofSensors)
    for k = 1:NAPerSensor(sens)
            motesWithNAsamePosition = nodesR(sr(find(sr(:,2)==NAPositionsWithinSensors(ssaux)),1)); %get rid of the ones that have a NA in the place and also the one we want to predict
            motes = nodesR(~ismember(nodesR,motesWithNAsamePosition));
            %motes = motes_for_BME2(~ismember(motes_for_BME2,motes_for_BME(sens)));
            loc = locations(motes,:);
            v(ssaux) = length(loc(:,1));
            mloc(1:size(loc,1),:,ssaux) = loc;
            for m = 1:length(motes)
                temp2(m,ssaux) =eval(['node' num2str(motes(m)) '(NAPositionsWithinSensors(ssaux))']);
            end
%            ch3{ssaux} = locations(ismember(locations(:,1), motes),2:3);

            ssaux = ssaux+1;
    end
end


%temp(:,1) all sensors minus the first temperature in the position of the first NA 

tempReal = zeros(sum(NAPerSensor),1);

ssaux=1
for(sens = 1:NofSensors)
    for k = 1:NAPerSensor(sens)
        tempReal(ssaux) =eval(['node' num2str(sens) '(NAPositionsWithinSensors(ssaux))']);
        ssaux = ssaux+1;
    end
end



a_temperature=[];
b_temperature=[];


modelCsand={'exponentialC'};
paramsand={[1,1]};
nhmax=6;
nsmax=6;
total=nhmax+nsmax;
dmax=10000;
order=0;

real_temperature = tempReal;

    
tic
predict_temprature_BME6 = BMEintervalMode6(ck,mloc,v,cs,temp2,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
%time = toc
time(n) = toc


MAPE_now = sum(100*abs((real_temperature-predict_temprature_BME6)./real_temperature))/length(real_temperature)
RMSE_now = sqrt(nanmean((real_temperature - predict_temprature_BME6).^2))
CVRMSE_now = RMSE_now/mean(real_temperature)*100 

MAPE(n) = MAPE_now
RMSE(n) = RMSE_now
CVRMSE(n) = CVRMSE_now

end

csvwrite('/home/aurorax/Git_repos/posgradoActual/Melbourne/RoI/results/softDockDatasetHardBME.csv', [RMSE time])

%csvwrite('C:\Users\AGONZALEZVID\Desktop\Work_melb\actual\BMECode\Results_Dockland23\hardBME2.csv', [MAPE, RMSE, CVRMSE, time])