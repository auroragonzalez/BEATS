load('variable2')
%Case 2: adding soft sensors


Int5=0.5;
x2= (mote_2_data(start_data:end_data,1)-Int5)+1.*rand(1200,1)

motes_for_BME=[1:4 6:14 16:17 19:54];


hard = [1 7 13 16 20 26 40 47 50 54];
soft = motes_for_BME(~ismember(motes_for_BME, hard));

r = -0.5 + (0.5+0.5)*rand(10,1); % this is how the random numbers that I am going to add and substract are generated

%We save all the actual temperatures in another variable named ..._prev
for m = motes_for_BME
eval(['mote_' num2str(m) '_temprature_actual_prev = mote_' num2str(m) '_temprature_actual;' ]);
end

for m = soft
rng(m);
eval(['mote_' num2str(m) '_temprature_actual = mote_' num2str(m) '_temprature_actual -0.5 + (0.5+0.5)*rand(1200,1);' ]);
end
locations = dlmread('locations.txt');


N = 1200
n = 10 %FIXED 10 percent
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
%csvwrite('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/NApos.csv', positions.*matrixForNA);

NAPerSensor = sum(matrixForNA,2)';
[row,col] = find(matrixForNA);
sr = sortrows([row,col],1);
NAPositionsWithinSensors = sr(:,2)';


ck = repelem(locations(ismember(locations(:,1),motes_for_BME),2:3),NAPerSensor,1);
thesensor = repelem(motes_for_BME, NAPerSensor);




ssaux=1
for(sens = 1:length(soft))
    sensor = soft(sens);
%    motes = motes_for_BME2(~ismember(motes_for_BME2,motes_for_BME(sens))); % get rid of the one we want to predict 
    for k = 1:NAPerSoftSensor(sens)
            motesWithNAsamePosition = motes_for_BME2(sr(find(sr(:,2)==NAPositionsWithinSoftSensors(ssaux)),1)); %get rid of the ones that have a NA in the place and also the one we want to predict            
            deleteFromSoft = motesWithNAsamePosition(ismember(motesWithNAsamePosition,soft));
            motes = soft(~ismember(soft, deleteFromSoft)); %delete soft motes that are also NA in the same position and keep the soft ones that are not NA in 'motes'
            for m = 1:length(motes)
                tempS{ssaux}(m) =eval(['mote_' num2str(motes(m)) '_temprature_actual(NAPositionsWithinSensors(ssaux))']);
            end
            cs2{ssaux} = locations(ismember(locations(:,1), motes),2:3);
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





%%%%%%%%%%%%%%%%
%AGAIN
%%%%%%%%%%%%%%
temp = cell(1, length(thesensor));
ch3 = cell(1, length(thesensor));
tempS = cell(1, length(thesensor));
cs2 = cell(1, length(thesensor));

ssaux=1
for(sens = 1:length(motes_for_BME))
%    motes = motes_for_BME2(~ismember(motes_for_BME2,motes_for_BME(sens))); % get rid of the one we want to predict 
    for k = 1:NAPerSensor(sens)
            motesWithNAsamePosition = motes_for_BME2(sr(find(sr(:,2)==NAPositionsWithinSensors(ssaux)),1)); %get rid of the ones that have a NA in the place and also the one we want to predict
            motes = motes_for_BME2(~ismember(motes_for_BME2,motesWithNAsamePosition));
            motesSoft = motes(ismember(motes, soft)); % from motes which ones are soft
            motesHard = motes(ismember(motes, hard)); % from motes which ones are hard
           for m = 1:length(motesSoft)
               tempS{ssaux}(m) =eval(['mote_' num2str(motes(m)) '_temprature_actual(NAPositionsWithinSensors(ssaux))']);
            end
            for m = 1:length(motesHard)
               temp{ssaux}(m) =eval(['mote_' num2str(motes(m)) '_temprature_actual(NAPositionsWithinSensors(ssaux))']);
            end
            ch3{ssaux} = locations(ismember(locations(:,1), motesHard),2:3);
            cs2{ssaux} = locations(ismember(locations(:,1), motesSoft),2:3);
            ssaux = ssaux+1;
    end
end




temperature = temp;
a_temperature=[];
b_temperature=[];


modelCsand={'nuggetC','exponentialC'};
paramsand={-0.0795,[0.6656,22.3507]};
%nhmax=4;
nsmax=3;
total=nhmax+nsmax;
dmax=100;
order=0;

real_temperature = tempReal;


a=cellfun(@(x) x-0.5,tempS, 'UniformOutput', false);
b=cellfun(@(x) x+0.5,tempS, 'UniformOutput', false);

nhmax = 6
tic
predict_temprature_BME3 = BMEintervalMode3(ck,ch3,cs2,temp,a,b,modelCsand,paramsand,nhmax,nsmax,dmax,order);
time = toc

MAPE_now = sum(100*abs((real_temperature-predict_temprature_BME3)./real_temperature))/length(real_temperature)
RMSE_now = sqrt(mean((real_temperature - predict_temprature_BME3).^2));
CVRMSE_now = RMSE_now/mean(real_temperature)*100

MAPE = MAPE_now
RMSE = RMSE_now
CVRMSE = CVRMSE_now


csvwrite('fig13NA10soft.csv', [MAPE, RMSE, CVRMSE, time])