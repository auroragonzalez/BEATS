load('variable2')
%Case 1: all sensors are hard sensors delete the one under study

motes_for_BME=[1:4 6:14 16:17 19:54];
locations = dlmread('locations.txt');
rng(123);

nh = 3:51

time = zeros(length(nh),1);
MAPE = zeros(length(nh),1);
RMSE = zeros(length(nh),1);
CVRMSE = zeros(length(nh),1);

N=1200;
matrixForNA=zeros(51,N);
[nrow, ncol] = size(matrixForNA);
nNA = nrow*ncol*10/100;  % 10 percent has to be NA
msize = numel(matrixForNA);
elements = randperm(msize, nNA);
matrixForNA(elements)=1;
[rId, cId] = find(matrixForNA);
sortrows([rId, cId],1);

positions = zeros(51,N);
for row= 1:51
positions(row,:) = 1:N;
end
%csvwrite('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/NApos.csv', positions.*matrixForNA);


%M = (matrixForNA)'
NAPerSensor = sum(matrixForNA,2)';
[row,col] = find(matrixForNA);
sr = sortrows([row,col],1);
NAPositionsWithinSensors = sr(:,2)';
%rem(sort(elements), N+1)

%NAPerSensor = randi(10, 1, length(motes_for_BME));
%NAPositionsWithinSensors = randi(N, 1, sum(NAPerSensor))  %consider 1000 values




ck = repelem(locations(ismember(locations(:,1),motes_for_BME),2:3),NAPerSensor,1);
thesensor = repelem(motes_for_BME, NAPerSensor);
%A = zeros(length(motes_for_BME)-1,2,length(thesensor))
ch = zeros(50,2,length(thesensor));

for i = 1:length(thesensor)
   ch(:,:,i) = eval(['variable2.locMinusSensor' num2str(thesensor(i))]);
end


cs = [];

motes_for_BME2=[1:4 6:14 16:17 19:54];
   
temp = zeros(50,sum(NAPerSensor));

ssaux=1
for(sens = 1:length(motes_for_BME))
    motes = motes_for_BME2(~ismember(motes_for_BME2,motes_for_BME(sens)));
    for k = 1:NAPerSensor(sens)
            for m = 1:length(motes)
                temp(m,ssaux) =eval(['mote_' num2str(motes(m)) '_temprature_actual(NAPositionsWithinSensors(ssaux))']);
            end
             ssaux = ssaux+1;
    end
end

%temp(:,1) all sensors minus the first temperature in the position of the first NA 

tempReal = zeros(sum(NAPerSensor),1);

ssaux=1
for(sens = 1:length(motes_for_BME))
    for k = 1:NAPerSensor(sens)
        tempReal(ssaux) =eval(['mote_' num2str(motes_for_BME(sens)) '_temprature_actual(NAPositionsWithinSensors(ssaux))']);
        ssaux = ssaux+1;
    end
end



temperature = temp;
a_temperature=[];
b_temperature=[];


modelCsand={'nuggetC','exponentialC'};
paramsand={-0.0795,[0.6656,22.3507]};
%nhmax=3;
nsmax=3;
total=nhmax+nsmax;
dmax=100;
order=0;

for nhmax = nh
tic
predict_temprature_BME = BMEintervalMode2(ck,ch,cs,temperature,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
time(nhmax-2) = toc
real_temperature = tempReal;

MAPE_now = sum(100*abs((real_temperature-predict_temprature_BME)./real_temperature))/length(real_temperature)
RMSE_now = sqrt(mean((real_temperature - predict_temprature_BME).^2));
CVRMSE_now = RMSE_now/mean(real_temperature)*100

MAPE(nhmax-2) = MAPE_now
RMSE(nhmax-2) = RMSE_now
CVRMSE(nhmax-2) = CVRMSE_now

end


csvwrite('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/Results/fig2.csv', [nh',MAPE, RMSE, CVRMSE, time])