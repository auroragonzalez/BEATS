load('variable2')
%Case 1: all sensors are hard sensors delete the one under study

motes_for_BME=[1:4 6:14 16:17 19:54];
locations = dlmread('locations.txt');
rng(123);

ns = 120:120:1200;
nh = 3:51;

time = zeros(length(ns),length(nh));
MAPE = zeros(length(ns),length(nh));
%RMSE = zeros(length(ns),1);
%CVRMSE = zeros(length(ns),1);

for n = 1:length(ns)    
N=ns(n);
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
%nhmax=8;
nsmax=3;
total=nhmax+nsmax;
dmax=100;
order=0;

for nhmax = nh
tic
predict_temprature_BME = BMEintervalMode2(ck,ch,cs,temperature,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
time(n,nhmax-2) = toc
real_temperature = tempReal;

MAPE_now = sum(100*abs((real_temperature-predict_temprature_BME)./real_temperature))/length(real_temperature)
%RMSE_now = sqrt(mean((real_temperature - predict_temprature_BME).^2));
%CVRMSE_now = RMSE_now/mean(real_temperature)*100

MAPE(n, nhmax-2) = MAPE_now
%RMSE(n) = RMSE_now
%CVRMSE(n) = CVRMSE_now

end

end


%csvwrite('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/Results/fig4MAPE.csv', [MAPE])
%csvwrite('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/Results/fig4time.csv', [time])
