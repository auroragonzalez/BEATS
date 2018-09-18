load('variable2')
load('variable3')
%Case 1: all sensors are hard sensors delete the one under study

motes_for_BME=[1:4 6:14 16:17 19:54];
locations = dlmread('locations.txt');
%rng(123);

ns = 1:10
time = zeros(length(ns),1);
MAPE = zeros(length(ns),1);
RMSE = zeros(length(ns),1);
CVRMSE = zeros(length(ns),1);
 
N=1200;
matrixForNA=zeros(51,N);
[nrow, ncol] = size(matrixForNA);

for n = ns
rng(n);
nNA = nrow*ncol*n/100;  % 10 percent has to be NA
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
chN = zeros(50,2,length(thesensor));

for i = 1:length(thesensor)
   ch(:,:,i) = eval(['variable3.locMinusSensor' num2str(thesensor(i))]);
end


cs = [];

motes_for_BME2=[1:4 6:14 16:17 19:54];
   



sensNormalised = zeros(54, 1200);
for (sens = motes_for_BME )
   a=eval(['mote_' num2str(sens) '_temprature_actual']);
   x = [min(a,[],1);max(a,[],1)];
   b = bsxfun(@minus,a,x(1,:));
   b = bsxfun(@rdivide,b,diff(x,1,1));     
   sensNormalised(sens,:) = b;
end


temp = zeros(50,sum(NAPerSensor));

ssaux=1;
for(sens = 1:length(motes_for_BME))
    motes = motes_for_BME2(~ismember(motes_for_BME2,motes_for_BME(sens)));
    for k = 1:NAPerSensor(sens)
            for m = 1:length(motes)
                %temp(m,ssaux) =sensNormalised(motes(m), NAPositionsWithinSensors(ssaux));
                temp(m,ssaux) = eval(['mote_' num2str(motes(m)) '_temprature_actual(NAPositionsWithinSensors(ssaux))']);
            end
             chN(:,:,ssaux) = locations(ismember(locations(:,1), motes),2:3);
             ssaux = ssaux+1;
    end
end




%temp(:,1) all sensors minus the first temperature in the position of the first NA 

tempReal = zeros(sum(NAPerSensor),1);

ssaux=1;
for(sens = 1:length(motes_for_BME))
    for k = 1:NAPerSensor(sens)
        %tempReal(ssaux) =sensNormalised(motes_for_BME(sens), NAPositionsWithinSensors(ssaux));
        tempReal(ssaux) =eval(['mote_' num2str(motes_for_BME(sens)) '_temprature_actual(NAPositionsWithinSensors(ssaux))']);
        ssaux = ssaux+1;
    end
end





temperature = temp;
a_temperature=[];
b_temperature=[];


modelCsand={'nuggetC','exponentialC'};
paramsand={-0.0795,[0.6656,22.3507]};
nhmax=6;
nsmax=3;
total=nhmax+nsmax;
dmax=100;
order=0;

tic
predict_temprature_BME2 = BMEintervalMode2(ck,ch,cs,temperature,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
time = toc
%time(n) = toc
real_temperature = tempReal;

MAPE_now = sum(100*abs((real_temperature-predict_temprature_BME2)./real_temperature))/length(real_temperature);
RMSE_now = sqrt(mean((real_temperature - predict_temprature_BME2).^2))
CVRMSE_now = RMSE_now/mean(real_temperature)*100;

MAPE(n) = MAPE_now
RMSE(n) = RMSE_now
CVRMSE(n) = CVRMSE_now

end



