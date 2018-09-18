%Case 1: all sensors are hard sensors

i=1
motes_for_BME=[1:4 6:7];
locations = dlmread('locations.txt')
rng(1234)
NAPerSensor = randperm(length(motes_for_BME))


    
ck = repelem(locations(ismember(locations(:,1),motes_for_BME),2:3),NAPerSensor,1)
ch = locations(ismember(locations(:,1),motes_for_BME),2:3)  % everything for the moment
cs = []

temp = zeros(length(motes_for_BME),1);
for u = 1:length(motes_for_BME)
   temp(u,:)= eval(['mote_' num2str(motes_for_BME(u)) '_temprature_actual(i);']);
end
temperature = temp
a_temperature=[]
b_temperature=[]

modelCsand={'nuggetC','exponentialC'};
paramsand={-0.0795,[0.6656,22.3507]};
nhmax=3;
nsmax=3;
total=nhmax+nsmax;
dmax=100;
order=0;

predict_temprature_BME = BMEintervalMode(ck,ch,cs,temperature,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);



%Case 1: all sensors are hard sensors delete the one under question

i=1
motes_for_BME=[1:4 6:7];
locations = dlmread('locations.txt');
rng(1234);
NAPerSensor = randperm(length(motes_for_BME));


ck = repelem(locations(ismember(locations(:,1),motes_for_BME),2:3),NAPerSensor,1);
thesensor = repelem(motes_for_BME, NAPerSensor);
%A = zeros(length(motes_for_BME)-1,2,length(thesensor))
ch = zeros(53,2,length(thesensor));

for i = 1:length(thesensor)
   ch(:,:,i) = eval(['variable.locMinusSensor' num2str(thesensor(i))]);
end

ch = locations(ismember(locations(:,1),motes_for_BME),2:3);  % everything for the moment
cs = [];

temp = zeros(length(motes_for_BME),1);
for u = 1:length(motes_for_BME)
   temp(u,:)= eval(['mote_' num2str(motes_for_BME(u)) '_temprature_actual(i);']);
end
temperature = temp;
a_temperature=[];
b_temperature=[];

modelCsand={'nuggetC','exponentialC'};
paramsand={-0.0795,[0.6656,22.3507]};
nhmax=3;
nsmax=3;
total=nhmax+nsmax;
dmax=100;
order=0;

predict_temprature_BME = BMEintervalMode2(ck,ch,cs,temperature,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
