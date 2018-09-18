locations = dlmread('locations.txt')


N=length(locations)

for k=1:N
    locMinusSensor = locations(1:end ~= k,2:3)
    my_field = strcat('locMinusSensor',num2str(k));
    variable.(my_field) =locMinusSensor;
end

save('variable')

load('variable.mat')
k=2
eval(['variable.locMinusSensor' num2str(k)])



motes_for_BME=[1:4 6:14 16:17 19:54];

for k=motes_for_BME
    locMinusSensor = locations(motes_for_BME ~= k,2:3)
    my_field = strcat('locMinusSensor',num2str(k));
    variable2.(my_field) =locMinusSensor;
end

save('variable2')
