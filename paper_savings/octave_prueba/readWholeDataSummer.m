clear all

pkg load signal
runningMean


function Y = downsampling (X,l)
k=0;
for i=1:length(X)
    if rem(i,l)==0
        k=k+1;
        Y(k) = X(i);
   end
end
endfunction

threshold = 12;

%Load electricity
load('wholeElec.mat')


%datenum is a matlab function that gives de number of days from 1 1 year 0
time = datenum(2016,04,15):1/(24*12):datenum(2016,12,01);

%I place the datapoints on my grid
elec = interp1(Elec(:,1),Elec(:,2),time);

%csvwrite('Elec.csv', Elec);

%elec = moving_average(elec,threshold);

elec = runningMean(elec,threshold);

%Load external temperature from file previously extracted from DB
load('Tout.mat')

%csvwrite('Tout.csv', Tout);

tout = interp1(Tout(:,1),Tout(:,2),time);


%Academic period (lectures) Summer
start = [ datenum(2016,04,07)  datenum(2016,9,04)];
stop = [datenum(2016,07,21)  datenum(2016,12,23) ];

lecturingPeriod = nan(size(time));
%remove holidays periods
for j=1:2
    for i=1:length(time)
        if isnan(lecturingPeriod(i)) && time(i)>start(j) && time(i)<stop(j)
            lecturingPeriod(i) = 1;
        end
    end
end

%Restringir consumo electrico a periodos lectivos
elec=elec.*lecturingPeriod;  %CUIDADO! ESTA FUNCIÓN NO HACE NADA....COMPROBAR elec ANTES Y DESPUES
threshold2 = 1;

%elec = moving_average(elec,12*24*7);
%demand = moving_average(demand,12*24*1);
%I do a sinuoidal curve to compare but I do not use it in the algorithm
Tout2 = 20+8*sin(time*2*pi/365+120*pi/180);
tops = 20+8*sin(time*2*pi/365+120*pi/180);

%I use this one instead
threshold = 240;
%tout = moving_average(tout,threshold);
tout = runningMean(tout,threshold);

threshold = 12;
tops = tout;


%I place a threshold from which AC is starting to be used
thc=20.5;
thh=15;
for i=1:length(tops)
    if tops(i)<thh
        tops(i) = thh-tops(i);
        continue
    end
    if tops(i)>thc
            tops(i) = tops(i)-thc;
            continue
    end
    tops(i) = 0;
end

%these two parameters in the future can be estimated with an optimisation algorithm
%I find the parameter that will fit the curve best. Also change from degree to kW
tops = 0.8*tops;
%Also find the displacement that fits the curve best
tops = tops+5.5;

%I do not use this anymore
%lag = 15 ; %days
%lag = lag*24*12;

%tops2(1+lag:length(tops)) = tops(1:length(tops)-lag);
tops2 = tops;

%I downsample everything to hours
inc = 12*1;
time = downsampling(time,inc);
Tout2 = downsampling(Tout2,inc);
tout = downsampling(tout,inc);
%demand = downsampling(demand,inc);
elec = downsampling(elec,inc);
tops2 = downsampling(tops2,inc);

%Creating artificial profile
Y = weeklyProfile(1.55,tops2,5);

f = figure
plot(time, tout,'m'); hold on
plot(time,0.0001* elec,'-'); hold on
%plot(time,tops2,'k-');
plot(time,Y,'r--');
plot([datenum(2016,7,29,12) datenum(2016,8,13,12)] ,[18 18],'k.-');

grid on
xlim([datenum(2016,03,15) datenum(2016,12,15)])
datetick('x',"mm-YYYY")

%Zoom
xlim([datenum(2016,05,15) datenum(2016,7,15)])
%datetick('x',"dd-mm-YYYY")


ylim([0 40]);
ylabel('Temp. [C] & Power [10000W]')
legend('Tout(mean)','Elec. Power','predicted')

print(f,'FinalResultZoom.png')
