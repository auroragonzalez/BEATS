clear all

pkg load signal
pkg load statistics

threshold = 12;

%Load electricity
load('TotalPower.mat');
t=time(2:end);

%datenum is a matlab function that gives de number of days from 1 1 year 0
time = datenum(2016,04,15):1/(24*12):datenum(2016,12,01);

%I place the datapoints on my grid
elec = interp1(t,totalPower,time);

elec = moving_average(elec,threshold);
elec = elec/1000;

%Load external temperature from file previously extracted from DB
load('Tout.mat')

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
elec=elec.*lecturingPeriod;
threshold2 = 1;

%elec = moving_average(elec,12*24*7);
%demand = moving_average(demand,12*24*1);
%I do a sinuoidal curve to compare but I do not use it in the algorithm
Tout2 = 20+8*sin(time*2*pi/365+120*pi/180);
tops = 20+8*sin(time*2*pi/365+120*pi/180);

%I use this one instead
threshold = 24*12;
tout=tout.*lecturingPeriod;
%plot(tout); hold on

tout = moving_average(tout,threshold);
%plot(tout,'r');

%Now that we have daily temperatures we calculate cdd
tempAbove21 = ( tout>21).* (tout-21);
%plot(tempAbove21,'g');
cdd = 0.0034722*nansum(tempAbove21) %Dday

threshold = 12;
tops = tout;


%I place a threshold from which AC is starting to be used
thc=18;
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
tops = 20*tops;  %20kW per K
%Also find the displacement that fits the curve best
tops = tops+150;   %150kW of non-conditioning load

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
Y = weeklyProfile(45,tops2,3);
t1=1328
t2=1340
Y(t1:t2)=3.5*ones(size(Y(t1:t2)));

badWeeks = [4 14:21 22];
per = 1;

for w=1
  if any(w==badWeeks)
    %continue
  end
  L1 = 26+48+(w-1)*24*per;
  L2 = 26+48+(w)*24*per;
  
  L1 = 1;
  L2 = 5520;
  rng =L1:L2;

  %f = figure
  plot(time(rng), tout(rng),'m'); hold on
  plot(time(rng), elec(rng),'-'); hold on
  %plot(time,tops2,'k-');
  plot(time(rng),Y(rng),'r.-');

  grid on
  %xlim([datenum(2016,03,15) datenum(2016,12,15)])
  datetick('x',"mm-YYYY")

  %Zoom
  %xlim([datenum(2016,05,15) datenum(2016,7,15)])
  %datetick('x',"dd-mm-YYYY")

  ylim([0 500]);
  ylabel('Temp. [C] & Power [kW]')
  legend('Tout(mean)','Elec. Power','predicted')
  title(sprintf('Week %d',w));
  %print(f,'FinalResultZoom.png')
  if 0
    res = (elec(rng)-Y(rng));
      
    predErr = nansum (res)
    errPercentage = 100*predErr/nansum(elec(rng))
    predError(w)  =  nansum (res);
    predEner(w) = nansum(Y(rng));
  end
end