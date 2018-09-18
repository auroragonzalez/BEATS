clear;

Nn = 1200
NofSensors = 4
nodesSoft = dlmread('nodesSoft2.csv');

max_epoch=1000
rank=20
lr=0.001
mom=0.8
reg=0.25
norm =0 ;



nodesSoft = nodesSoft(:,1:1200)

for m = 1:NofSensors
eval(['node' num2str(m) '_prev= nodesSoft(' num2str(m) ',:)' ]);
end


hard = [];
soft =[ 1 2 3 4];

for m = soft
    m
    eval(['node' num2str(m) '= node' num2str(m) '_prev -5 + (5+5)*rand(1,Nn);' ]);
end

for m = hard
eval(['node' num2str(m) '= node' num2str(m) '_prev;' ]);
end


R = [node1; node2; node3; node4]
Rgt = [node1_prev; node2_prev; node3_prev; node4_prev];



%%%%%%%%%%%%%%%%%%%%
%%%% EXPERIMENTS %%%
%%%%%%%%%%%%%%%%%%%%

NAS= 1:90

%%% SOFT experiment

RMSEsoft = zeros(length(NAS),1)
timesoft = zeros(length(NAS),1)
for k = NAS
    tic
    [R2, elements] = pmf3( k, R, max_epoch, rank, lr, mom,reg,norm);
    realTest = Rgt(elements);
    predTest = R2(elements);
    RMSEsoft(k) = sqrt(mean((realTest - predTest).^2)) ;
    timesoft(k) = toc;
end

%%% HARD experiment
RMSEhard = zeros(length(NAS),1)
timehard = zeros(length(NAS),1)
for k = NAS
    tic
    [R2, elements] = pmf3(k, Rgt, max_epoch, rank, lr, mom,reg,norm);
    realTest = Rgt(elements);
    predTest = R2(elements);
    RMSEhard(k) = sqrt(mean((realTest - predTest).^2));
    timehard(k) = toc;
end



csvwrite('/home/aurorax/Git_repos/posgradoActual/Melbourne/RoI/testDocksoftPMF.csv', [RMSEsoft timesoft RMSEhard timehard])


%%% SOFT deltas experiment

deltas= 1:0.5:15



RMSEdeltas = zeros(length(deltas),1)
for k = 1:length(deltas)
    delta = deltas(k)
    for m = soft
        eval(['node' num2str(m) '= node' num2str(m) '_prev -' num2str(delta) '+(' num2str(delta) '+' num2str(delta) ')*rand(1,Nn);' ]);
    end
    R = [node1; node2; node3; node4]
    [R2, elements] = pmf3(10, R, max_epoch, rank, lr, mom,reg,norm);
    realTest = Rgt(elements);
    predTest = R2(elements);
    RMSEdeltas(k) = sqrt(mean((realTest - predTest).^2)) ;
end
csvwrite('/home/aurorax/Git_repos/posgradoActual/Melbourne/RoI/testDocksoftPMFdeltas.csv', [RMSEdeltas])
