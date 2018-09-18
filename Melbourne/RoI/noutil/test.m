clear;

Nn = 1200
NofSensors = 4
load('DocklandDataforBMEExperiment.mat')
locations = mote_position

nodes = [506 509 510 511];

for m = 1:length(nodes)
eval(['node' num2str(m) '_prev= node_' num2str(nodes(m)) '_humidity_measured(1:Nn);' ]);
end


hard = [];
soft =[ 1 2 3 4];

for m = soft
eval(['node' num2str(m) '= node' num2str(m) '_prev -5 + (5+5)*rand(1,Nn);' ]);
end

for m = hard
eval(['node' num2str(m) '= node' num2str(m) '_prev;' ]);
end


R = [node1; node2; node3; node4]

max_epoch=100
rank=20
lr=0.001
mom=0.8
reg=0.25
minibatch_size=1
norm =0 ;

[R2, elements] = pmf3(locations, 10, R, max_epoch, rank, lr, mom,reg, minibatch_size,norm);


realTest = R(elements);
predTest = R2(elements);
RMSE1 = sqrt(mean((realTest - predTest).^2))





Rgt = [node1_prev; node2_prev; node3_prev; node4_prev];
realTest = Rgt(elements);
RMSE2 = sqrt(mean((realTest - predTest).^2))
[R2, elements] = pmf3(locations, 10, Rgt, max_epoch, rank, lr, mom,reg, minibatch_size,norm);

realTest = Rgt(elements);
predTest = R2(elements);
RMSE3 = sqrt(mean((realTest - predTest).^2))



