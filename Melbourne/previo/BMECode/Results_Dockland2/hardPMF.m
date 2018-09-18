clear;
rng('default')
N = 1200
NofSensors = 4
load('DocklandDataforBMEExperiment.mat')

RMSE_normalised = []
RMSE_final = []
time = []
RMSE_inner = []


N = 1200
NofSensors = 4

matrixForNA=zeros(NofSensors,N);
[nrow, ncol] = size(matrixForNA);
n=10
nNA = nrow*ncol*n/100;
msize = numel(matrixForNA);
rng(n);
elements = randperm(msize, nNA);
matrixForNA(elements)=1;
[rId, cId] = find(matrixForNA);
 sortrows([rId, cId],1);

positions = zeros(NofSensors,N);
for row= 1:NofSensors
positions(row,:) = 1:N;
end
%csvwrite('/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/NApos.csv', positions.*matrixForNA);


%RENAME the sensors
nodes = [506 509 510 511];

for m = 1:length(nodes)
eval(['node' num2str(m) '= node_' num2str(nodes(m)) '_humidity_measured(1:N);' ]);
end

nodesR = [1 2 3 4]; % rename node



maxk = 1
maxepoch=1000; 

cluster = repelem(1:maxk,1:maxk);
RMSE_inner = zeros(maxepoch, length(cluster));

ssaux = 1
for k = 1:maxk
CLU= dlmread(['BMECode/kmeansResRDock/k' num2str(k) '/groupsk' num2str(k) '.txt']);
for index = 1:k
    %    fprintf('############')
    %    fprintf('k= %s',k)
    %    fprintf('\n')
    %    fprintf('index= %s',index)
    %    fprintf('\n')
    %    fprintf('#############')
submotes_for_BME2=CLU(find(CLU(:,2)==index),1).';
submatrixForNA = matrixForNA(submotes_for_BME2,:);
elements2 = find(submatrixForNA');

mote = repelem(1:length(submotes_for_BME2),1200);
obs = repmat(1:1200,1,length(submotes_for_BME2));

temp = [];
tempN = [];
for i= submotes_for_BME2
temp = [temp; eval(['node' num2str(i)])'];
a = eval(['node' num2str(i)])';
x = [min(a,[],1);max(a,[],1)];
b = bsxfun(@minus,a,x(1,:));
b = bsxfun(@rdivide,b,diff(x,1,1));     
tempN = [tempN; b];

end


all_data = zeros(length(mote),3);

all_data(:,1) = mote;
all_data(:,2) = obs;
all_data(:,3) = tempN;

all_index = 1:length(mote);
test_index = elements2;
ismem = ismember(all_index,test_index);
train_index = all_index(~ismem);

train = all_data(train_index,:);
test = all_data(test_index,:);



% Version 1.000
%

% Code provided by Ruslan Salakhutdinov
%
% Permission is granted for anyone to copy, use, modify, or distribute this
% program and accompanying programs and documents for any purpose, provided
% this copyright notice is retained and prominently displayed, along with
% a note saying that the original programs are available from our
% web page.
% The programs and documents are distributed without any warranty, express or
% implied.  As the programs were written for research purposes only, they have
% not been tested to the degree that would be advisable in any important
% application.  All use of these programs is entirely at the user's own risk.

%rand('state',0); 
%randn('state',0); 

  restart=0;
  epsilon=50; % Learning rate 
  lambda  = 0.01; % Regularization parameter 
  momentum=0.8; 

  epoch=1; 

% load moviedata % Triplets: {user_id, movie_id, rating} 
  mean_rating = mean(train(:,3)); 
  
  
  pairs_tr = length(train); % training data 
  pairs_pr = length(test); % validation data 

  numbatches= 1; % Number of batches  
  num_m = 1200;  % Number of movies 
  num_p = 51;  % Number of users 
  num_feat = 10; % Rank 10 decomposition 

  w1_M1     = 0.1*randn(num_m, num_feat); % Movie feature vectors
  w1_P1     = 0.1*randn(num_p, num_feat); % User feature vecators
  w1_M1_inc = zeros(num_m, num_feat);
  w1_P1_inc = zeros(num_p, num_feat);
  foo = 1;
  
  tic
for epoch = epoch:maxepoch

  rr = randperm(pairs_tr);
  train = train(rr,:);
  clear rr 

  for batch = 1:numbatches
   % fprintf(1,'epoch %d batch %d \r',epoch,batch);
    N=length(train)/numbatches; % number training triplets per batch 

    aa_p   = double(train((batch-1)*N+1:batch*N,1));
    aa_m   = double(train((batch-1)*N+1:batch*N,2));
    rating = double(train((batch-1)*N+1:batch*N,3));

    rating = rating-mean_rating; % Default prediction is the mean rating. 

    %%%%%%%%%%%%%% Compute Predictions %%%%%%%%%%%%%%%%%
    pred_out = sum(w1_M1(aa_m,:).*w1_P1(aa_p,:),2);
    f = sum( (pred_out - rating).^2 + ...
        0.5*lambda*( sum( (w1_M1(aa_m,:).^2 + w1_P1(aa_p,:).^2),2)));

    %%%%%%%%%%%%%% Compute Gradients %%%%%%%%%%%%%%%%%%%
    IO = repmat(2*(pred_out - rating),1,num_feat);
    Ix_m=IO.*w1_P1(aa_p,:) + lambda*w1_M1(aa_m,:);
    Ix_p=IO.*w1_M1(aa_m,:) + lambda*w1_P1(aa_p,:);

    dw1_M1 = zeros(num_m,num_feat);
    dw1_P1 = zeros(num_p,num_feat);

    for ii=1:N
      dw1_M1(aa_m(ii),:) =  dw1_M1(aa_m(ii),:) +  Ix_m(ii,:);
      dw1_P1(aa_p(ii),:) =  dw1_P1(aa_p(ii),:) +  Ix_p(ii,:);
    end

    %%%% Update movie and user features %%%%%%%%%%%

    w1_M1_inc = momentum*w1_M1_inc + epsilon*dw1_M1/N;
    w1_M1 =  w1_M1 - w1_M1_inc;

    w1_P1_inc = momentum*w1_P1_inc + epsilon*dw1_P1/N;
    w1_P1 =  w1_P1 - w1_P1_inc;
  end 

  %%%%%%%%%%%%%% Compute Predictions after Paramete Updates %%%%%%%%%%%%%%%%%
  pred_out = sum(w1_M1(aa_m,:).*w1_P1(aa_p,:),2);
  f_s = sum( (pred_out - rating).^2 + ...
        0.5*lambda*( sum( (w1_M1(aa_m,:).^2 + w1_P1(aa_p,:).^2),2)));
  err_train(epoch) = sqrt(f_s/N);

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%% Compute predictions on the validation set %%%%%%%%%%%%%%%%%%%%%% 
  NN=pairs_pr;

  aa_p = double(test(:,1));
  aa_m = double(test(:,2));
  rating = double(test(:,3));

  pred_out = sum(w1_M1(aa_m,:).*w1_P1(aa_p,:),2) + mean_rating;
%  ff = find(pred_out>5); pred_out(ff)=5; % Clip predictions 
%  ff = find(pred_out<1); pred_out(ff)=1;

  err_valid(epoch) = sqrt(sum((pred_out- rating).^2)/NN);
   fprintf(1, 'epoch %4i batch %4i Training RMSE %6.4f  Test RMSE %6.4f  \n', ...
              epoch, batch, err_train(epoch), err_valid(epoch));
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  %if (rem(epoch,10))==0
   %  save pmf_weight w1_M1 w1_P1
 % end

realTest = temp(test_index);
maxTrain = max(temp(train_index));
minTrain = min(temp(train_index));

predTest = pred_out*(maxTrain-minTrain) + minTrain;

RMSE = sqrt(mean((realTest - predTest).^2));

%fprintf('real RMSE= %s',RMSE);
%fprintf('\n');


%RMSE_inner = [RMSE_inner,RMSE];
RMSE_inner(epoch, ssaux) = RMSE;
end
time = [time, toc]
RMSE_normalised = [RMSE_normalised, err_valid(epoch)]
RMSE_final = [RMSE_final,RMSE]

foo = foo+1
ssaux = ssaux+1;
end

    
end

%csvwrite('C:\Users\AGONZALEZVID\Desktop\Work_melb\actual\BMECode\Results_Dockland\hardPMF1.csv',[cluster; time; RMSE_final]) 
%csvwrite('fig11NA10pNUMCLchangesInnerRMSE.csv',RMSE_inner) 