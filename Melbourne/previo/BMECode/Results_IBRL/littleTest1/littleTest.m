load('variable2');
motes_for_BME=[1:4 6:14 16:17 19:54];
hard = [1 7 13 16 20 26 40 47 50 54];
soft = motes_for_BME(~ismember(motes_for_BME, hard));

%controlled missing data: at observation = mote id (so mote 1 has a
%missing value in 1st position, mote 2 has a missing value in 2nd
%position... and so on)

N=54;
matrixForNA2=eye(54)

k =1 %we do it with 1 cluster (all together)
CLU= dlmread(['BMECode/kmeansResR/k' num2str(k) '/groupsk' num2str(k) '.txt']);

submotes_for_BME2=CLU(find(CLU(:,2)==index),1).';
submatrixForNA = matrixForNA2(submotes_for_BME2,:);
elements2 = find(submatrixForNA');

mote = repelem(1:length(submotes_for_BME2),54);   %change
obs = repmat(1:54,1,length(submotes_for_BME2));

temp = [];
tempN = [];
tempT= [];

for i= submotes_for_BME2
temp = [temp; eval(['mote_' num2str(i) '_temprature_actual(1:54)'])];
%tempT = [tempT; eval(['mote_' num2str(i) '_temprature_actual_prev'])];
a = eval(['mote_' num2str(i) '_temprature_actual(1:54)']);
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


%%%%%%PMF

  restart=0;
  epsilon=50; % Learning rate 
  lambda  = 0.01; % Regularization parameter 
  momentum=0.8; 

  epoch=1; 
  maxepoch=1000; % change to 1000

% load moviedata % Triplets: {user_id, movie_id, rating} 
  mean_rating = mean(train(:,3)); 
  
  
  pairs_tr = length(train); % training data 
  pairs_pr = length(test); % validation data 

  numbatches= 1; % Number of batches  
  num_m = 54;  % Number of observations 
  num_p = 51;  % Number of sensors 
  num_feat = 10; % Rank 10 decomposition 

  w1_M1     = 0.1*randn(num_m, num_feat); % Obs feature vectors
  w1_P1     = 0.1*randn(num_p, num_feat); % Sens feature vecators
  w1_M1_inc = zeros(num_m, num_feat);
  w1_P1_inc = zeros(num_p, num_feat);
  foo = 1;

for epoch = epoch:maxepoch

   % fprintf(1,'epoch %d batch %d \r',epoch,batch);
    N=length(train)/numbatches; % number training triplets per batch 

    aa_p   = double(train(:,1));
    aa_m   = double(train(:,2));
    rating = double(train(:,3));

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

end

realTest = temp(test_index);
maxTrain = max(temp(train_index));
minTrain = min(temp(train_index));

predTest = pred_out*(maxTrain-minTrain) + minTrain;

RMSE = sqrt(mean((realTest - predTest).^2))


%%%%%%PMF END%%%%%%%








