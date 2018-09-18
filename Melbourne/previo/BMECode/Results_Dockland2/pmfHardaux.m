rng('default')
clear;
R = zeros(3,5)
rng(1)
R = randn(3, 5) + 2

all_data = zeros(size(R,1)*size(R,2),3);

mote = repelem(1:3,5);
obs = repmat(1:5,1,3);
R2 = R'
temp = R2(:);

all_data(:,1) = mote;
all_data(:,2) = obs;
all_data(:,3) = temp;




all_index = 1:length(mote);
test_index = [5 10 14]
ismem = ismember(all_index,test_index);
train_index = all_index(~ismem);

train = all_data(train_index,:);
%test = all_data(test_index,:);

trainN = train;
for sss = 1:3
a = train(find(train(:,1)==sss ),3 );
x = [min(a,[],1);max(a,[],1)];
b = bsxfun(@minus,a,x(1,:));
b = bsxfun(@rdivide,b,diff(x,1,1));     
trainN(find(train(:,1)==sss ),3 )=b;
end

tempN = []
for sss = 1:3
a = all_data(find(all_data(:,1)==sss ),3 );
x = [min(a,[],1);max(a,[],1)];
b = bsxfun(@minus,a,x(1,:));
b = bsxfun(@rdivide,b,diff(x,1,1));     
tempN(find(all_data(:,1)==sss ),3 )=b;
end
tempN = tempN(:,3)


all_data2 = zeros(length(mote),3);

all_data2(:,1) = mote;
all_data2(:,2) = obs;
all_data2(:,3) = tempN;
test = all_data2(test_index,:);

train = trainN;

rand('state',0); 
randn('state',0); 

  restart=0;
  epsilon=50; % Learning rate 
  lambda  = 0.01; % Regularization parameter 
  momentum=0.8; 

  epoch=1; 
  maxepoch=1000; % change to 50
  train_vec = train;
  probe_vec = test;
  %load moviedata % Triplets: {user_id, movie_id, rating} 
  mean_rating = mean(train_vec(:,3)); 
  
  
  pairs_tr = length(train_vec); % training data 
  pairs_pr = length(probe_vec); % validation data 

  numbatches= 1; % Number of batches  
  num_m = 5;  % Number of movies 
  num_p = 3;  % Number of users 
  num_feat = 2; % Rank 10 decomposition 

  w1_M1     = 0.1*randn(num_m, num_feat); % Movie feature vectors
  w1_P1     = 0.1*randn(num_p, num_feat); % User feature vecators
  w1_M1_inc = zeros(num_m, num_feat);
  w1_P1_inc = zeros(num_p, num_feat);


for epoch = epoch:maxepoch
  %rr = randperm(pairs_tr);
  %train_vec = train_vec(rr,:);
  %clear rr 

  for batch = 1:numbatches
    fprintf(1,'epoch %d batch %d \r',epoch,batch);
    N=size(train_vec,1); % number training triplets per batch 

    aa_p   = double(train_vec((batch-1)*N+1:batch*N,1));
    aa_m   = double(train_vec((batch-1)*N+1:batch*N,2));
    rating = double(train_vec((batch-1)*N+1:batch*N,3));

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

  aa_p = double(probe_vec(:,1));
  aa_m = double(probe_vec(:,2));
  rating = double(probe_vec(:,3));

  pred_out = sum(w1_M1(aa_m,:).*w1_P1(aa_p,:),2) + mean_rating;
%  ff = find(pred_out>5); pred_out(ff)=5; % Clip predictions 
%  ff = find(pred_out<1); pred_out(ff)=1;

  err_valid(epoch) = sqrt(sum((pred_out- rating).^2)/NN);

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   pred_out = sum(w1_M1(aa_m,:).*w1_P1(aa_p,:),2) + mean_rating;
   sqrt(sum((pred_out- rating).^2)/NN);
  
   realTest = temp(test_index);
   maxTrain = max(temp(train_index));
   minTrain = min(temp(train_index));

   predTest = pred_out*(maxTrain-minTrain) + minTrain;
   RMSE =  sqrt(sum((predTest- realTest).^2)/NN);
   err_back(epoch) =  RMSE;
   
  fprintf(1, 'epoch %4i batch %4i Training RMSE %6.4f  Test RMSE %6.4f Test back unnormalised RMSE %6.4f  \n', ...
              epoch, batch, err_train(epoch), err_valid(epoch), err_back(epoch) );
end


