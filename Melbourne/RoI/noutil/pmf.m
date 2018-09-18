R = [5 3 0 1; 4 0 0 1; 1 1 0 5; 1 0 0 4; 0 1 5 4];


X = R
learning_rate=0.001
momentum=0.8
max_epoch=1000
regularization=0.25
minibatch_size=1
rank=5
max_epoch=250
nan_value=0
status_percentage=0.1



[N M] = size(X)

X_mean = mean(mean(X));
lr = learning_rate
reg = regularization
mom = momentum
rng(1234)
U = 0.1*randn(N, rank)
V = 0.1*randn(M, rank)


U_inc = zeros(N,rank)
V_inc = zeros(M,rank)
dU =zeros(N,rank)
dV = zeros(M,rank)
epoch = 0



epoch=1

for epoch = 1:1000
[r, c] = find(X) ;
mat = [r,c];
[~,idx] = sort(mat(:,1)); % sort just the first column
pairs = mat(idx,:);   % sort the whole matrix using the sort indices
mbIndices = 1:(size(pairs,1)+1);
start_indices = mbIndices(1:(size(pairs,1)-1));
end_indices= mbIndices(2:size(pairs,1));
mb_indices = [start_indices' end_indices'];
n_batches = length(mb_indices);

mean_abs_err = 0;    

for iter = 1:size(mb_indices,1)    
    i=mb_indices(iter,1);
    j=mb_indices(iter,2);
    dU =zeros(N,rank);
    dV = zeros(M,rank);
    r_i = pairs(iter,1);
    c_i = pairs(iter,2);
    X_i = X(r_i, c_i) - X_mean;
    pred = sum(U(r_i,:).* V(c_i,:));  %rows times rows because it is U times V transpose
    grad_loss = 2*(pred-X_i);
    
    grad_U = grad_loss * V(c_i,:) + reg * U(r_i,:);
    grad_V = grad_loss * U(r_i,:) + reg * V(c_i,:);
    dU(r_i,:) = grad_U;
    dV(c_i,:) = grad_V;
    U_inc = mom * U_inc + lr * dU;
    V_inc = mom * V_inc + lr * dV;
    U = U- U_inc
    V = V- V_inc
end
    

    
end


R2 = U*(V')+ X_mean

