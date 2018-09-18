% deleted: nan_value, status_percentage


function [R2] = pmf4(RoI, max_epoch, rank, lr, mom,reg, normalize)
[N,M] = size(RoI);

if normalize == 1
    RoIprev= RoI;
    for j = 1:N
        a = RoI(j,find(RoI(j,:)))
        b = (a-min(a))/(max(a)-min(a))
        RoI(j,find(RoI(j,:))) = b
    end

end



R_mean = mean(mean(RoI));
rng(1234);
U = 0.1*randn(N, rank);
V = 0.1*randn(M, rank);

U_inc = zeros(N,rank);
V_inc = zeros(M,rank);

dU =zeros(N,rank);
dV = zeros(M,rank);


for epoch = 1:max_epoch
[r, c] = find(RoI) ;
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
    X_i = RoI(r_i, c_i) - R_mean;
    pred = sum(U(r_i,:).* V(c_i,:));  %rows times rows because it is U times V transpose
    grad_loss = 2*(pred-X_i);
    
    grad_U = grad_loss * V(c_i,:) + reg * U(r_i,:);
    grad_V = grad_loss * U(r_i,:) + reg * V(c_i,:);
    dU(r_i,:) = grad_U;
    dV(c_i,:) = grad_V;
    U_inc = mom * U_inc + lr * dU;
    V_inc = mom * V_inc + lr * dV;
    U = U- U_inc;
    V = V- V_inc;
end
    
epoch    
end


R2 = U*(V')+ R_mean;


if normalize == 1
    R2prev= R2;
    for j = 1:N
        maxTrain = max(RoIprev(j,:));
        minTrain =  min(RoIprev(j,find(RoIprev(j,:))));
        R2(j,:) = R2(j,:)*(maxTrain-minTrain) + minTrain;
    end

end


end








