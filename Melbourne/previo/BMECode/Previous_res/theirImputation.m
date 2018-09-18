locations = dlmread('locations.txt');
% First of all we cluster Punit's data as they do

X = locations(:,2:3)
X = X([1:4 6:14 16:17 19:54],:)

% Now apply the K-means algorithm
% Note that order of results may vary
maxerr = 0;
K=8
[proto Nproto membership] = simple_kmeans(X,K,maxerr);

loc2 = locations([1:4 6:14 16:17 19:54],:)
aux = [loc2(:,1) membership]

normc_fcn = @(m) sqrt(m.^2 ./ sum(m.^2));
norm01_fcn = @(m) (m - min(m)) / ( max(m) - min(m) )



for K = 3:20
    [proto Nproto membership] = simple_kmeans(X,K,maxerr);
    loc2 = locations([1:4 6:14 16:17 19:54],:)
    aux = [loc2(:,1) membership];
    for i = 1:K
    motes_current_group = aux(aux(:,2)==i,1)
    aux2 = zeros(1200,length(motes_current_group));
        for m = 1:length(motes_current_group)
        aux2(:,m) = eval(['mote_' num2str(motes_current_group(m)) '_temprature_actual']);
        end
%        csvwrite(['/home/aurora/Escritorio/BMECode/kmeansRes/k' num2str(K) '/gr_' num2str(i) '.csv'], aux2)
%        csvwrite(['/home/aurora/Escritorio/BMECode/kmeansRes/k' num2str(K) '/gr_' num2str(i) 'norm.csv'], normc_fcn(aux2))
%        csvwrite(['/home/aurora/Escritorio/BMECode/kmeansRes/k' num2str(K) '/motes_gr_' num2str(i) '.csv'], motes_current_group)
        a=aux2;
        x = [min(a,[],1);max(a,[],1)];
        b = bsxfun(@minus,a,x(1,:));
        b = bsxfun(@rdivide,b,diff(x,1,1))
        csvwrite(['/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansRes/k' num2str(K) '/gr_' num2str(i) 'norm01.csv'], b)
        [nrowb, ncolb] = size(b)
        b = [zeros(1,ncolb); b]
        csvwrite(['/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansRes/k' num2str(K) '/gr_' num2str(i) 'norm01-i.csv'], b)
    end
end


