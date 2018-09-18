load('variable2')

for K = 3:20
    aux = dlmread(['/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansResR/k' num2str(K) '/groupsk' num2str(K) '.txt'])    
    for i = 1:K
    motes_current_group = aux(aux(:,2)==i,1)
    aux2 = zeros(1200,length(motes_current_group));
        for m = 1:length(motes_current_group)
        aux2(:,m) = eval(['mote_' num2str(motes_current_group(m)) '_temprature_actual']);
        end
        csvwrite(['/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansResR/k' num2str(K) '/gr_' num2str(i) '.csv'], aux2);
%        csvwrite(['/home/aurora/Escritorio/BMECode/kmeansRes/k' num2str(K) '/gr_' num2str(i) 'norm.csv'], normc_fcn(aux2));
        csvwrite(['/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansResR/k' num2str(K) '/motes_gr_' num2str(i) '.csv'], motes_current_group);
        a=aux2;
        x = [min(a,[],1);max(a,[],1)];
        b = bsxfun(@minus,a,x(1,:));
        b = bsxfun(@rdivide,b,diff(x,1,1));
        csvwrite(['/home/aurora/Git_repos/posgradoActual/Melbourne/BMECode/kmeansResR/k' num2str(K) '/gr_' num2str(i) 'norm01.csv'], b);
    end
end


