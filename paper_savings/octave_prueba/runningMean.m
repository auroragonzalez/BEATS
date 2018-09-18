pkg load statistics

function Y = runningMean(X,d)

L =  length(X);

Y=X;

for i=1+floor(d/2):L-floor(d/2)
    Y(i) = nanmean(X(1+i-floor(d/2):i+floor(d/2)) );
end
        
Y(1: floor(d/2)) = Y(1+floor(d/2))*ones(size(Y(1: floor(d/2))));
Y(end- floor(d/2)+1:end) = nan(size(Y(end- floor(d/2)+1:end)));