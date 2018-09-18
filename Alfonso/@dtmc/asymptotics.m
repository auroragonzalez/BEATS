function [xFix,tMix] = asymptotics(mc)
% ASYMPTOTICS Determine Markov chain asymptotics
%
% Syntax:
%
%   [xFix,tMix] = asymptotics(mc)
%
% Description:
%
%   ASYMPTOTICS finds stationary distribution xFix of discrete-time Markov
%   chain mc and estimates mixing time tMix.
%
% Input Arguments:
%
%   mc - Discrete-time Markov chain (@dtmc) object, with NumStates states
%        and transition matrix P.
%
% Output Arguments:
%
%   xFix - Stationary distribution, with xFix*P = xFix. For unichains, the
%          distribution is unique, and xFix has size 1-by-NumStates.
%          Otherwise, each row of xFix represents a distinct stationary
%          distribution, with the number of rows determined by the number
%          of independent recurrent classes in mc.
%
%   tMix - Mixing time -1/log(mu), where mu is the second largest
%          eigenvalue modulus (SLEM) of P, if it exists and is nonzero.
%
% Notes:
%
%   o Every Markov chain has a left eigenvector xFix with eigenvalue 1, as
%     a consequence of P being a nonnegative stochastic matrix. The Perron-
%     Frobenius Theorem [2] implies that if mc is a unichain (a chain with
%     a single recurrent communicating class), then xFix is unique. For
%     reducible chains with multiple recurrent classes, eigenvalue 1 has
%     higher multiplicity, and xFix is nonunique. If a chain is periodic,
%     xFix will be stationary but not limiting, in the sense that arbitrary
%     initial distributions will not converge to it. Only for ergodic
%     chains is xFix both unique and limiting. See DTMC/CLASSIFY.
%
%	o For ergodic chains, tMix is a characteristic time for any initial
%     distribution to converge to xFix. Specifically, it is the time for
%     the total variation distance between an initial distribution and xFix
%     to decay by a factor of e = exp(1). Mixing times are a measure of the
%     relative connectivity of transition structures in different chains.
%
% Example:
%
%   mc = dtmc(blkdiag(rand(2),rand(3)));
%   x = asymptotics(mc)
%   mc1 = subchain(mc,1);
%   mc2 = subchain(mc,3);
%   [x1,t1] = asymptotics(mc1)
%   [x2,t2] = asymptotics(mc2)
%
% References:
% 
%   [1] Gallager, R.G. Stochastic Processes: Theory for Applications.
%       Cambridge, UK: Cambridge University Press, 2013.
%
%   [2] Horn, R., and C. R. Johnson. Matrix Analysis. Cambridge, UK:
%       Cambridge University Press, 1985.
%
%   [3] Seneta, E. Non-negative Matrices and Markov Chains. New York, NY:
%       Springer-Verlag, 1981.
%
% See also DTMC/ISREDUCIBLE, DTMC/ISERGODIC, DTMC/CLASSIFY, DTMC/EIGPLOT

% Copyright 2017 The MathWorks, Inc.

P = full(mc.P);
numStates = mc.NumStates;

zeroTol = numStates*eps(1);

% Compute xFix:

[eVecs,eVals] = eig(P','vector');
m = sort(abs(eVals),'descend');
rho = m(1);
PFIdx = (abs(eVals-rho) < rho*zeroTol)';
xFix = eVecs(:,PFIdx)';
xFix = abs(xFix);
xFix = xFix./sum(xFix,2);

if nargout > 1
    
    % Compute tMix:
    
    SLEMIdx = find(abs(m-rho) > rho*zeroTol,1); 
    mu = m(SLEMIdx);
    
    if isempty(mu)
        
        error(message('econ:dtmc:asymptotics:NoSLEM'))
        
    elseif mu < zeroTol
        
        error(message('econ:dtmc:asymptotics:ZeroSLEM'))
        
    else
            
        tMix = -1/log(mu);
        
    end
    
end