function tf = isergodic(mc)
% ISERGODIC Check Markov chain for ergodicity
%
% Syntax:
%
%   tf = isergodic(mc)
%
% Description:
%
%   ISERGODIC checks discrete-time Markov chain mc for ergodicity.
%
% Input Argument:
%
%   mc - Discrete-time Markov chain (@dtmc) object.
%
% Output Argument:
%
%   tf - Logical value indicating whether mc is ergodic.
%
% Notes:
%     
%   o A Markov chain is ergodic if it is both irreducibile and aperiodic.
%     This is equivalent to mc.P being a primitive nonnegative matrix.
%     
%   o By Wielandt's theorem [3], a Markov chain mc is ergodic if and only
%     if all elements of (mc.P)^m are positive for m = (n-1)^2 + 1, where 
%     n is the number of states. ISERGODIC computes this matrix power to
%     determine ergodicity.
%
%   o By the Perron-Frobenius Theorem [2], ergodic Markov chains have
%     unique limiting distributions. That is, they have unique stationary
%     distributions to which every initial distribution converges. Ergodic
%     unichains, which consist of a single ergodic class plus transient
%     classes, also have unique limiting distributions (with zero
%     probability mass in the transient classes).
%
% Example:
%
%   % Periodic chain:
%   P = [0 1 0; 0 0 1; 1 0 0];
%   mc = dtmc(P);
%   isreducible(mc)
%   isergodic(mc)
%
% References:
% 
%   [1] Gallager, R.G. Stochastic Processes: Theory for Applications.
%       Cambridge, UK: Cambridge University Press, 2013.
%
%   [2] Horn, R., and C. R. Johnson. Matrix Analysis. Cambridge, UK:
%       Cambridge University Press, 1985.
%
%   [3] H. Wielandt. "Unzerlegbare, Nicht Negativen Matrizen."
%       Mathematische Zeitschrift. Vol. 52, 1950, pp. 642-648.
%
% See also DTMC/ISREDUCIBLE, DTMC/ASYMPTOTICS

% Copyright 2017 The MathWorks, Inc.

P = mc.P;
numStates = mc.NumStates;

zeroTol = numStates*eps(1);

% Compute matrix power:

m = (numStates-1)^2 + 1; % Weilandt exponent
Q = P^m;

tf = full(~any(Q(:) < zeroTol));