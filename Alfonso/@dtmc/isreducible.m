function tf = isreducible(mc)
% ISREDUCIBLE Check Markov chain for reducibility
%
% Syntax:
%
%   tf = isreducible(mc)
%
% Description:
%
%   ISREDUCIBLE checks discrete-time Markov chain mc for reducibility.
%
% Input Argument:
%
%   mc - Discrete-time Markov chain (@dtmc) object.
%
% Output Argument:
%
%   tf - Logical value indicating whether mc is reducible.
%
% Notes:
%
%   o A Markov chain is reducible if it consists of more than one
%     communicating class. Asymptotic analysis is "reduced" to individual
%     classes. See DTMC/CLASSIFY, DTMC/ASYMPTOTICS.
%     
%   o A Markov chain mc is irreducible if every state is reachable from
%     every other state in at most n-1 steps, where n is the number of
%     states. This is equivalent to (I+Z)^(n-1) containing all positive
%     elements, where Z = (mc.P > 0) is the zero-pattern matrix of mc [2].
%     ISREDUCIBLE computes this matrix power to determine reducibility.
%
%   o By the Perron-Frobenius Theorem [2], irreducible Markov chains
%     have unique stationary distributions. Unichains, which consist of a
%     single recurrent class plus transient classes, also have unique
%     stationary distributions (with zero probability mass in the transient
%     classes). Reducible chains with multiple recurrent classes have
%     stationary distributions that depend on the initial distribution.
%
% Example:
%
%   % Reducible chain:
%   P = [0.5 0.5 0; 0.5 0.5 0; 0 0 1];
%   mc = dtmc(P);
%   isreducible(mc)
%
% References:
% 
%   [1] Gallager, R.G. Stochastic Processes: Theory for Applications.
%       Cambridge, UK: Cambridge University Press, 2013.
%
%   [2] Horn, R., and C. R. Johnson. Matrix Analysis. Cambridge, UK:
%       Cambridge University Press, 1985.
%
% See also DTMC/ISERGODIC, DTMC/ASYMPTOTICS

% Copyright 2017 The MathWorks, Inc.

P = mc.P;
numStates = mc.NumStates;

zeroTol = numStates*eps(1);

% Compute matrix power:

Z = (P > zeroTol);
Q = (eye(numStates)+Z)^(numStates-1);

tf = full(any(Q(:) < zeroTol));