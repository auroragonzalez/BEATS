function X = redistribute(mc,numSteps,varargin)
% REDISTRIBUTE Compute Markov chain redistributions
%
% Syntax:
%
%   X = redistribute(mc,numSteps)
%   X = redistribute(...,param,val,...)
%
% Description:
%
%   REDISTRIBUTE computes data X on the evolution of a distribution of
%   states in discrete-time Markov chain mc.
%
% Input Arguments:
%
%   mc - Discrete-time Markov chain (@dtmc) object, with NumStates states.
%
%   numSteps - Positive integer specifying the number of discrete time
%        steps to compute.
%
% Optional Input Parameter Name/Value Pairs:
%
%   NAME	VALUE
%
%   'X0'    Initial distribution. X0 is a nonnegative vector of length
%           NumStates (which REDISTRIBUTE normalizes to sum to 1). The
%           default is a uniform distribution of states.
%
% Output Argument:
%
%   X - X is an array of state probabilities of size (1+numSteps)-by-
%       NumStates. The first row is X0. Subsequent rows are the
%       redistributions at each step, determined by transition matrix mc.P.
%
% Notes:
%
%   o If mc is ergodic, and numSteps is sufficiently large, X(end,:) will
%     approximate x = asymptotics(mc). See DTMC/ASYMPTOTICS.
%
%   o Use DTMC/DISTPLOT to visualize the data created by REDISTRIBUTE.
%
% Example:
%
%   % 10-step evolution of a uniform distribution of states:
%
%   mc = mcmix(4,'Zeros',8);
%   X = redistribute(mc,10);
%   distplot(mc,X)
%
% See also DTMC/DISTPLOT, DTMC/ASYMPTOTICS, DTMC/SIMULATE

% Copyright 2017 The MathWorks, Inc.

P = mc.P;
numStates = mc.NumStates;

% Parse inputs and set defaults:

parseObj = inputParser;

addRequired(parseObj,'numSteps',...
        @(x)validateattributes(x,{'numeric'},...
        {'scalar','positive','integer'}))
   
addParameter(parseObj,'X0',ones(1,numStates)/numStates,...
        @(x)validateattributes(x,{'numeric'},...
        {'vector','nonnegative','numel',numStates}))
   
parse(parseObj,numSteps,varargin{:});

numSteps = parseObj.Results.numSteps;
X0 = parseObj.Results.X0;

if sum(X0) == 0
    
    error(message('econ:dtmc:redistribute:ZeroInitialDistribution'))
    
else
    
    X0 = X0/sum(X0); % Normalize initial distribution

end

% Compute redistributions:
    
X = zeros(1+numSteps,numStates);
X(1,:) = X0;

for i = 2:(1+numSteps)

    X(i,:) = X(i-1,:)*P;
    X(i,:) = X(i,:)/sum(X(i,:)); % Renormalize

end