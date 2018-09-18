function lc = lazy(mc,varargin)
% LAZY Adjust Markov chain state inertia
%
% Syntax:
%
%   lc = lazy(mc)
%   lc = lazy(mc,w)
%
% Description:
%
%   LAZY transforms discrete-time Markov chain mc into a "lazy chain" lc
%   with adjusted state inertia.
%
% Input Arguments:
%
%   mc - Discrete-time Markov chain (@dtmc) object with NumStates states
%        and transition matrix P.
%
%   w - Scalar or vector of inertial weights with values between 0 and 1.
%       If w is a scalar, it is applied to all states via the linear
%       transformation (lc.P) = (1-w)*(mc.P) + w*I. If w is a vector of
%       length NumStates, weights are applied state by state (row by row).
%       The default value is 0.5.
%
% Output Argument:
%
%   lc - Discrete-time Markov chain (@dtmc) object. Lazy version of mc.
%
% Notes:
%
%   o In the directed graph associated with mc, the default "lazy"
%     transformation ensures self-loops on all states, eliminating
%     periodicity. If mc is irreducible, lc is ergodic. See DTMC/GRAPHPLOT.
%
% Example:
%
%   % Irreducible, periodic chain:
%   P = [0 1 0; 0 0 1; 1 0 0];
%   mc = dtmc(P);
%   x0 = asymptotics(mc)
%   isergodic(mc)
% 
%   % Ergodic chain:
%   lc = lazy(mc);
%   x1 = asymptotics(lc)
%   isergodic(lc)
%
% References:
% 
%   [1] Gallager, R.G. Stochastic Processes: Theory for Applications.
%       Cambridge, UK: Cambridge University Press, 2013.
%
% See also DTMC/ASYMPTOTICS

% Copyright 2017 The MathWorks, Inc. 

P = mc.P;
numStates = mc.NumStates;
stateNames = mc.StateNames;

parseObj = inputParser;

addOptional(parseObj,'w',0.5,...
    @(x)validateattributes(x,{'numeric'},{'vector','>=',0,'<=',1}))

parse(parseObj,varargin{:});

w = parseObj.Results.w;

% Check length of weights vector:

if ~isscalar(w) && (length(w) ~= numStates)
    
	error(message('econ:dtmc:lazy:WeightsWrongSize'))
    
end

% Compute lazy chain:

w = w(:); % Column vector
Q = (1-w).*P + w.*eye(numStates);

lc = dtmc(Q,'StateNames',stateNames);