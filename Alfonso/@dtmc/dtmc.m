classdef (Sealed,InferiorClasses = {?matlab.graphics.axis.Axes,?matlab.ui.control.UIAxes}) dtmc
%DTMC Create discrete-time Markov chain
%
% Syntax:
%
%   mc = dtmc(P)
%   mc = dtmc(...,param,val,...)
%
% Description:
%
%   DTMC constructs discrete-time, finite-state, time-homogeneous Markov
%   chain mc, specified by state transition matrix P.
%
% Input Argument:
%
%   P - numStates-by-numStates transition matrix. P(i,j) is either the
%       theoretical nonnegative probability of a transition from state i to
%       state j or else an empirical count of observed transitions from
%       state i to state j. Each row of P with a non-NaN sum is normalized
%       to 1 when mc.P is constructed. If x is a row vector of length
%       numStates specifying a distribution of states at time t (x sums to
%       one), then x*mc.P is the distribution of states at time t+1.
%
%       P allows NaN entries to indicate estimable values. Rows containing
%       NaNs are not normalized by DTMC.
%
% Optional Input Parameter Name/Value Pair:
%
%   NAME            VALUE
%
%   'StateNames'	String, cell, or numeric vector of state labels, of
%                   length numStates. The default is a string vector of
%                   numerical labels, string(1:numStates).
%
% Output Argument:
%
%   mc - Discrete-time Markov chain (@dtmc) object with properties:
%
%        o P            % Normalized transition matrix
%        o NumStates    % Number of states
%        o StateNames   % String vector of state names
%
% Example:
%
%   % Two-state business cycle ([3], p. 697):
%
%   p11 = 0.90; % Persistence of expansion state
%   p22 = 0.75; % Persistence of contraction state
%   P = [p11,1-p11;1-p22,p22];
%   mc = dtmc(P,'StateNames',{'Expansion','Contraction'})
%
% References:
% 
%   [1] Gallager, R.G. Stochastic Processes: Theory for Applications.
%       Cambridge, UK: Cambridge University Press, 2013.
%
%   [2] Haggstrom, O. Finite Markov Chains and Algorithmic Applications.
%       Cambridge, UK: Cambridge University Press, 2002.
%  
%   [3] Hamilton, J. D. Time Series Analysis. Princeton, NJ: Princeton
%       University Press, 1994.
%
%   [4] Norris, J. R. Markov Chains. Cambridge, UK: Cambridge University
%       Press, 1997.
%
% See also MCMIX

% Copyright 2017 The MathWorks, Inc.

    properties (SetAccess = private, GetAccess = public)

        P % Normalized transition matrix
        
    end
    
    properties (SetAccess = public, GetAccess = public)
                
        StateNames % Names for states 1:NumStates
        
    end
    
    properties (Dependent, SetAccess = private, GetAccess = public)
                
        NumStates % Number of states
        
    end

    methods
        
        % Constructor
        function mc = dtmc(P,varargin)
            
            if  nargin == 0 % Construct default object

                mc.P = [];
                mc.StateNames = string({});

            else % Construct specified object
                
                parseObj = inputParser;

                addRequired(parseObj,'P',...
                   @(x)validateattributes(x,{'numeric','logical'},...
                    {'nonempty','nonnegative','2d','square'}))
                 
                addParameter(parseObj,'StateNames',{},...
                   @(x)validateattributes(x,{'numeric','cell','string'},...
                    {'vector'}))
                 
                parse(parseObj,P,varargin{:});
                
                P = parseObj.Results.P;
                
                % Normalize non-NaN rows:
                I = any(isnan(P),2);
                P(~I,:) = P(~I,:)./sum(P(~I,:),2);
                
                names = parseObj.Results.StateNames; 

                mc.P = P;
                mc.StateNames = names;
            
            end
            
        end % Constructor
        
        % Set StateNames
        function mc = set.StateNames(mc,names)
            
            numStates = mc.NumStates; %#ok
            
            stateNames = string(names);
            
            if isempty(stateNames)
                
                    mc.StateNames = string(1:numStates); % Assign default
                    
            elseif (~isvector(stateNames)) || ...
                   (length(stateNames) ~= numStates)
                
                    error(message('econ:dtmc:dtmc:StateNamesWrongSize'))
                    
            elseif any(ismissing(stateNames))
                
                    error(message('econ:dtmc:dtmc:MissingStateNames'))
                    
            else
                
                    mc.StateNames = stateNames;
                    
            end
            
        end % SET StateNames
        
        % Get NumStates
        function numStates = get.NumStates(mc)
            
            numStates =  size(mc.P,2);
            
        end % GET NumStates
        
    end % Methods
    
end % CLASSDEF