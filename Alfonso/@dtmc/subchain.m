function sc = subchain(mc,states)
% SUBCHAIN Extract Markov subchain
%
% Syntax:
%
%   sc = subchain(mc,states)
%
% Description:
%
%   SUBCHAIN extracts subchain sc from discrete-time Markov chain mc.
%   The subchain contains specified states and all states reachable from
%   specified states.
%
% Input Arguments:
%
%   mc - Discrete-time Markov chain (@dtmc) object.
%
%   states - States to include in the subchain. Input must be valid state
%        numbers (vector of positive integers) or state names (string
%        vector, character vector, or cell vector of character vectors).
%
% Output Argument:
%
%   sc - Discrete-time Markov chain (@dtmc) object. Subchain of mc
%        containing input states and all states reachable from input
%        states. State names are inherited from mc.
%
% Notes:
%
%   o State j is reachable from state i if there is a nonzero probability
%     of moving from i to j in a finite number of steps. SUBCHAIN
%     determines reachability by forming the transitive closure of the
%     associated digraph, then enumerating one-step transitions.
%
%   o Subchains are closed under reachability to ensure that the transition
%     matrix of sc remains stochastic (rows sum to 1), with transition
%     probabilities identical to those in mc.P.
%
%   o Specifying a state in a recurrent communicating class extracts the
%     entire communicating class. Specifying a state in a transient
%     communicating class extracts the transient class and all classes
%     reachable from the transient class. To extract a unichain, specify a
%     state in each component transient class. See DTMC/CLASSIFY.
%
% Example:
%
% % Extract recurrent class from a unichain:
%
%   P = [0 1 0 0; 0.5 0 0.5 0; 0 0 0.5 0.5; 0 0 0.5 0.5];
%   mc = dtmc(P);
%   x = asymptotics(mc)
%   graphplot(mc,'ColorNodes',true)
%   sc = subchain(mc,3)
%   graphplot(sc,'ColorNodes',true)
%
% References:
% 
%   [1] Gallager, R.G. Stochastic Processes: Theory for Applications.
%       Cambridge, UK: Cambridge University Press, 2013.
%
%   [2] Horn, R., and C. R. Johnson. Matrix Analysis. Cambridge, UK:
%       Cambridge University Press, 1985.
%
% See also DTMC/CLASSIFY

% Copyright 2017 The MathWorks, Inc.

P = mc.P;
numStates = mc.NumStates;
stateNames = mc.StateNames;
stateNums = 1:numStates;

% Check start states:

numsFlag = isnumeric(states);
nameFlag = (isstring(states) || ischar(states) || iscell(states));

if (nargin < 2) || isempty(states)
    
	error(message('econ:dtmc:subchain:NoStartStates'))
   
elseif numsFlag && ...
       (~isvector(states) || ...
       any(mod(states,1) ~= 0) || ...
       any(states < 1) || any(states > numStates))
    
	error(message('econ:dtmc:subchain:InvalidStateNumbers'))
    
elseif nameFlag && ...
       (~isvector(states) || ...
       any(~ismember(states,stateNames)))

	error(message('econ:dtmc:subchain:InvalidStateNames'))
    
end

% Convert state names to state numbers:

if nameFlag
    
    states = stateNums(ismember(stateNames,states));
    
end

% Find reachable states:

G = digraph(mc.P);
T = transclosure(G);

for i = 1:length(states)
    
    states = union(states,successors(T,states(i)));
    
end

subStates = ismember(stateNums,states);
subNames = stateNames(subStates);
sc = dtmc(P(subStates,subStates),'StateNames',subNames);