function [bins,ClassStates,ClassRecurrence,ClassPeriod] = classify(mc)
% CLASSIFY Classify Markov chain states
%
% Syntax:
%
%   [bins,ClassStates,ClassRecurrence,ClassPeriod] = classify(mc)
%
% Description:
%
%   CLASSIFY partitions states of discrete-time Markov chain mc into
%   disjoint communicating classes. Optional output arguments provide
%   information on transience/recurrence and periodicity.
%
% Input Argument:
%
%   mc - Discrete-time Markov chain (@dtmc) object, with NumStates states.
%
% Output Arguments:
%
%   bins - Vector of length NumStates identifying the communicating class
%       to which each state belongs. Bin values range from 1 to NumClasses.
%
%   ClassStates - Cell vector of length NumClasses giving state names in
%       each class. State names are those specified in mc.StateNames.
%
%   ClassRecurrence - Logical vector of length NumClasses indicating
%       whether each class is recurrent (true) or transient (false).
%
%   ClassPeriod - Vector of length NumClasses giving the period of each
%       class. Aperiodic classes have period 1.
%
%   The order of classes in ClassStates, ClassRecurrence, and ClassPeriod
%   corresponds to the class numbers assigned in the bins output.
%
% Notes:
%
%   o Communicating classes of a Markov chain are the equivalence classes
%     formed under the relation of mutual reachability. That is, two states
%     are in the same class if and only if each is reachable from the other
%     with nonzero probability in a finite number of steps.
%
%   o Communicating classes are equivalent to strongly connected components
%     in the associated digraph [2]. See DTMC/GRAPHPLOT.
%
%   o Irreducible chains consist of a single communicating class. Unichains
%     consist of a single recurrent class and any transient classes that
%     transition to the recurrent class.
%
%   o CLASSIFY determines recurrence/transience from the outdegree of the
%     "supernode" associated with each communicating class in the condensed
%     digraph [1]. Outdegree 0 corresponds to recurrence; outdegree greater
%     than 0 corresponds to transience. See DTMC/GRAPHPLOT.
%
%   o CLASSIFY determines periodicity using a breadth-first search of
%     cycles in the associated digraph, as in [3]. Class period is the
%     greatest common divisor of the lengths of all cycles originating
%     at any state in the class.
%
% Example:
%
%   P = [0.5 0.5 0 0; 0.5 0.4 0.1 0; 0 0 0 1; 0 0 1 0];
%   mc = dtmc(P);
%   graphplot(mc,'ColorNodes',true)
%   [bins,ClassStates,ClassRecurrence,ClassPeriod] = classify(mc)
%
% References:
% 
%   [1] Gallager, R.G. Stochastic Processes: Theory for Applications.
%       Cambridge, UK: Cambridge University Press, 2013.
%
%   [2] Horn, R., and C. R. Johnson. Matrix Analysis. Cambridge, UK:
%       Cambridge University Press, 1985.
%
%   [3] Jarvis, J. P., and D. R. Shier. Graph-Theoretic Analysis of Finite
%       Markov Chains. In Applied Mathematical Modeling: A
%       Multidisciplinary Approach. Boca Raton: CRC Press, 2000.
%
% See also DTMC/GRAPHPLOT, DTMC/SUBCHAIN, DTMC/ISREDUCIBLE, DTMC/ISERGODIC

% Copyright 2017 The MathWorks, Inc. 

P = mc.P;
numStates = mc.NumStates;
stateNames = mc.StateNames;
stateNums = 1:numStates;

G = digraph(P);

% Classify states:

bins = conncomp(G,'Type','strong');
numClasses = max(bins);

ClassStates = cell(1,numClasses);
for i = 1:numClasses
    
    ClassStates{i} = stateNames(bins == i);
    
end

% Determine class recurrence:

ClassRecurrence = (outdegree(condensation(G)) == 0)';

% Determine class periodicity:

ClassPeriod = ones(1,numClasses);

for i = 1:numClasses
    
    if ~ClassRecurrence(i) % Class is transient
        
        p = 1; 
        
    else % Implement search in [3]
        
        % Extract recurrent class:
        
        g = subgraph(G,stateNums(bins == i));
        
        % Perform breadth-first search:
        
        events = {'edgetonew','edgetodiscovered','edgetofinished'};
        t = bfsearch(g,1,events);
        
        % Identify tree and non-tree edges:
        
        treeEdges = t.Edge(t.Event == 'edgetonew',:);
        nonTreeEdges = t.Edge(t.Event ~= 'edgetonew',:);
        
        % Compute node levels:

        level = zeros(1,numnodes(g));
        for j = 1:size(treeEdges,1)
            
            level(treeEdges(j,2)) = level(treeEdges(j,1)) + 1;
            
        end
        
        % Compute non-tree edge values:

        value = level(nonTreeEdges(:,1)) - level(nonTreeEdges(:,2)) + 1;
        
        % Compute period:
        
        p = 0; % gcd(p,0) = p

        for k = 1:size(nonTreeEdges,1)
    
            p = gcd(p,value(k));
            if p == 1
                
                break % gcd(1,val) = 1
                
            end
        
        end
        
    end

    ClassPeriod(i) = p;    
    
end