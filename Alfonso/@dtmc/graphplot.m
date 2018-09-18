function varargout = graphplot(varargin)
% GRAPHPLOT Plot Markov chain directed graph
%
% Syntax:
%
%   graphplot(mc)
%   graphplot(...,param,val,...)
%   h = graphplot(...)
%   h = graphplot(ax,...)
%
% Description:
%
%   GRAPHPLOT plots the directed graph of discrete-time Markov chain mc.
%   Nodes correspond to states of mc. Directed edges correspond to nonzero
%   transition probabilities in mc.P. Options highlight transition
%   probabilities, communicating classes, and class properties of
%   recurrence/transience and period. GRAPHPLOT optionally plots the
%   condensed digraph, with communicating classes as "supernodes".
%
% Input Arguments:
%
%   mc - Discrete-time Markov chain (@dtmc) object, with NumStates states.
%
%   ax - Axes object in which to plot. If unspecified, GRAPHPLOT plots to
%        the current axes (gca).
%
% Optional Input Parameter Name/Value Pairs:
%
%   NAME            VALUE
%
%   'LabelNodes'    Logical value indicating whether to label nodes with
%                   the names in mc.StateNames. If false, nodes are labeled
%                   with state numbers. The default value is true.
%
%   'ColorNodes'    Logical value indicating whether to color nodes with
%                   information on their communicating class. If true,
%                   nodes in the same communicating class have the same
%                   color. Solid markers represent nodes in recurrent
%                   classes, hollow markers represent nodes in transient
%                   classes. Periodicity of recurrent classes is displayed
%                   in a legend. The default value is false.
%
%   'LabelEdges'    Logical value indicating whether to label edges with
%                   the transition probabilities in mc.P (rounded to two
%                   decimal places). The default value is false.
%
%	'ColorEdges'    Logical value indicating whether to color edges to
%                   indicate transition probabilities. Color coding is
%                   summarized in a color bar. The default value is false.
%
%   'Condense'      Logical value indicating whether to condense the graph,
%                   with each communicating class represented by a single
%                   "supernode". Node labels list the component states of
%                   each supernode. An edge from supernode i to supernode j
%                   indicates a nonzero probability of transition from some
%                   state in i to some state in j. Transition probabilities
%                   between supernodes are not well-defined, and edge
%                   information is disabled. The default value is false.
%
% Output Argument:
%
%   h - Handle to the graph plot.
%
% Notes:
%
%   o To produce the directed graph as a MATLAB DIGRAPH object, and make
%     use of additional methods from that class, use G = digraph(mc.P).
%
%   o The 'LabelNodes' parameter allows lengthy node labels to be turned
%     off and replaced by node numbers, for readability. To remove node
%     labels completely, set h.NodeLabel = {};
%
%   o Node information on communicating classes and their properties is
%     computed by DTMC/CLASSIFY.
%
%   o To extract a communicating class in the graph, use DTMC/SUBCHAIN.
%
%   o The condensed graph is useful for identifying transient classes
%     (supernodes with positive outdegree) and recurrent classes
%     (supernodes with zero outdegreee), as well as visualizing the overall
%     structure of unichains (chains with a single recurrent class and any
%     transient classes that transition into it).
%
% Example:
%
%   P = [0.5 0.5 0 0; 0.5 0 0.5 0; 0 0 0 1; 0 0 1 0];
%   mc = dtmc(P);
%   graphplot(mc,'ColorNodes',true)
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
% See also DTMC/CLASSIFY

% Copyright 2017 The MathWorks, Inc.

% Preprocess varargin for target axes:

try
    
    [ax,args] = internal.econ.axesparser(varargin{:});
    
catch ME
    
    throw(ME)
    
end

% This function produces a single plot:

if ~isempty(ax) && ~isscalar(ax)
    
    error(message('econ:internal:econ:axesparser:InvalidParent'));
    
end

mc = args{1};
G = digraph(mc.P);
args = args(2:end);

% Parse inputs and set defaults:

parseObj = inputParser;

addParameter(parseObj,'LabelNodes',true,...
       @(x)validateattributes(x,{'numeric','logical'},{'scalar','binary'}))
   
addParameter(parseObj,'ColorNodes',false,...
       @(x)validateattributes(x,{'numeric','logical'},{'scalar','binary'}))

addParameter(parseObj,'LabelEdges',false,...
       @(x)validateattributes(x,{'numeric','logical'},{'scalar','binary'}))
   
addParameter(parseObj,'ColorEdges',false,...
       @(x)validateattributes(x,{'numeric','logical'},{'scalar','binary'}))
   
addParameter(parseObj,'Condense',false,...
       @(x)validateattributes(x,{'numeric','logical'},{'scalar','binary'}))

parse(parseObj,args{:});

nodeLabelFlag = parseObj.Results.LabelNodes;
nodeColorFlag = parseObj.Results.ColorNodes;
edgeLabelFlag = parseObj.Results.LabelEdges;
edgeColorFlag = parseObj.Results.ColorEdges;
condensedFlag = parseObj.Results.Condense;

% Plot to gca if no parent axes is specified:

if isempty(ax)

    ax = gca;

end

% Store NextPlot flag (and restore on cleanup):

next = get(ax,'NextPlot');
cleanupObj = onCleanup(@()set(ax,'NextPlot',next));

% Precompute state classification information, as necessary:

if nodeColorFlag

    [bins,~,ClassRecurrence,ClassPeriod] = classify(mc);
    numClasses = max(bins);
    NodeColors = lines(numClasses);
    
end

% Switch on 'Condense' to produce standard/condensed digraph:

if ~condensedFlag % Standard digraph
    
    % Create basic plot:

    basicPlot(G)

    % Modify plot according to parameter settings:

    if nodeLabelFlag

        hPlot.NodeLabel = cellstr(mc.StateNames);

    end

    if nodeColorFlag

        colorNodes

    end

    if edgeLabelFlag

        hPlot.EdgeLabel = round(G.Edges.Weight,2);

    end

    if edgeColorFlag

        hPlot.EdgeCData = G.Edges.Weight;
        colormap(ax,jet)
        caxis(ax,[0,1])
        hCB = colorbar(ax);
        hCB.Label.String = '{\bf Transition Probability}';

    end
    
else % Condensed digraph
    
    % Create basic plot:

    basicPlot(condensation(G))
    
  	% Modify plot according to parameter settings:
    
    if nodeLabelFlag && ~nodeColorFlag
    
        bins = classify(mc);
        numClasses = max(bins);
        
    end
    
    if nodeLabelFlag

        stateNames = cellstr(mc.StateNames);
        superNodeNames = cell(1,numClasses);

        for ii = 1:numClasses

            N = stateNames(bins == ii);
            s = strjoin(N,',');            
            superNodeNames{ii} = ['{',s,'}'];

        end

        hPlot.NodeLabel = superNodeNames;

    end
    
    if nodeColorFlag
        
        colorNodes
        
    end
    
    if edgeLabelFlag || edgeColorFlag
        
        warning(message('econ:dtmc:graphplot:CondensedEdgeInfo'))
        
    end

end

    %----------------------------------------------------------------------
    function basicPlot(graph) 
    % Create basic plot.
       
        hPlot = plot(ax,graph,...
                     'Marker','o','NodeColor','r','EdgeColor','b',...
                     'MarkerSize',8,'LineWidth',1,'ArrowSize',10);
        
        % Modify axes properties conditional on NextPlot flag:

        ax.Tag = 'GraphPlot';

        switch next
    
            case {'replace','replaceall'}
                
                % Turn off axes ticks:
        
                ax.XTick = [];
                ax.YTick = [];
                
            case {'replacechildren','add'}
        
                % Do not modify axes properties
    
        end
        
    end % BASICPLOT

    %----------------------------------------------------------------------
    function colorNodes 
    % Create markers with required properties and plot over BASICPLOT.
        
        X = hPlot.XData;
        Y = hPlot.YData;

        H = zeros(1,numClasses); % Class handles
        L = cell(1,numClasses);  % Class labels
        
        set(ax,'NextPlot','add');

        for i = 1:numClasses

            if condensedFlag
                
                H(i) = plot(ax,X(i),Y(i),...
                            'o','MarkerSize',8,'LineWidth',1);
                
            else
                
                bi = (bins == i);
                H(i) = plot(ax,X(bi),Y(bi),...
                            'o','MarkerSize',8,'LineWidth',1);
            
            end
            
            set(H(i),'MarkerEdgeColor',NodeColors(i,:));
            
            if ClassRecurrence(i)
                
                set(H(i),'MarkerFaceColor',NodeColors(i,:))
                
                if (ClassPeriod(i) == 1)
                    
                    L{i} = 'Aperiodic';
                    
                else
                    
                    L{i} = sprintf('Period = %u',ClassPeriod(i));
                    
                end
        
            else
                
                set(H(i),'MarkerFaceColor','w')
                
                L{i} = 'Transient';
                
            end

        end
        
        hLegend = legend(ax,H,L,'Location','Best');
        hLegend.Title.String = 'Classes';
        
    end % COLORNODES

    %----------------------------------------------------------------------

% Suppress assignment to ans:

nargoutchk(0,1)

if nargout > 0
    
    varargout = {hPlot};
    
end

end % GRAPH