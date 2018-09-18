function varargout = distplot(varargin)
% DISTPLOT Plot Markov chain redistributions
%
% Syntax:
%
%   distplot(mc,X)
%   distplot(...,param,val,...)
%   h = distplot(...)
%   h = distplot(ax,...)
%
% Description:
%
%   DISTPLOT plots data X on the evolution of a distribution of states in
%   discrete-time Markov chain mc.
%
% Input Arguments:
%
%   mc - Discrete-time Markov chain (@dtmc) object, with NumStates states.
%
%   X - Array of state probabilities, of size (1+numSteps)-by-NumStates,
%       as produced by DTMC/REDISTRIBUTE. The first row is an initial
%       distribution. Subsequent rows are the redistributions at each step.
%       Rows are normalized by their sum before plotting.
%
%   ax - Axes object in which to plot. If unspecified, DISTPLOT plots to
%        the current axes (gca).
%
% Optional Input Parameter Name/Value Pairs:
%
%   NAME        VALUE
%
%   'Type'      Type of plot produced by DISTPLOT. Values are:
%
%               o 'Evolution'   Evolution of the initial distribution. The
%                               plot is a (1+NumSteps)-by-NumStates image.
%                               Row i displays the redistribution at step
%                               i. This is the default.
%
%               o 'Histogram'   Animated histogram of the redistributions.
%                               The vertical axis displays probability
%                               mass, the horizontal axis displays states.
%                               Animation progress is controlled by the
%                               'FrameRate' parameter.
%
%               o 'Graph'       Animated graph of the redistributions.
%                               Nodes are colored by their probability mass
%                               at each step. Animation progress is
%                               controlled by the 'FrameRate' parameter.
%
%   'FrameRate' Positive scalar specifying the length of discrete time
%               steps, in seconds, for animated plots. The default pauses
%               at each time step, and proceeds by pressing the space bar.
%
% Output Argument:
%
%   h - Handle to the distribution plot.
%
% Example:
%
%   % 10-step evolution of a uniform distribution of states:
%
%   mc = mcmix(4,'Zeros',8);
%   X = redistribute(mc,10);
%   distplot(mc,X)
%
% See also DTMC/REDISTRIBUTE, DTMC/SIMPLOT

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
numStates = mc.NumStates;
args = args(2:end);

% Parse inputs and set defaults:

parseObj = inputParser;

addRequired(parseObj,'X',...
        @(x)validateattributes(x,{'numeric'},...
        {'nonempty','nonnegative','2d','ncols',numStates,'<=',1}))
   
addParameter(parseObj,'Type','Evolution',...
        @(x)validateattributes(x,{'char','string'},...
        {'vector'}))
   
addParameter(parseObj,'FrameRate',Inf,...
       @(x)validateattributes(x,{'numeric'},... 
       {'scalar','positive'}))
   
parse(parseObj,args{:});

X = parseObj.Results.X;
plotType = validatestring(parseObj.Results.Type,...
           {'Evolution','Histogram','Graph'});
frameRate = parseObj.Results.FrameRate;

% Normalize rows:
s = sum(X,2);
if any(s == 0)
    
    error(message('econ:dtmc:distplot:ZeroRowDistribution'))
    
else

    X = X./s;
    
end

numSteps = size(X,1)-1;

% Plot to gca if no parent axes is specified:

if isempty(ax)
    
    ax = gca;
    
end

% Store NextPlot flag (and restore on cleanup):

next = get(ax,'NextPlot');
cleanupObj = onCleanup(@()set(ax,'NextPlot',next));

% Create plot:

switch plotType
        
    case 'Evolution'
        
        hPlot = imagesc(ax,X);
        hPlot.Tag = 'Evolution';
        
        % Modify axes properties conditional on NextPlot flag:

        ax.Tag = 'DistPlot';

        switch next
    
            case {'replace','replaceall'}
    
                ax.XTick = 1:numStates;
                ax.XTickLabel = mc.StateNames;
                ax.YTick = 1:numSteps+1;
                ax.YTickLabel = string(0:numSteps);

                colormap(ax,jet)
                hCB = colorbar(ax);
                hCB.Label.String = 'Probability';

                xlabel(ax,'State')
                ylabel(ax,'Simulation Step')
                title(ax,'Distribution of States')
        
            case {'replacechildren','add'}

                % Do not modify axes properties

        end
        
    case 'Histogram'
        
        hPlot = histogram(ax,'Categories',cellstr(mc.StateNames),...
                          'BinCounts',X(1,:));
        hPlot.Tag = 'Histogram';
        
        % Modify axes properties conditional on NextPlot flag:

        ax.Tag = 'DistPlot';

        switch next
    
            case {'replace','replaceall'}
                
                ylim(ax,[0 1.1*max(X(:))])
        
                grid(ax,'on')
                xlabel(ax,'State')
                ylabel(ax,'Probability')
                title(ax,'Distribution of States')
        
            case {'replacechildren','add'}

                % Do not modify axes properties

        end
        
        for i = 1:numSteps+1
            
            if i > 1

                hPlot.BinCounts = X(i,:);
                
            end
            
            legend(ax,hPlot,['Step ',num2str(i-1)],'Location','NorthEast')

            aniPause(frameRate)

        end
         
	case 'Graph'
        
        hPlot = graphplot(ax,mc,'ColorEdges',true);  
        hPlot.Tag = 'Graph';
        
        set(ax,'NextPlot','add');
        
        nodeX = hPlot.XData;
        nodeY = hPlot.YData;
            
        cmap = ax.Parent.Colormap;
        numColors = size(cmap,1);
        hCB = colorbar(ax);
        hCB.Label.String = '{\bf Probability}';

        for i = 1:numSteps+1

            for j = 1:numStates

                if X(i,j) == 0
                    
                    cIdx = 1;
                    
                else
                    
                    cIdx = ceil(numColors*X(i,j));
                    
                end

                plot(ax,nodeX(j),nodeY(j),'o',...
                     'MarkerSize',10,...
                     'MarkerEdgeColor',cmap(cIdx,:),...
                     'MarkerFaceColor',cmap(cIdx,:));

            end
            
            legend(ax,hPlot,['Step ',num2str(i-1)],'Location','NorthEast')

            aniPause(frameRate)

        end
        
end

% Suppress assignment to ans:

nargoutchk(0,1)

if nargout > 0
    
    varargout = {hPlot};
    
end

%--------------------------------------------------------------------------
function aniPause(frameRate)

if isinf(frameRate)
    
    pause
    
else
    
    pause(frameRate)
    
end