function varargout = simplot(varargin)
% SIMPLOT Plot Markov chain simulations
%
% Syntax:
%
%   simplot(mc,X)
%   simplot(...,param,val,...)
%   h = simplot(...)
%   h = simplot(ax,...)
%
% Description:
%
%   SIMPLOT plots data X on random walks through sequences of states in
%   discrete-time Markov chain mc.
%
% Input Arguments:
%
%   mc - Discrete-time Markov chain (@dtmc) object, with NumStates states.
%
%   X - Array of simulated data, of size (1+numSteps)-by-numSims, as
%       produced by DTMC/SIMULATE. The first row contains initial states.
%       Columns are state numbers representing random walks from the
%       corresponding initial state.
%
%   ax - Axes object in which to plot. If unspecified, SIMPLOT plots to
%        the current axes (gca).
%
% Optional Input Parameter Name/Value Pairs:
%
%   NAME        VALUE
%
%   'Type'      Type of plot produced by SIMPLOT. Values are:
%
%               o 'States'      States reached by simulations in X. The
%                               plot is a (1+numSteps)-by-NumStates image.
%                               Row i displays the proportion of walks in
%                               each state at step i. This is the default.
%
%               o 'Transitions' Transitions realized by simulations in X.
%                               The plot is a NumStates-by-NumStates image.
%                               Element (i,j) displays the proportion of
%                               transitions from state i to state j in all
%                               simulations. The plot is an empirical
%                               estimate of the transition matrix mc.P.
%
%               o 'Graph'       Animated graph of state counts (node size)
%                               and state flows (edge width) at each step.
%                               Animation progress is controlled by the
%                               'FrameRate' parameter.
%
%   'FrameRate' Positive scalar specifying the length of discrete time
%               steps, in seconds, for animated plots. The default pauses
%               at each time step, and proceeds by pressing the space bar.
%
% Output Argument:
%
%   h - Handle to the simulation plot.
%
% Notes:
%
%   o To compare a 'Transitions' plot with the transition matrix, use:
%
%     figure
%     imagesc(mc.P)
%     colormap(jet)
%     axis square
%
% Example:
%
%   mc = mcmix(4,'Zeros',8);
% 
%   % Run 3 simulations of length 10 from each state:
% 
%   X = simulate(mc,10,'X0',3*ones(1,4));
%   simplot(mc,X)
%
% See also DTMC/SIMULATE, DTMC/DISTPLOT

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
        {'nonempty','2d','positive','integer','<=',numStates}))
   
addParameter(parseObj,'Type','States',...
        @(x)validateattributes(x,{'char','string'},...
        {'vector'}))
   
addParameter(parseObj,'FrameRate',Inf,...
       @(x)validateattributes(x,{'numeric'},... 
       {'scalar','positive'}))
   
parse(parseObj,args{:});

X = parseObj.Results.X;
plotType = validatestring(parseObj.Results.Type,...
           {'States','Transitions','Graph'});
frameRate = parseObj.Results.FrameRate;

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
    
    case 'States'
                  
        S = stateCounts(X,numStates);
        S = S./sum(S,2);

        hPlot = imagesc(ax,S);
        hPlot.Tag = 'States';
        
        % Modify axes properties conditional on NextPlot flag:

        ax.Tag = 'SimPlot';

        switch next
    
            case {'replace','replaceall'}
            
                ax.XTick = 1:numStates;
                ax.XTickLabel = mc.StateNames;

                ax.YTick = 1:(numSteps+1);
                ax.YTickLabel = string(0:numSteps);

                xlabel(ax,'State')
                ylabel(ax,'Simulation Step')

                colormap(ax,jet) 
                hCB = colorbar(ax);
                hCB.Label.String = '{\bf Proportion of Simulations}';

                title(ax,'States Reached By Simulations')
        
            case {'replacechildren','add'}

                % Do not modify axes properties

        end
            
    case 'Transitions'

        T = transitionCounts(X,numStates);
        s = sum(T,2);
        for n = 1:numStates
            
            if s(n) ~= 0
                
                T(n,:) = T(n,:)/s(n);
                
            end
            
        end

        hPlot = imagesc(ax,T);        
        hPlot.Tag = 'Transitions';
        
        % Modify axes properties conditional on NextPlot flag:

        ax.Tag = 'SimPlot';

        switch next
    
            case {'replace','replaceall'}
            
                ax.XTick = 1:numStates;
                ax.XTickLabel = mc.StateNames;
                ax.YTick = 1:numStates;
                ax.YTickLabel = mc.StateNames;

                axis(ax,'square')

                colormap(ax,jet) 
                hCB = colorbar(ax);
                hCB.Label.String = '{\bf Row Proportions}';
                
                title(ax,'Transitions Realized By Simulations')

            case {'replacechildren','add'}

                % Do not modify axes properties

        end
                   
    case 'Graph'
        
        hPlot = graphplot(ax,mc,'ColorEdges',true);
        hPlot.MarkerSize = 1;
        hPlot.Tag = 'Graph';
        
        set(ax,'NextPlot','add');
                        
        nodeX = hPlot.XData;
        nodeY = hPlot.YData;
        
        S = stateCounts(X,numStates);
        SMax = max(S(:));
        
        T = zeros(numStates,numStates,numSteps+1);
        for i = 2:numSteps+1
            
            Xi = X((i-1):i,:);
            Ti = transitionCounts(Xi,numStates);
            T(:,:,i) = Ti/sum(Ti(:));
            
        end
        TMax = max(T(:));
                
        nodeSizeMax = 20;
        edgeWidthMax = 10;
            
        for i = 1:numSteps+1
            
            hNodes = zeros(1,numStates);

            for j = 1:numStates

                hNodes(j) = plot(ax,nodeX(j),nodeY(j),'ko',...
                                 'MarkerFaceColor',[0.8 0.8 0.8],...
                                 'MarkerSize',...
                                 1+(nodeSizeMax-1)*(S(i,j)/SMax));
                      
            end
            
            if i > 1
                
                T0 = T(:,:,i-1);
                T1 = T(:,:,i);
                    
                for m = 1:numStates
                    
                    for n = 1:numStates
                        
                        if T0(m,n) > 0
                            
                            highlight(hPlot,m,n,'LineWidth',1)
                            
                        end
                        
                        if T1(m,n) > 0
                            
                            highlight(hPlot,m,n,'LineWidth',...
                                      1+(edgeWidthMax-1)*(T1(m,n)/TMax))
                                  
                        end
                        
                    end
                    
                end
                
            end
            
            legend(ax,hPlot,['Step ',num2str(i-1)],'Location','NorthEast')
                     
           	aniPause(frameRate)
            
           	if i < (numSteps+1)
                
            	delete(hNodes)
                
          	end
            
        end

end

% Suppress assignment to ans:

nargoutchk(0,1)

if nargout > 0
    
    varargout = {hPlot};
    
end

%--------------------------------------------------------------------------
function S = stateCounts(X,numStates)

numSteps = size(X,1)-1;
S = zeros(numSteps+1,numStates);

for i = 1:numSteps+1
    
    for j = 1:numStates
        
        S(i,j) = sum(X(i,:) == j);
        
    end
    
end

%--------------------------------------------------------------------------
function T = transitionCounts(X,numStates)

T = zeros(numStates);

numSteps = size(X,1)-1;
numSims = size(X,2);
        
for j = 1:numSims
    
    for i = 1:numSteps
        
        T(X(i,j),X(i+1,j)) = T(X(i,j),X(i+1,j))+1;
        
    end
    
end

%--------------------------------------------------------------------------
function aniPause(frameRate)

if isinf(frameRate)
    
    pause
    
else
    
    pause(frameRate)
    
end