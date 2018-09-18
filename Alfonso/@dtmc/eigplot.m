function varargout = eigplot(varargin)
% EIGPLOT Plot Markov chain eigenvalues
%
% Syntax:
%
%   eigplot(mc)
%   [eVals,h] = eigplot(mc)
%   [eVals,h] = eigplot(ax,mc)
%
% Description:
%
%   EIGPLOT plots eigenvalues of the transition matrix of discrete-time
%   Markov chain mc. The plot highlights the unit circle, the Perron-
%   Frobenius eigenvalue at (1,0), the circle of second largest eigenvalue
%   magnitude (SLEM), and the spectral gap between the two circles, which
%   determines the mixing time. See DTMC/ASYMPTOTICS.
%
% Input Argument:
%
%   mc - Discrete-time Markov chain (@dtmc) object.
%
%   ax - Axes object in which to plot. If unspecified, EIGPLOT plots to
%        the current axes (gca).
%
% Output Arguments:
%
%   eVals - Eigenvalues of mc.P, sorted by magnitude.
%
%   h - Vector of handles to plotted graphics objects.
%
% Notes:
%
%   o By the Perron-Frobenius Theorem [2], a chain with a single recurrent
%     communicating class (a unichain) has exactly one eigenvalue equal to
%     1 (the Perron-Frobenius eigenvalue), and an accompanying nonnegative
%     left eigenvector that normalizes to a unique stationary distribution.
%     All other eigenvalues have modulus less than or equal to 1. The
%     inequality is strict unless the recurrent class is periodic. When
%     there is periodicity of period k, there are k eigenvalues on the unit
%     circle at the k roots of unity.
%
%   o For an ergodic unichain, any initial distribution converges to the
%     stationary distribution at a rate determined by the second largest
%     eigenvalue modulus (SLEM), mu. The spectral gap, 1-mu, provides a
%     visual measure, with large gaps (smaller SLEM circles) producing
%     faster convergence. Rates are exponential, with a characteristic time
%     given by tMix = 1/log(1/mu). See DTMC/ASYMPTOTICS.
%
% Example:
%
%   % Faster mixing chain:
%   mc1 = mcmix(10,'Zeros',20);
%   eigplot(mc1)
% 
%   % Slower mixing chain:
%   mc2 = mcmix(10,'Zeros',80);
%   eigplot(mc2)
%
% References:
% 
%   [1] Gallager, R.G. Stochastic Processes: Theory for Applications.
%       Cambridge, UK: Cambridge University Press, 2013.
%
%   [2] Horn, R., and C. R. Johnson. Matrix Analysis. Cambridge, UK:
%       Cambridge University Press, 1985.
%
%   [3] Seneta, E. Non-negative Matrices and Markov Chains. New York, NY:
%       Springer-Verlag, 1981.
%
% See also DTMC/ASYMPTOTICS

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

% Compute eigenvalues:

P = full(mc.P);
numStates = mc.NumStates;

zeroTol = numStates*eps(1);

eVals = eig(P);
z = complex(eVals);

% Compute SLEM:

m = sort(abs(eVals),'descend');
rho = m(1);
SLEMIdx = find(abs(m-rho) > rho*zeroTol,1);
mu = m(SLEMIdx);
isSLEM = ~isempty(mu);

% Plot to gca if no parent axes is specified:

if isempty(ax)
    
    ax = gca;
    
end

% Store NextPlot flag (and restore on cleanup):

next = get(ax,'NextPlot');
cleanupObj = onCleanup(@()set(ax,'NextPlot',next));

% Plot eigenvalues:

hz = plot(ax,z,'b*','MarkerSize',10,'Tag','eVals');
set(ax,'NextPlot','add');

% Highlight Perron-Frobenius eigenvalue:

hpf = plot(ax,1,0,'b*','MarkerSize',10,'LineWidth',1.5,'Tag','PFeVal');

% Plot unit circle:

theta = 0:0.01:2*pi;
r1 = ones(size(theta));
[x1,y1] = pol2cart(theta,r1);
huc = plot(ax,x1,y1,'r-','LineWidth',2,'Tag','Unit Circle');

if isSLEM

    % Plot SLEM circle and spectral gap:

    r2 = mu*r1;
    [x2,y2] = pol2cart(theta,r2);
    x = [x1 x2(end:-1:1)];
    y = [y1 y2(end:-1:1)];
    
    hslem = plot(ax,x2,y2,'r--','LineWidth',1,...
                 'Tag','SLEM Circle');
    hsg = patch(ax,x,y,'r','FaceAlpha',0.2,'EdgeColor','none',...
                'Tag','Spectral Gap');
            
    [~,icons] = legend([hz,hsg],'Eigenvalues','Spectral Gap',...
                       'Location','NorthEastOutside');
    patchInLegend = findobj(icons,'type','patch');
    set(patchInLegend,'FaceAlpha',0.2)

else

    legend(hz,'Eigenvalues','Location','NorthEastOutside')

end

% Modify axes properties conditional on NextPlot flag:

ax.Tag = 'EigPlot';

switch next
    
    case {'replace','replaceall'}
    
        grid(ax,'on')
        axis(ax,'square')
        axis(ax,'tight')
        
    case {'replacechildren','add'}
        
        % Do not modify axes properties
    
end

% Return "plot object":

h = [hz;hpf;huc];

if isSLEM
    
    h = [h;hslem;hsg];
    
end

% Suppress assignment to ans:

nargoutchk(0,2)

if nargout > 0

    varargout = {eVals,h};
    
end