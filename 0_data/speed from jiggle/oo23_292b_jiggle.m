% example simple implementation of TagJiggle.m & SpeedFromRMS.m

% Step 1- set up variables and calculate tag jiggle RMS amplitude for each axis

% load downsampled whale data with pitch, roll, p, fs, etc.
load('oo23_292bprh10.mat'); 

% load calibrated acceleration data (in tag frame).  Example units are in g but could be anything
load('oo23_292b_Adata_250Hz.mat'); % loads A and Afs, 250Hz

% plot the data for inspection
plott(p,fs,'r',Aw,fs)
plott(p,fs,'r',M,fs)

% The depth measurements before 1.203 hours of deployment is corrupt.
% We will discard the data prior to this point as this will affect the OCDR
% estimate and the jiggle speed. Also, A and M were not calibrated for this
% period due to the corrupt p.
start = round(1.203*60*60*fs);
startA = round(1.203*60*60*Afs);

p(1:start,:) = []; Aw(1:start,:) = []; Mw(1:start,:) = []; pitch(1:start,:) = []; roll(1:start,:) = [];
A(1:startA,:) = [];

% Remove the period of when the tag detached
endRec = round(19.96*60*60*fs);
endRecA = round(19.96*60*60*Afs);

p(endRec:end,:) = []; Aw(endRec:end,:) = []; Mw(endRec:end,:) = []; pitch(endRec:end,:) = []; roll(endRec:end,:) = [];
A(endRecA:end,:) = [];

% plot the data for inspection
plott(p,fs,'r',Aw,fs)
% OK

% Afs if the sample rate of the accelerometer data, fs is the (usually downsampled) sample rate of the tag data at which you will want the final speed values to match.
JX = TagJiggle(A(:,1),Afs,fs,[10 90],.5); %[10 90] and 0.5 are default choices, can also input [].  If Afs<180, only a high pass filter at 10 Hz is used.
JY = TagJiggle(A(:,2),Afs,fs,[10 90],.5);
JZ = TagJiggle(A(:,3),Afs,fs,[10 90],.5);
J = TagJiggle(A,Afs,fs,[10 90],.5);
% Remove one row
%JX(end,:) = []; JY(end,:) = []; JZ(end,:) = []; J(end,:) = [];

% if you've calculated flownoise RMS values, speedFromRMS can compare the speed from jiggle method to the speed from the flownoise method.
if exist('flownoise','var') && sum(isnan(flownoise)) ~= length(flownoise) 
    RMS = [JX JY JZ flownoise];
else
    RMS = [JX JY JZ J];
end

% Remove one row from RMS
%RMS(end,:) = [];

% Find where tag slip occurred
caldir = '/Volumes/Expansion/Jiggle method/cal'; % directory where cal files will be stored
settagpath('cal',caldir);
% Get the OTAB from DEPLOY
[CAL DEPLOY] = d3loadcal('oo23_292b');
% There is one large point slip at 49419
% WARNING - OTAB records the time from the VERY start of the recording,
% including the corrupt depth data
%slip =  23192*fs - start; Not enough data points, also only a minor slip
slip = 49419*fs - start;
% Check if this is reasonable
plott(p,fs,'r',M,fs) % Yes, there is a slip at 49419.
% Check if the slip variable captures this properly
figure(1);clf;
ax(1) = subplot(211);
plot(p); hold on; set(gca,'YDir','reverse'); ylabel('Depth (m)'); plot(slip, 0, 'ro');
ax(2) = subplot(212);
yyaxis left; plot(M); hold on; ylabel('Magnetism'); plot(49419*fs, 0, 'ro');
grid on
linkaxes(ax, 'x'); % Link the axes

tagslips = [slip; length(p)]; % creates a vector of indices indicating the end of periods when the tag was in different orientations
% see SpeedFromRMS for info on the following optional parameters.
binSize = 0.5;
filterSize = 0.5; 
minDepth = 10; 
minPitch = 45;
minSpeed = 0.5; 
minTime = 0.2;
tagon = ones(size(p));
% tagon is an index indicating data points that are on the animal.


%% Now everything is set up to run SpeedFromRMS.  On the first graph you can
% further threshold the parameters minDepth, minPitch and maxRoll by
% clicking on the colorbar for each axis.  For instance, click at 60 to set
% the minPitch to 60 degrees and watch how points disappear.  Since OCDR
% increases in accuracy with higher pitch, the goal is to use the highest
% pitch angle possible that still leaves enough points to create a nice
% exponential relationship between OCDR and jiggle RMS amplitude.  The
% other thresholds (roll and depth) are mostly useful if you have a lot of
% outliers near the surface or at high roll rate (uncommon).

% of the following output variables, the "speed" table is the most important one.  The rest of the outputs are for documenting the fit of the speed curve
[~,speed,sectionsendindex,fits,speedModels,modelsFit,speedThresh,multiModels] = SpeedFromRMS(RMS,fs,p,pitch,roll,[],tagslips,tagon,binSize,filterSize,minDepth,minPitch,minSpeed,minTime);

%% Check if the vertical speed from the method matches the jiggle speed
speed_Jiggle=table2array(speed(:,2));
OCDR=table2array(speed(:,1));

figure(10);clf;
ax(1) = subplot(211);
yyaxis left; plot(p); hold on; set(gca,'YDir','reverse'); ylabel('Depth (m)');
yyaxis right; plot(OCDR); hold on; ylabel('OCDR (m/s)'); ylim([0 5]); % Set the limits according to your desired range
ax(2) = subplot(212);
yyaxis left; plot(speed_Jiggle); hold on; ylabel('Jiggle speed (m/s)'); ylim([0 5]);
yyaxis right; plot(OCDR); hold on; ylabel('OCDR (m/s)'); ylim([0 5]); % Set the limits according to your desired range
grid on
linkaxes(ax, 'x'); % Link the axes

%% The following section is optional, but it can help organize the data into a couple of simple structures:
% 1) speed (a speed table with speed as well as prediction and confidence
% intervals).  speed.JJ will be the speed from tag jiggle in m/s.  
% 2) speedstats (a structure with information about the models used to
% create the speed curves)
% 3) JigRMS (a table with the raw (unadjusted) jiggle RMS values for each axis

speed.Properties.VariableNames{'RMS2'} = 'FN';
speed.Properties.VariableNames{'P68_2'} = 'FNP68';
speed.Properties.VariableNames{'P95_2'} = 'FNP95';
speed.Properties.VariableNames{'C95_2'} = 'FN95';
speed.Properties.VariableNames{'r2_2'} = 'FNr2';
fits.Properties.VariableNames{'RMS2r2'} = 'FNr2';

if ~exist('flownoise','var') || sum(isnan(flownoise)) == length(flownoise) % if there is no flownoise variable, get rid of those parts of the table.
    speed.FN = nan(size(speed.JJ));
    speed.FNP68 = []; speed.FNP95 = []; speed.FN95 = []; speed.FNr2 = [];
    try speedModels(:,2) = []; catch; end
    try ModelFits(:,2) = []; catch; end
end
speedstats = struct();
speedstats.Models = speedModels;
speedstats.ModelFits = modelsFit;
speedstats.r2used = fits;
speedstats.sections_end_index = sectionsendindex;
speedstats.info = 'Each row is the model for a tag orientation section.  The last row in Models is the regression on all data.  The r2 used is the r2 for each section (sometimes the whole data section was used if there was not enough data to get a good regression for an individual section';

for ii = 1:length(multiModels);
    speedstats.multiModels{ii} = multiModels{ii}.Coefficients;
end
speedstats.Thresh = speedThresh;
X = JX; Y = JY; Z = JZ; Mag = J;
JigRMS = table(X, Y, Z, Mag);

% Make a directory to save the images from the analysis
whaleID = "oo23_292b";
dir_path = ['SpeedPlots' filesep char(whaleID)];

% Create the directory if it doesn't exist
if ~exist(dir_path, 'dir')
    mkdir(dir_path);
end

for fig = [1 301:300+size(speedstats.r2used,1)]
    % Construct the file name with the appropriate file separator
    file_name = ['SpeedPlots' filesep char(whaleID) filesep 'fig' num2str(fig) '.png'];
    % Save the figure as a PNG file
    saveas(fig, file_name);
end

save([char(whaleID) 'speed.mat'],'speed','speedstats','JigRMS'); % or save the values within your prh file;

% check your values;
figure; ax =  plotyy(1:length(p),p,1:length(p),speed.JJ); set(ax(1),'ydir','rev');