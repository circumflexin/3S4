% Script to generate the audit mat file from Audition-generated txt files.
% This is for the detailed audits of whales sounds, not manmade noise.
% Uses a DTAG cal file to extract WAV file durations.
clear all;

tag = 'oo23_299a';

% Load the cal file (NOTE: cal file for oo23_292b is preliminary!)
settagpath('cal','D:\Analysis\3S4\0_data\cal')
[CAL,D] = d3loadcal(tag);

% Loop over each annotation file to extract data
taudit=[];
duraudit=[];
code=cell(0);
d = dir('D:\Analysis\3S4\0_data\Audits\draft_audits\oo23_299a\*.txt');
for i=1:length(d)
    tstart = datenum(D.SCUES.TIME(strcmp(D.FN, d(i).name(1:(end-15))),:)); % UTC start time of annotated WAV file 
    data = readtable([d(i).folder,'\',d(i).name]);  % read data
    taudit = [taudit; tstart + (data{:,1})/86400];  % annotation start time
    duraudit = [duraudit; data{:,2} - data{:,1}];   % annotation duration in seconds 
    code = [code; data{:,3}];                       % annotation codes as cell array
end

%% Save this uncorrected version
save([tag,'aud.mat'],'taudit','duraudit','code')


%% Check data
code1 = unique(code) % unique labels used
for i=1:length(code1)
    n(i)=sum(strcmp(code,code1(i))); % and how often
end

% Labels used more often
code1red = code1(n>3)
nred = n(n>3);

% Plot counts for labels used more often
subplot(2,2,[1 3]); plot(nred,1:length(nred),'o')
set(gca,'YTick',0:length(nred),'YTickLabel',code1red)
xlabel('Number of times used'); ylabel('Annotation label')

% Plot UTC time vs index and audit duration
subplot(2,2,2); plot(taudit,1:length(taudit)); datetick('x'); 
xlabel('Time (UTC)'); ylabel('Index no.')
subplot(2,2,4); plot(taudit,duraudit); datetick('x'); 
xlabel('Time (UTC)'); ylabel('Duration (s)')

% Keep for plots:
% SS: WHISTLE and SS
% CS: clicking
% TAILSLAP
% Only keep qualities 2, 3 and 12 (period w both 1s and 2s)
% Exclude LFS for now and BR
% Correct the many spelling mistakes later
