% Script to generate an audit mat file from Audition-generated txt files.
% This is for the audits of manmade noise (boat noise, echosounders), not whale sounds.
% Uses a DTAG cal file to extract WAV file durations.
clear all;

tag = 'oo23_292b';

% Load the cal file (NOTE: cal file for oo23_292b is preliminary!)
settagpath('cal','C:\--- 3S-PROJECT ---\3S-2023\Plots for science meeting\cal')
[CAL,D] = d3loadcal(tag);

% Loop over each annotation file to extract data
taudit_noise=[];
code=cell(0);
d = dir('C:\--- 3S-PROJECT ---\3S-2023\Plots for science meeting\noise_audits\oo23_292b\*.txt');
for i=1:length(d)
    tstart = datenum(D.SCUES.TIME(strcmp(D.FN, d(i).name(1:(end-10))),:)); % UTC start time of annotated WAV file 
    data = readtable([d(i).folder,'\',d(i).name]);  % read data
    taudit_noise = [taudit_noise; tstart + (data{:,1})/86400];  % annotation start time
    code = [code; data{:,3}];                       % annotation codes as cell array
end

% Turn codes in logical vectors indicating change of noise condition
ncodes = size(code,1);
inoise = nan(ncodes,5);
for i=1:ncodes
    inoise(i,1) = logical(str2double(code{i}(1))); % vessel noise
    inoise(i,2) = logical(str2double(code{i}(3))); % 10 kHz band
    inoise(i,3) = logical(str2double(code{i}(4))); % 20 kHz band
    inoise(i,4) = logical(str2double(code{i}(5))); % 40 kHz band
    inoise(i,5) = logical(str2double(code{i}(6))); % 80 kHz band
end


%% Save 
% save([tag,'aud_noise.mat'],'taudit_noise','inoise')


%% Check data
code1 = unique(code)' % unique labels used
for i=1:length(code1)
    n(i)=sum(strcmp(code,code1(i))); % and how often
end
n
