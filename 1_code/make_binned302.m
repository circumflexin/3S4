
tag = 'oo23_302a'
tstart = datenum('29-Oct-2023 17:13:32');
load(['dsfb\',tag,'_pt_dsfb.mat']) % this includes time vectors twh and tgps
load([tag,'aud.mat'])
tcutoff = 30

astart = taudit(1)
aend = taudit(end)

%checks
datestr(taudit(1));
astart_e = (astart-tstart)*60*60*24; %audit start, time elapsed
aend_e = (aend-tstart)*60*60*24; %audit end, time elapsed

bins = [astart_e:5*60:aend_e]; %binstart in seconds
binmids = bins+2.5*60; %bin midpoint in seconds
binend = bins+5*60; %bin end in seconds

twh_e = (wtrack.twh-tstart)*60*60*24;
taudit_e = (taudit-tstart)*60*60*24;

mindist = min(wtrack.dsfb,[],2);
bin_dsfb = nan(length(bins),1);
bin_hasS3 = nan(length(bins),1);
bin_durS3 = nan(length(bins),1);

cue = "SS 3"; % the cue we're interested in 

for bin = 1:length(bins)
    [val,ind] = min(abs(binmids(bin)-twh_e)); % find closest time point in the distance to fishing vessel timeseries
    if val < tcutoff % check it's close enough to be relevant
        bin_dsfb(bin) = mindist(ind);
    end
    ind2 = find(taudit_e > bins(bin) & taudit_e < binend(bin)); % find the audit cues within the bin
    aud_bin = string(code(ind2));
    bin_hasS3(bin) = any(aud_bin == cue); % check if it's the cue we're interested in
    durs = duraudit(ind2); % get the durations within the bin
    bin_durS3(bin) = sum(durs(aud_bin == cue)); % sum the durations of the cue of interest
    %
end

% make table
tab = table(bins',binmids', binend', bin_dsfb,bin_hasS3,bin_durS3, 'Variablenames', ["Binstart","Binmid","Binend", "Bin_distfish", "Bin_hasS3", "Bin_durS3"])
writetable(tab, append(tag, "_binned.csv"))