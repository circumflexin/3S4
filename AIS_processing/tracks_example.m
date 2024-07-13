deploy = "oo23_302a_pt"
load(append(deploy,"_dsfb.mat"))

%this is the track of the whale in lat/long
wpos = wtrack.poswh;
plot(wpos(:,2), wpos(:,1))

%this is the distance to fishing boat data:
disfish = wtrack.dsfb; %m

% colums are distances to each boat, rows are distances for each sample in
% the whale track 
plot(disfish) % so here each colour represents a different vessel 

% all the timeseries are matched by sample, rather than time. So if you
% want to know the distance to the nearest fishing vessel at a given time,
% for example to match it to an audit cue:
t_tailslap = datetime("29-Oct-2023 21:45:00");

% you first work out the difference
% to the nearest vessel for the whole track:
dnearest = min(disfish,[],2);
hold on 
plot(dnearest)

% then get the sample at the time you want, for example
twh = datetime(wtrack.twh,'ConvertFrom','datenum');% this is the time vector
% from the whaletrack:
twh(1:4)

[val,ind] = min(abs(twh-t_tailslap)); % value and index of nearest in time
if val < seconds(2) % check it's close enough in time to be relevant
    dist_atslap = dnearest(ind);
else
    dist_atslap = NaN;
end

%now you know how far the nearest boat was when the whale performed a given
%action
dist_atslap

