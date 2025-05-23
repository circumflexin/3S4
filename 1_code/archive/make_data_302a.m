
% config
dis_thresh = 30 %km
t_thresh = 30 %sec
df = 30
sog_thresh = 30
wtrack_dir = "D:\Analysis\3S4\0_data\Pseuedotracks"
btrack_dir = "D:\Analysis\3S4\0_data\AIS data\Individual vessel AIS tracks_Oct9 - Nov30 vessels with IMO"
fs_dif = 10
ptracks = {'oo23_302a_pt'}
diagn = true;

AISfiles = dir(fullfile(btrack_dir,'*.csv'))

tag = 'oo23_302a';
ptrackfolder = 'D:\Analysis\3S4\AIS_processing\outputs\';
btrack_dir = "D:\Raw\3S4\AIS data\Individual vessel AIS tracks_Oct9 - Nov30 vessels with IMO";
load([ptrackfolder,tag,'_pt_dsfb.mat']) % this includes time vectors twh and tgps
load([ptrackfolder,tag,'_pt_relAIS.mat'])

nx = 10; % 1 Hz data thinned
twh = datetime(wtrack.twh(1:nx:end), "ConvertFrom", 'datenum');
poswh = wtrack.poswh;

k = 1;

dsfb = nan(length(twh),length(rel));
fb_lat = dsfb;
fb_lon = dsfb;
ellipsoid = almanac('earth','wgs84','meters');


for j = 1:length(rel)
    j
    boat = readtable(fullfile(btrack_dir,rel(j)));
    boat = boat(boat.sog<sog_thresh,:);
    bool = boat.date_time_utc > twh(1) & boat.date_time_utc < twh(length(twh));
    btrack2 = boat(bool,:);
    bt = btrack2.date_time_utc;
    o1 =  twh>min(btrack2.date_time_utc);
    o2 = twh<max(btrack2.date_time_utc);
    o3 = o1+o2;
    starts = find(o3 ==2,1, 'first');
    ends = find(o3 ==2, 1, 'last');
    for i = starts:ends
        twi = twh(i);
        pwi = poswh(ceil(i/fs_dif),:);
        [val,ind] = min(abs(twh(i)-bt));
        if val < seconds(t_thresh)
            pfb = [btrack2.lat(ind),btrack2.lon(ind)];
            [dsfb(i,j),tmp] = distance(pwi, pfb, ellipsoid);
            fb_lon(i,j) = btrack2.lon(ind);
            fb_lat(i,j) = btrack2.lat(ind);
        end
    end
end


temp.dsfb = dsfb;
temp.fb_lon = fb_lon;
temp.fb_lat = fb_lat;

