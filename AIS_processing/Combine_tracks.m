
% config
dis_thresh = 5 %km
t_thresh = 30 %sec
wtrack_dir = "D:\Raw\3S4\Pseuedotracks"
btrack_dir = "D:\Raw\3S4\AIS data\Individual vessel AIS tracks_Oct9 - Oct31 vessels with IMO"
df = 30

trackfiles = dir(fullfile(btrack_dir,'*.csv'))
trackn = 'oo23_302a_pt.mat'
temp = open(fullfile(wtrack_dir,trackn))
poswh = temp.poswh;
twh = temp.twh
datestr(twh(1:4))
twht = datetime(twh,'ConvertFrom','datenum');

rel = strings()

ellipsoid = almanac('earth','wgs84','meters');

n = 1
% get rid of files which have no overlap or are never closer than the
% thresh or are not fihing boats
for f = 1:length(trackfiles)
   ais_file=trackfiles(f).name
   boat = readtable(fullfile(btrack_dir,ais_file));
   x1 = string(boat.validated_shiptype{1}) ~= "Fishing Vessel";
   x2 = ~any(boat.date_time_utc > twht(1) & boat.date_time_utc < twht(length(twht)));
   if(~any([x1,x2]))
       posf = [boat.lat, boat.lon];
       for i=1:length(poswh)
       mind = min(distance(poswh(i,:), posf, ellipsoid));
       if mind < dis_thresh*1000
               rel(n,1) = string(ais_file)
               n = n +1
               break
       end
       end
   end
end


dswh = nan(length(twh),1);
i = 1
temp = rel(i)
twht = datetime(twh,'ConvertFrom','datenum');

btrack = readtable(fullfile(btrack_dir,temp));
bool = btrack.date_time_utc > twht(1) & btrack.date_time_utc < twht(length(twht))
btrack2 = btrack(bool,:);
bt = btrack2.date_time_utc

o1 =  twht>btrack2.date_time_utc(1)
o2 = twht<btrack2.date_time_utc(length(btrack2.date_time_utc))
o3 = o1+o2
starts = min(find(o3 ==2))
ends = max(find(o3 ==2))

for i = starts:ends 
    twi = twht(i);
    pwi = poswh(i);
    [~,sfb] = min(abs(twht(i)-bt));
    
end
