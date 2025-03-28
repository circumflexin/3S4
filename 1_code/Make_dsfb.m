
% config
dis_thresh = 30 %km
t_thresh = 30 %sec
df = 30
sog_thresh = 30
wtrack_dir = "D:\Analysis\3S4\0_data\Pseuedotracks"
btrack_dir = "D:\Analysis\3S4\0_data\AIS data\Individual vessel AIS tracks_Oct9 - Nov30 vessels with IMO"
fs_dif = 10
ptracks = {'oo23_292b_pt','oo23_295a_pt','oo23_295b_pt','oo23_297b_pt','oo23_299a_pt','oo23_299b_pt','oo23_301a_pt','oo23_302a_pt'}
diagn = false;

AISfiles = dir(fullfile(btrack_dir,'*.csv'))

for k = 1:length(ptracks)
    trackn = ptracks(k)
    temp = open(fullfile(wtrack_dir,append(trackn,".mat")))
    poswh = temp.poswh;
    twh = temp.twh(1:fs_dif:end);
    %datestr(twh(1:4))
    twht = datetime(twh,'ConvertFrom','datenum');

    rel = strings()
    relname = strings()
    ellipsoid = almanac('earth','wgs84','meters');
    n = 1;
    % get rid of files which have no overlap or are never closer than the
    % thresh or are not fishing boats
    for f = 1:length(AISfiles)
        ais_file=AISfiles(f).name
        boat = readtable(fullfile(btrack_dir,ais_file));
        x1 = string(boat.validated_shiptype{1}) ~= "Fishing Vessel"; %
        AIS_name = string(boat.name_ais(1));
        x2 = (twht(1) > boat.date_time_utc(length(boat.date_time_utc)) |  twht(length(twht)) <boat.date_time_utc(1)); %tag starts after ais ends or ends before it starts
        if ~any([x1, x2])
            boat2 = boat(1:df:end,:); %downsample for performance
            x3 = ~any(boat2.date_time_utc > twht(1) & boat2.date_time_utc < twht(length(twht))); % check if AIS actually records any data during deploy
            if(~any(x3))
                posf = [boat2.lat, boat2.lon];
                for i=1:length(poswh)
                    mind = min(distance(poswh(i,:), posf, ellipsoid));
                    if mind < dis_thresh*1000
                        rel(n,1) = string(ais_file)
                        rel(n,2) = AIS_name
                        n = n +1;
                        break
                    end
                end
            end
        end
    end


    dsfb = nan(length(twh),length(rel));
    fb_lat = dsfb;
    fb_lon = dsfb;

    for j = 1:length(rel)
        j
        boat = readtable(fullfile(btrack_dir,rel(j)));
        boat = boat(boat.sog<sog_thresh,:);
        bool = boat.date_time_utc >= twht(1) & boat.date_time_utc < twht(length(twht));
        btrack2 = boat(bool,:);
        bt = btrack2.date_time_utc;
        o1 =  twht>=min(btrack2.date_time_utc);
        o2 = twht<=max(btrack2.date_time_utc);
        o3 = o1+o2;
        starts = find(o3 ==2,1, 'first');
        ends = find(o3 ==2, 1, 'last');
        for i = starts:ends
            twi = twht(i);
            pwi = poswh(ceil(i),:);
            [val,ind] = min(abs(twht(i)-bt));
            if val < seconds(t_thresh)
                pfb = [btrack2.lat(ind),btrack2.lon(ind)];
                [dsfb(i,j),tmp] = distance(pwi, pfb, ellipsoid);
                fb_lon(i,j) = btrack2.lon(ind);
                fb_lat(i,j) = btrack2.lat(ind);
            end
        end
    end

    p = plot(dsfb)
    temp.dsfb = dsfb
    temp.fb_lon = fb_lon;
    temp.fb_lat = fb_lat;

    wtrack = temp
    save(fullfile("D:\Analysis\3S4\2_pipeline\make_dsfb\",append(trackn,"_dsfb.mat")),'wtrack')
    save(fullfile("D:\Analysis\3S4\2_pipeline\make_dsfb\",append(trackn,"_relAIS.mat")),'rel')
    writematrix(rel, fullfile("D:\Analysis\3S4\2_pipeline\make_dsfb\",append(trackn,"_relAIS.csv")))
    if diagn
        saveas(gcf, [string(trackn)], 'fig')
        saveas(gcf, [string(trackn)], 'png')
    end
end