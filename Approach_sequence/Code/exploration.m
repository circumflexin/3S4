
tag = 'oo23_302a';
ptrackfolder = 'D:\Analysis\3S4\AIS_processing\outputs\';
btrack_dir = "D:\Raw\3S4\AIS data\Individual vessel AIS tracks_Oct9 - Nov30 vessels with IMO";
load([ptrackfolder,tag,'_pt_dsfb.mat']) % this includes time vectors twh and tgps
load([ptrackfolder,tag,'_pt_relAIS.mat'])



nx = 10; % 1 Hz data thinned
twh = datetime(wtrack.twh(1:nx:end), "ConvertFrom", 'datenum');
swcor = wtrack.swcor(1:nx:end);
swcorh = wtrack.swcorh(1:nx:end);
headcor = wtrack.headcor(1:nx:end);
dsfb = wtrack.dsfb(1:nx:end,:);
% poswh = wtrack.poswh(1:nx/10:end,:);

headcor = rad2deg(headcor);
pos_gps = wtrack.pos_gps;


plot(twh,dsfb(:,:)/1000);
set(gca,'YLim',[0.2,22],'YTick',[0.25 0.5 1 2 5 10 20],...
            'YScale','log','YDir','reverse')

% get period of interest
ax = gca;
win = ginput(2);
start = num2ruler(win(1,1),ax.XAxis);
fin = num2ruler(win(2,1),ax.XAxis);

bool = twh > start & twh < fin;
twh2 = twh(bool);
poswh2 = poswh(bool);
dsfb2 = dsfb(bool,:);
mds = min(dsfb2);
[val,ind] = mink(mds,2);

plot(twh2,dsfb2(:,ind))
rel2 = rel(ind)

colline(poswh2(:,1),poswh2(:,2),datenum(twh2-start));
hold on
for j = 1%:length(rel2)
        j
        boat = readtable(fullfile(btrack_dir,rel2(j)));
        boat = sortrows(boat, "date_time_utc")
        bool = boat.date_time_utc > start & boat.date_time_utc < fin;
        boat = boat(bool,:);
        colline(boat.lat,boat.lon, datenum(boat.date_time_utc-start))
end
hold off

%get file start times
settagpath('cal','D:\Raw\3S4\cal')
[CAL,DEPLOY,ufname] = d3loadcal(tag)

ctab = num2cell(DEPLOY.SCUES.TIME);
names = DEPLOY.FN

ctab = cell2table([ctab names'])
writetable(ctab, 'out/ctab.csv')


% closing speed vs over-ground speed

clospeed = diff(dsfb2)
plot(clospeed(:,19))