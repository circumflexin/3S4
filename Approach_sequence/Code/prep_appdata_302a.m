
tag = 'oo23_302a';
ptrackfolder = 'D:\Analysis\3S4\AIS_processing\outputs\';
btrack_dir = "D:\Raw\3S4\AIS data\Individual vessel AIS tracks_Oct9 - Nov30 vessels with IMO";
load([ptrackfolder,tag,'_pt_dsfb.mat']) % this includes time vectors twh and tgps
load([ptrackfolder,tag,'_pt_relAIS.mat'])

nx = 20; % 1 Hz data thinned
twh = datetime(wtrack.twh(1:nx:end), "ConvertFrom", 'datenum');
swcor = wtrack.swcor(1:nx:end);
swcorh = wtrack.swcorh(1:nx:end);
headcor = wtrack.headcor(1:nx:end);
dsfb = wtrack.dsfb(1:nx:end,:);
poswh = wtrack.poswh(1:nx/10:end,:);
fb_lat = wtrack.fb_lat(1:nx:end,:);
fb_lon = wtrack.fb_lon(1:nx:end,:);
headcor = rad2deg(headcor);
pos_gps = wtrack.pos_gps;

% get period of interest
plot(twh,dsfb(:,:)/1000);
set(gca,'YLim',[0,22],'YTick',[0.25 0.5 1 2 5 10 20],...
            'YScale','log','YDir','reverse')
ax = gca;
win = ginput(2);
start = num2ruler(win(1,1),ax.XAxis);
fin = num2ruler(win(2,1),ax.XAxis);

bool = twh > start & twh < fin;
twh2 = twh(bool);
poswh2 = poswh(bool,:);
dsfb2 = dsfb(bool,:);
fb_lon2 = fb_lon(bool,:);
fb_lat2 = fb_lat(bool,:);



mds = min(dsfb2);
find(min(mds,[],1) < 2000)
[val,ind] = mink(mds,2);

plot(twh2,dsfb2(:,ind))
rel2 = rel(ind)


% perform animation with presets
%figure('units','pixels','position',[0 0 1440 1080])
h = animatedline('MaximumNumPoints',100,Color="black", LineWidth = 2);
h2 = animatedline('MaximumNumPoints',100,Color="red");
h3 = animatedline('MaximumNumPoints',100, Color="blue");

h4 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h5 = animatedline('MaximumNumPoints',100,Color="#d3d3d3");
h6 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h7 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h8 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h9 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h10 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h11 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");

timestamp_text = text(21.65,70.49,'','FontSize',14,'HorizontalAlignment', 'right')


axis([21.3,21.65,70.48,70.6])

xw = poswh2(:,2);
yw = poswh2(:,1);

F(length(xw)) = struct('cdata',[],'colormap',[]);

v = VideoWriter("test_small.avi");
open(v)
for k = 1:length(xw)
    addpoints(h,xw(k),yw(k));
    addpoints(h2,fb_lon2(k,7),fb_lat2(k,7));
    addpoints(h3,fb_lon2(k,19),fb_lat2(k,19));
    
    % plot vessels the whales don't visit
    addpoints(h4,fb_lon2(k,6),fb_lat2(k,6))
    addpoints(h5,fb_lon2(k,10),fb_lat2(k,10))
    addpoints(h6,fb_lon2(k,13),fb_lat2(k,13))
    addpoints(h7,fb_lon2(k,17),fb_lat2(k,17))
    addpoints(h8,fb_lon2(k,18),fb_lat2(k,18))
    addpoints(h9,fb_lon2(k,20),fb_lat2(k,20))
    addpoints(h10,fb_lon2(k,26),fb_lat2(k,26))
    addpoints(h11,fb_lon2(k,29),fb_lat2(k,29))
    timestamp_text.String = char(twh2(k));
    
    drawnow limitrate
    frame = getframe(gcf);
    writeVideo(v,frame);
end
close(v)



%get file start times
settagpath('cal','D:\Raw\3S4\D3\cal')
[CAL,DEPLOY,ufname] = d3loadcal(tag)

ctab = num2cell(DEPLOY.SCUES.TIME);
names = DEPLOY.FN

ctab = cell2table([ctab names'])
writetable(ctab, 'out/ctab.csv')


% convert a cue time to a location in the audio


% closing speed vs over-ground speed
clospeed = diff(dsfb2);
plot(clospeed(:,19))