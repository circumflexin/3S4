
clear all 

tag = 'oo23_295a';
ptrackfolder = 'D:\Analysis\3S4\2_pipeline\make_dsfb\';
btrack_dir = "D:\Analysis\3S4\0_data\AIS data\Individual vessel AIS tracks_Oct9 - Nov30 vessels with IMO";
out_dir = "D:\Analysis\3S4\2_pipeline\Fishing_animations";
load([ptrackfolder,tag,'_pt_dsfb.mat']) % this includes time vectors twh and tgps
load([ptrackfolder,tag,'_pt_relAIS.mat'])
trim = false;
husgpsfolder = 'D:\Analysis\3S4\0_data\AIS data\'

nx = 10; % difference in sampling frequencies
twh = datetime(wtrack.twh(1:nx:end), "ConvertFrom", 'datenum');
swcor = wtrack.swcor(1:nx:end);
swcorh = wtrack.swcorh(1:nx:end);
headcor = wtrack.headcor(1:nx:end);
dsfb = wtrack.dsfb;
poswh = wtrack.poswh;
fb_lat = wtrack.fb_lat;
fb_lon = wtrack.fb_lon;
headcor = rad2deg(headcor);
pos_gps = wtrack.pos_gps;

%dsfb(dsfb<100) = 20


if(trim)
    % get period of interest
    plot(twh,dsfb(:,:)/1000);
    set(gca,'YLim',[0,22],'YTick',[0.1, 0.25 0.5 1 2 5 10 20],...
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
else
    twh2 = twh;
    poswh2 = poswh;
    dsfb2 = dsfb;
    fb_lon2 = fb_lon;
    fb_lat2 = fb_lat;
end

%relevant vessels
mds = min(dsfb2);
close_ves = find(min(mds,[],1) < 2000) %vessels to plot 
length(close_ves)

% approached vessels
ind = find(mds<50)
val = mds(mds<50)
%[val,ind] = mink(mds,2);
rel2 = rel(ind,:); %get names and AIS files of the vessels the whales got close to

plot(dsfb2(:,ind)/1000,twh2);
set(gca,'XLim',[0,5],'XTick',[0.01,0.1, 0.25 0.5 1 2 5 10 20],...
            'XScale','log','XDir','reverse','YDir','reverse')
%mycolors = [1 0 0; 0 0 1];
legend(rel2(:,2))
ax = gca;
%ax.ColorOrder = mycolors;
f = gcf;
exportgraphics(f,fullfile('2_pipeline\Fishing_animations',strcat(tag,'.png')),'Resolution',300)


proj = projcrs("UTM39SW84","Authority","IGNF")
[whx, why] = projfwd(proj,poswh2(:,1),poswh2(:,2));
minx = min(whx);
miny = min(why);
whx = whx-minx;
why = why-miny;

[fbx, fby] = projfwd(proj,fb_lat2,fb_lon2);
fbx = fbx-minx;
fby = fby-miny;

wlimits = [min(whx)-1000, max(whx)+1000,min(why)-1000,max(why)+1000]

% load HUS position
load([husgpsfolder,'HUS_GPS_30s.mat'])
df = 30; % GPS resolution (in s)
ts = datetime(ts,'ConvertFrom','datenum');

indh = ts>=twh2(1) & ts<=max(twh2);
ts = ts(indh); poss = poss(indh,:); % limit to deployment period

[husx, husy] = projfwd(proj,poss(:,1),poss(:,2));
husx = husx-minx;
husy = husy-miny;

% relevant vessels not to plot as non-focals
find((ismember(close_ves, ind)))
length(close_ves)

%%  perform animation with presets
close all

figure('units','pixels','position',[0 0 1440 1080])
w = animatedline('MaximumNumPoints',100,Color="black", LineWidth = 2);
hus = animatedline('MaximumNumPoints',100,Color="green", LineWidth = 2);

bf1 = animatedline('MaximumNumPoints',100,Color="red", LineWidth = 2); % decide which approaches you will score, highlight and make sure you know the colour of the vessel
%bf2 = animatedline('MaximumNumPoints',100, Color="blue", LineWidth = 2);

%bf3 = animatedline('MaximumNumPoints',100,Color="magenta", LineWidth = 1); 
%bf4 = animatedline('MaximumNumPoints',100, Color="#D95319", LineWidth = 1);
%bf5 = animatedline('MaximumNumPoints',100,Color="#A2142F", LineWidth = 1); 


h1 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h2 = animatedline('MaximumNumPoints',100,Color="#d3d3d3");
h3 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h4 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h5 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h6 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h7 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h8 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h9 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h10 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h11 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h12 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h13 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h14 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h15 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h16 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h17 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h18 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h19 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h20 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h21 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h22 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h23 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");
h24 = animatedline('MaximumNumPoints',100, Color="#d3d3d3");


timestamp_text = text(wlimits(2),wlimits(4)*0.99,'','FontSize',14,'HorizontalAlignment', 'right')
timestamp_text2 = text(wlimits(2),wlimits(4)*0.97,'','FontSize',14,'HorizontalAlignment', 'right')

axis(wlimits)

xw = whx
yw = why

F(length(xw)) = struct('cdata',[],'colormap',[]);
vfile = fullfile(out_dir,[tag,'.avi'])
v = VideoWriter(vfile, 'MPEG-4');
open(v)
for k = 1:length(xw)
    addpoints(w,xw(k),yw(k));
    addpoints(bf1,fbx(k,ind(1)),fby(k,ind(1)));
    %addpoints(bf2,fbx(k,ind(2)),fby(k,ind(2)));
    %addpoints(bf3,fbx(k,ind(3)),fby(k,ind(3)));
    %addpoints(bf4,fbx(k,ind(4)),fby(k,ind(4)));

    [tmp,ind2] = min(abs(ts-twh(k)));
    addpoints(hus,husx(ind2),husy(ind2));

    % plot vessels the whales don't visit
    addpoints(h1,fbx(k,close_ves(1)),fby(k,close_ves(1)))
    addpoints(h2,fbx(k,close_ves(2)),fby(k,close_ves(2)))
    %addpoints(h3,fbx(k,close_ves(3)),fby(k,close_ves(3)))
    addpoints(h4,fbx(k,close_ves(4)),fby(k,close_ves(4)))
    addpoints(h5,fbx(k,close_ves(5)),fby(k,close_ves(5)))
    addpoints(h6,fbx(k,close_ves(6)),fby(k,close_ves(6)))
    addpoints(h7,fbx(k,close_ves(7)),fby(k,close_ves(7)))
    %addpoints(h8,fbx(k,close_ves(8)),fby(k,close_ves(8)))
    %addpoints(h9,fbx(k,close_ves(9)),fby(k,close_ves(9)))
    %addpoints(h10,fbx(k,close_ves(10)),fby(k,close_ves(10)))
    %addpoints(h11,fbx(k,close_ves(11)),fby(k,close_ves(11)))
    %addpoints(h12,fbx(k,close_ves(12)),fby(k,close_ves(12)))
    %addpoints(h13,fbx(k,close_ves(13)),fby(k,close_ves(13)))
    %addpoints(h14,fbx(k,close_ves(14)),fby(k,close_ves(14)))
    %addpoints(h15,fbx(k,close_ves(15)),fby(k,close_ves(15)))
    %addpoints(h16,fbx(k,close_ves(16)),fby(k,close_ves(16)))
    %addpoints(h17,fbx(k,close_ves(17)),fby(k,close_ves(17)))
    %addpoints(h18,fbx(k,close_ves(18)),fby(k,close_ves(18)))
    %addpoints(h19,fbx(k,close_ves(19)),fby(k,close_ves(19)))
    %addpoints(h20,fbx(k,close_ves(20)),fby(k,close_ves(20)))
    %addpoints(h21,fbx(k,close_ves(21)),fby(k,close_ves(21)))
    %addpoints(h22,fbx(k,close_ves(22)),fby(k,close_ves(22)))
    %addpoints(h23,fbx(k,close_ves(23)),fby(k,close_ves(23)))
    %addpoints(h24,fbx(k,close_ves(24)),fby(k,close_ves(24)))


    timestamp_text.String = char(twh2(k));
    timestamp_text2.String = k;
    
    drawnow %limitrate
    frame = getframe(gcf);
    writeVideo(v,frame);
end
close(v)
%% 


% score video
score_file = fullfile(out_dir,[tag,'.csv'])
if ~isfile(score_file)
    writetable(array2table(zeros(0,7),'VariableNames',{'Vessel_no','Vessel_name','W_vessel_start','W_vessel_end','Decision_point','Final_loop_start' 'Final_loop_end'}),score_file)
end
winopen(score_file) % tell excel to open the frame annotations file

%score the video in sample number (lower time feed) it's quicker and if you
% use excel it will only fuck up a time and date value anyway

% get vessel number & name
ind(2)
rel(ind(2),2)

%after you're done scoring 
tab = readtable(score_file)
tab.Deploy = tag
tab.W_vessel_start = twh2(tab.W_vessel_start)
tab.W_vessel_end = twh2(tab.W_vessel_end)
tab.Decision_point = twh2(tab.Decision_point)
tab.Final_loop_start = twh2(tab.Final_loop_start)
tab.Final_loop_end = twh2(tab.Final_loop_end)

%get file start times
settagpath('cal','D:\Analysis\3S4\0_data\cal')
[CAL,DEPLOY,ufname] = d3loadcal(tag)
ctab = num2cell(DEPLOY.SCUES.TIME);
names = DEPLOY.FN;
ctab = cell2table([ctab names'])

% e.g. 
[file, time] = get_wavtime(tab.W_vessel_start, ctab)




