% Generates a timeseries plot and map per deployment.
% These plots are preliminary, made with the purpose of discussing the
% format during the 3S-Progress meeting on 16-18 April 2024.
clear all; close all

%% User defined
save_plots = true; % save plots yes/no?
trunc_dist = 50; % cut off vessel distance at the noise floor 
% to prevent large spurious spikes in the plot

tags = {'oo23_292b','oo23_295a','oo23_295b','oo23_297b','oo23_299a','oo23_299b'}; % all tags with complete audits
%tags = {'oo23_292b','oo23_295a','oo23_295b','oo23_297b','oo23_299a','oo23_299b'}; % all priority tags (focal, nonfocal and one baseline (oo23_302a))

% Data folders (relative or absolute paths)
prhfolder = 'D:\Analysis\3S4\0_data\prh\';
calfolder = 'D:\Analysis\3S4\0_data\cal\';
speedfolder = 'D:\Analysis\3S4\0_data\speed from jiggle\';
auditfolder = 'D:\Analysis\3S4\0_data\Audits\';
noiseauditfolder = 'D:\Analysis\3S4\0_data\Noise audit data\';
ptrackfolder = 'D:\Analysis\3S4\2_pipeline\make_dsfb\';
husgpsfolder = 'D:\Analysis\3S4\0_data\AIS data\';
HMM_folder = 'D:\Analysis\3S4\2_pipeline\HMMs';

%% Start code
% Loop over tags
for j=1:length(tags)


% Load user defined
tag = tags{j};
get_udef(tag)


%% Load input data

% Load DTAG data (10 Hz)
settagpath('prh',prhfolder,'cal',calfolder)
loadprh(tag,'fs','p','pitch','head')
tutc = (tstart + (0:(1/fs):((length(p)-1)/fs))/86400)'; % 10 Hz time vector
ind = find(tutc<=ttagoff); tutc=tutc(ind); p=p(ind); pitch=pitch(ind); head=head(ind); % keep data before tag-off
p(1:(tsgood*3600*fs)) = nan; % set bad depths to nan

% Calculate time-averaged heading and turning angle
N = fs * 30; % averaging window length
tmp = [zeros(N*0.5,1); head; zeros(N*0.5,1)]; % add half a window on either side
L = floor(length(tmp)/N)*N; 
thead = tutc(1) + (0:((L/N)-1))*((N/fs)/86400);
tmp = reshape(tmp(1:L), N, L/N);
headm=[];
for i=1:size(tmp,2)
    headm(i,1) = rad2deg(circ_mean(tmp(:,i))); % mean heading
end
turnangle = [wrapTo180(diff(headm)); nan];

% Load speed from acceleration jiggle (10 Hz)
psurf = 5; % depth criterion
offset = 0.5; % small difference with DTAG data in this case (check later)
load([speedfolder,tag,'prh10_jiggle.mat'],'speed')
sw = [nan((tsgood*3600*fs)-1-(0.5*fs),1); speed.JJ]; % set to nan when depth was bad
sw(p<psurf)=nan; % remove speeds near the surface
clear speed

% Thin DTAG sensor data to more plottable fs 
fs_old = fs;
fs = 0.1; % in Hz
tutc = tutc(1:(fs_old/fs):end);
p = p(1:(fs_old/fs):end);
pitch = pitch(1:(fs_old/fs):end);
head = head(1:(fs_old/fs):end);
sw = sw(1:(fs_old/fs):end);

% Load whale audits
load([auditfolder,tag,'aud.mat'], 'taudit','duraudit','code')
% Indices for sound types to plot (better to fix the codes at an earlier stage and make a list of all possible labels):
ics = strncmpi(code,'CS 12',5) | strncmpi(code,'CS 2',4) | strncmpi(code,'CS 3',4) | strncmpi(code,'CS 13',5) | strncmpi(code,'BUZZ 2',6) | strncmpi(code,'BUZZ 3',6) | strncmpi(code,'BUZZ 12',7) | strncmpi(code,'BUZZCS 12',9) | strncmpi(code,'CSBUZZ 23',9);
itailslap = strncmpi(code,'TAILSLAP 2',10) | strncmpi(code,'TAILSLAP 3',10);
ihfw = strncmpi(code,'HFWHISTLE 2',11) | strncmpi(code,'HFWHISTLE 3',11); % there aren't any right now
iss = strncmpi(code,'SS 12',5) | strncmpi(code,'SS 2',4) | strncmpi(code,'SS 3',4)| strncmpi(code,'SS2',3);
iss = iss | strncmpi(code,'WHISTEL 2',9) | strncmpi(code,'WHISTLE 2',9) | strncmpi(code,'WHSITLE 2',9) | strncmpi(code,'WHISTLE 3',9) | strncmpi(code,'WHSITLE 3',9);

% % TEMP: Plot audited breaths with dive profile
% % Small change in alignment - tag moving backward or other issue? 
% figure(99); plot(tutc,p); set(gca,'YDir','reverse'); datetick('x')
% ibr = strncmpi(code,'BR',2);
% hold on; plot(taudit(ibr),1,'ro'); hold off

% Load manmade noise audits
load([noiseauditfolder,tag,'aud_noise.mat'], 'taudit_noise','inoise')
taudit_noise = [taudit_noise; tutc(end)]; % add end time
noise_labels = {'vessel noise','10 kHz','20 kHz','40 kHz','80 kHz'};
% convert to plottable format 
inoise2 = nan(length(tutc),5);
for i=1:(length(taudit_noise)-1) 
    ind = tutc>=taudit_noise(i) & tutc<taudit_noise(i+1); 
    if inoise(i,1)==1, inoise2(ind,1) = 1; end
    if inoise(i,2)==1, inoise2(ind,2) = 2; end    
    if inoise(i,3)==1, inoise2(ind,3) = 3; end
    if inoise(i,4)==1, inoise2(ind,4) = 4; end
    if inoise(i,5)==1, inoise2(ind,5) = 5; end
end

% Load pseudotrack - temporary, change datastructure to match Paul's for
% futre compatbility 
load([ptrackfolder,tag,'_pt_dsfb.mat']) % this includes time vectors twh and tgps
nx = 10; % 1 Hz data thinned to 0.1 Hz for plotting
twh = wtrack.twh(1:nx:end);
swcor = wtrack.swcor(1:nx:end);
swcorh = wtrack.swcorh(1:nx:end);
headcor = wtrack.headcor(1:nx:end);
headcor = rad2deg(headcor);
pos_gps = wtrack.pos_gps;
turnanglecor = [wrapTo180(diff(headcor)); nan];

poswh = wtrack.poswh;

% Load HUS GPS data
load([husgpsfolder,'HUS_GPS_30s.mat'])
df = 30; % GPS resolution (in s)
ind = ts>=ttagon & ts<=ttagoff;
ts = ts(ind); poss = poss(ind,:); % limit to deployment period

% Vessel-whale distance
ellipsoid = almanac('earth','wgs84','meters');
dswh = nan(length(ts),1);  %  distance to HUS
%dnfwh = nan(length(ts),2); %  TEMP: distance to nearest fishing vessel
for i=1:length(ts)
    [tmp,ind] = min(abs(ts(i)-twh));
    [dswh(i,1),tmp] = distance(poswh(ind,:), poss(i,:), ellipsoid);
%     if strcmp(tag,'oo23_292b')
%         [dtmp(1),tmp] = distance(posf(1,:), poss(i,:), ellipsoid);
%         [dtmp(2),tmp] = distance(posf(2,:), poss(i,:), ellipsoid);
%         [dtmp(3),tmp] = distance(posf(3,:), poss(i,:), ellipsoid);
%         [dnfwh(i,1),dnfwh(i,2)] = min(dtmp); % [minimum distance, vessel index];
%     end
end

%load HMM
HMM = readtable(fullfile(HMM_folder,"HMM_2state_1.5min.csv"));
HMM = HMM(strcmp(HMM.deploy, append(tag,'_pt')),:);

dsfb = wtrack.dsfb;
dsfb(dsfb<trunc_dist) = trunc_dist;

%% Map
sdf = df/(1/fs); % resolution difference HUS and whale track
figure(1); colormap('colorcube'); 

for i=1:2 % loop over two subplots

% Panel layout     
% if strcmp(tag,'oo23_292b') % deployment dependent
    if i==1, subplot(1,3,i); elseif i==2, subplot(1,3,2:3); end 
% end

% Plot HUS track
if i==2 % color gradient only in panel 2
    [tmp, is] = min(abs(ts-tson(1))); [tmp, ie] = min(abs(ts-tson(2)));
    hc1 = colline(poss(is:ie,2),poss(is:ie,1),1:(ie-is+1));
    set(hc1,'LineWidth',6,'Marker','.','MarkerSize',18);
end
ind = ts>=datenum(tson(1)) & ts<=datenum(tson(2)); 
hold on; plot(poss(ind,2),poss(ind,1),'k:','LineWidth',0.5); box on;

% Plot whale track
if i==2 % Only in panel 2: %% NOTE: Gradient not right if whale track is shorter than exposure (e.g. oo23_295a). Needs fixing
    [tmp, is] = min(abs(twh-tson(1))); [tmp, ie] = min(abs(twh-tson(2)));
    tmp = repmat(1:ceil((ie-is+1)/sdf),sdf,1); tmp = tmp(:);
    hc2 = colline(poswh(is:ie,2),poswh(is:ie,1),tmp(1:(ie-is+1))); % plot color gradient
    set(hc2,'LineWidth',6,'Marker','.','MarkerSize',12);
end
plot(poswh(:,2), poswh(:,1),'k-',pos_tagon(:,2),pos_tagon(:,1),'ko','LineWidth',1)
xlabel('Longitude (\circE)'); ylabel('Latitude (\circN)');
ind = (twh>=datenum(tbas) & twh<=datenum(tson(1))); 
plot(poswh(ind,2), poswh(ind,1),'.-')          % baseline track
if i==1 % only in panel 1:
    ind = (twh>=datenum(tson(1)) & twh<=datenum(tson(2))); 
    plot(poswh(ind,2), poswh(ind,1),'.-');     % plot dashed-dotted sonar track
    plot(pos_gps(:,2), pos_gps(:,1),'o','Color','#FFA500');  % plot gps locations
end

% HUS position at sonar start
ind = find(ts>=datenum(tson(1)) & ts<=datenum(tson(2))); 
plot(poss(ind(1),2),poss(ind(1),1),'ko');

% Set limits of subplot 2
if i==2
    if strcmp(tag,'oo23_292b')
        set(gca,'YLim',[70.17, 70.28], 'XLim',[20.8, 21.15])
    elseif strcmp(tag,'oo23_295a')
        set(gca,'YLim',[70.2, 70.5], 'XLim',[20.1, 21.1])
    else
        set(gca,'YLim',[min(poswh(:,1)), max(poswh(:,1))],...
            'XLim',[min(poswh(:,2)), max(poswh(:,2))])
    end
end

% Fix latlon aspectratio
latlim = get(gca,'YLim'); lonlim = get(gca,'XLim');
latmean = ((latlim(1)+latlim(2))/2);
aspectratio = 1/cos(latmean*(pi/180));
set(gca,'DataAspectRatio',[aspectratio 1 1])

% Add scale bar
pos_bar(1) = latlim(1) + (latlim(2)-latlim(1)) * (1/20);
pos_bar(2) = lonlim(1) + (lonlim(2)-lonlim(1)) * (1/20); % (15/20);
pos_bar(3) = lonlim(1) + (lonlim(2)-lonlim(1)) * (3/20); % (17/20);
pos_bar(4) = lonlim(1) + (lonlim(2)-lonlim(1)) * (5/20); % (19/20);  
plot([pos_bar(2) pos_bar(4)],[pos_bar(1) pos_bar(1)],'k-','LineWidth',1)
[rm_scalebar,tmp] = distance(pos_bar(1),pos_bar(2),pos_bar(1),pos_bar(4),ellipsoid);
rkm_scalebar = (round(rm_scalebar/100)) / 10;
text(pos_bar(3),pos_bar(1),[num2str(rkm_scalebar) ' km'],'HorizontalAlignment','center','VerticalAlignment','bottom')
hold off

end

if save_plots
    saveas(gcf, ['plots\map_',tag], 'fig')
    % set(gcf,'color','w');
    % export_fig([name_pdf,'.pdf'], '-painters', '-q101', '-append','-nocrop')
end


%% Timeseries plot
figure(3); colormap('colorcube'); 

%%%%%%%%% Manmade noise panel
h(1)=subplot(8,1,1); 
plot(tutc,inoise2,'LineWidth',2); datetickzoom('x',15)

%%%%%%%%% Vessel distance panel
h(2)=subplot(8,1,2);
plot(twh,dsfb(:,:)/1000); datetickzoom('x',15)
ind = ts>=tson(1) & ts<=tson(2);
%semilogy(ts(ind),dswh(ind)); % distance to HUS
ylabel({'Vessel','distance (km)'});  datetickzoom('x',15)


%%%%%%%%% Speed panel
h(3)=subplot(8,1,3); plot(twh,swcor,'.:','Color',[.6 .6 .6]); datetickzoom('x',15)
hold on; plot(twh,swcorh,'k-','LineWidth',1)
ylabel('Speed (ms^{-1})')
HMM1 = HMM.twh(HMM.state==1);
HMM2 = HMM.twh(HMM.state==2);
plot(HMM1,ones(length(HMM1),1)*0.1,"|", 'MarkerSize',7, 'LineWidth',4, 'Color', "red");
plot(HMM2,ones(length(HMM2),1)*0.1,"|",'MarkerSize',7, 'LineWidth',4, 'Color', "#4DBEEE");
hold off

%%%%%%%%% Heading and turning angle panel
h(4)=subplot(8,1,4); plot(thead,turnangle,'-','Color','#FFA500','LineWidth',1);
hold on; plot(thead,headm,'k.:','LineWidth',0.5); datetickzoom('x',15)
ylabel({'Heading (\circ)','Turn angle (\circ)'})

%%%%%%%%% Dive profile panel
h(5)=subplot(8,1,5:8);
plim = max(max(p)+5, 95); % max depth in plot

% Plot polygon for missing data
if tsgood>0 % missing data at the start
    patch([tstart,tstart+tsgood/24,tstart+tsgood/24,tstart],...
        [plim-1,plim-1,-8,-8], [.9 .9 .9],'LineStyle','none')
end
if ~isnan(tegood) % missing data at the end
    patch([tstart+tegood/24,ttagoff,ttagoff,tstart+tegood/24],...
        [plim-1,plim-1,-8,-8], [.9 .9 .9],'LineStyle','none')
end

% Plot color gradient
yoffset = round(0.8421*plim/20)*20; % y-location of color and sun angle 
[tmp, is] = min(abs(tutc-tson(1))); [tmp, ie] = min(abs(tutc-tson(2)));
hold on; hc3 = colline(tutc(is:ie,1),ones(ie-is+1,1)*yoffset,1:(ie-is+1));
set(hc3,'LineWidth',6,'Marker','.','MarkerSize',8);
xlabel('Time (UTC)'); ylabel('Depth (m)'); box on

% Plot sun elevation angle
[tmp,SunAngle] = SolarAzEl(tutc,pos_tagon(1),pos_tagon(2),0);
ind = find(SunAngle>-6); % Only show civil twilight or higher
inight = find(abs(diff(ind))>1)+1; % insert nan at start of night
SunAngle_nonight = SunAngle(ind); SunAngle_nonight(inight)=nan;
plot(tutc(ind),SunAngle_nonight+yoffset,'-',...
    tutc([1 end]),[yoffset yoffset],':','Color',[.6 .6 .6])
tsun0 = tutc(find(abs(diff(SunAngle>=0))==1)); % times of sunrise and sunset

% plot dive profile
plot(tutc,p,'k-','LineWidth',1);  datetickzoom('x',15)
plot(tutc([1 end]),[0,0],':','Color',[.6 .6 .6])

% plot social sounds (and whistles) above the dive profile
tss = taudit(iss) + (duraudit(iss)/2)/86400; % one marker in the middle
plot(tss,ones(length(tss),1)*-5,'b.'); 

% plot HF whistles
thfw = taudit(ihfw);
plot(thfw,ones(length(thfw),1)*-5,'s','Color','#2AAA8A','LineWidth',2);

% Plot clicking sound periods onto dive profile
tcs = taudit(ics); durcs = duraudit(ics)/86400;
is=[]; ie=[];
for i=1:length(tcs)
    [tmp, is] = min(abs((tutc-tcs(i)))); 
    [tmp, ie] = min(abs((tutc-(tcs(i)+durcs(i)))));
    plot(tutc(is:ie),p(is:ie),'r-','LineWidth',2); % better to plot all at once, oh well.
end

% Plot tailslap sounds onto dive profile
ttailslap = taudit(itailslap);
ind=[];
for i=1:length(ttailslap)
    [tmp, ind(i,1)] = min(abs((tutc-ttailslap(i)))); % find closest time index
end
plot(tutc(ind),p(ind),'v','Color','#FFA500','LineWidth',2);

hold off


%%%%%%%%% Plot start/end of experimental phases and set axes properties
% Link axes
linkaxes(h,'x')

% Plot vertical lines in each panel
ylow = [-180,0.01,-180,-180,-180];
for i=1:length(h)
    set(gcf,'CurrentAxes',h(i))
    hold on;
    plot([ttagon,ttagon,nan,ttagoff,ttagoff],[ylow(i),180,nan,ylow(i),180],'Color','#808080','LineWidth',1);
    plot([tbas,tbas],[ylow(i),180],'Color','#FFC0CB','LineWidth',1);
    plot([tson(1),tson(1),nan,tson(2),tson(2)],[ylow(i),180,nan,ylow(i),180],'Color','#FFC000','LineWidth',1)
    for k=1:length(tsun0)
        plot([tsun0(k),tsun0(k)],[ylow(i),180],':','Color',[.6 .6 .6],'LineWidth',0.5)
    end
    hold off

     % Set axes properties
    if i==1
        set(gca,'YLim',[0.5,5.5],'YTick',1:5,'YTickLabel',noise_labels,...
            'XTickLabel','','OuterPosition',[0,0.875,1,0.11])
    elseif i==2
        set(gca,'YLim',[0.04,22],'YTick',[0.1, 0.5 1 2 5 10 20],'XTickLabel','',...
            'YScale','log','YDir','reverse',...
            'OuterPosition',[0,0.75,1,0.12])
    elseif i==3
        set(gca,'YLim',[0,3],'YTick',[0 1 2 3],'XTickLabel','',...
            'OuterPosition',[0,0.625,1,0.13])
    elseif i==4
        set(gca,'YLim',[-180,180],'YTick',[-150 0 150],'XTickLabel','',...
            'OuterPosition',[0,0.5,1,0.13])
    elseif i==5
        set(gca,'YLim',[-9, plim],'YDir','reverse',...
            'OuterPosition',[0,0.055,1,0.45])
    end
end
set(gca,'XLim',[ttagon,ttagoff]) % show from tag-on to tag-off

if save_plots
    saveas(gcf, ['plots\timeseries_',tag], 'fig')
    set(gca,'XLim',[tbas,tson(1)]) % baseline period
    saveas(gcf, ['plots\timeseries_baseline_',tag], 'fig')
    set(gca,'XLim',[tson(1)-0.5/24,tson(2)+0.5/24]) % sonar period +/- 30 min
    saveas(gcf, ['plots\timeseries_exposure_',tag], 'fig')
    % set(gcf,'color','w');
    % export_fig([name_pdf,'.pdf'], '-painters', '-q101', '-append','-nocrop')
end
close all
end