% Generates a timeseries plot and map for deployment oo23_292b.
% These plots are preliminary, made with the purpose of discussing the
% format during the 3S meeting on 26-27 Feb 2024.
clear all; close all

% User defined
tag = 'oo23_297b';  
get_udef(tag)

% Fake fishing vessels
posf=[70.2992,20.9389;70.2657,20.9769;70.2912,21.0616;70.2877,21.0726;...
    70.2397,20.8663;70.1856,20.8372;70.2102,21.0436];

%% Load input data

% Load DTAG data (10 Hz)
settagpath('prh','prh\','cal','cal\')
loadprh(tag,'fs','p','pitch','head')

%check any detals from calfile
[CAL,DEPLOY,ufname] = d3loadcal(tag)
datestr(DEPLOY.SCUES.TIME(1,:)) 


tutc = (tstart + (0:(1/fs):((length(p)-1)/fs))/86400)'; % 10 Hz time vector
ind = find(tutc<=ttagoff); tutc=tutc(ind); p=p(ind); pitch=pitch(ind); head=head(ind); % keep data before tag-off
p(1:(tgood*3600*fs)) = nan; % set bad depths to nan


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

% Load speed from jiggle (10 Hz)
psurf = 5; % depth criterion
offset = 0.5; % small difference with DTAG data in this case (check later)
load(['speed from jiggle\',tag,'speed.mat'],'speed')
sw = [nan((tgood*3600*fs)-1-(0.5*fs),1); speed.JJ]; % set to nan when depth was bad
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
load(['prelim audits\' tag 'aud.mat'], 'taudit','duraudit','code')
% Indices for sound types to plot (better to fix the codes at an earlier stage):
ics = strncmpi(code,'CS 12',5) | strncmpi(code,'CS 2',4) | strncmpi(code,'CS 3',4);
itailslap = strncmpi(code,'TAILSLAP',8); % currently includes all qualities (preliminary)
ihfw = strncmpi(code,'HFWHISTLE',9); % there aren't any right now
iss = strncmpi(code,'SS 12',5) | strncmpi(code,'SS 2',4) | strncmpi(code,'SS 3',4)| strncmpi(code,'SS2',3);
iss = iss | strncmpi(code,'WHISTEL 2',9) | strncmpi(code,'WHISTLE 2',9) | strncmpi(code,'WHSITLE 2',9) | strncmpi(code,'WHISTLE 3',9) | strncmpi(code,'WHSITLE 3',9);

% % TEMP: Plot audited breaths with dive profile
% % Looks like a small time-alignment issue towards the end...
% figure(99); plot(tutc,p); set(gca,'YDir','reverse'); datetick('x')
% ibr = strncmpi(code,'BR',2);
% hold on; plot(taudit(ibr),1,'ro'); hold off

% Load manmade noise audits
load(['noise_audits\' tag 'aud_noise.mat'], 'taudit_noise','inoise')
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

% Load pseudotrack 
load(['pseudotrack\',tag,'_pt.mat']) % this includes time vectors twh and tgps
headcor = rad2deg(headcor);
turnanglecor = [wrapTo180(diff(headcor)); nan];

% Load HUS GPS data
d = dir('GPS_AIS\*Sverdrup.mat');
df = 30; % resolution (in s)
for i=1:length(d)
    load([d(i).folder,'\',d(i).name])
    if i==1
        ts = gpsData.time(1:df:end);
        poss = [gpsData.lat_deg(1:df:end), gpsData.lon_deg(1:df:end)];
    else
        ts = [ts; gpsData.time(1:df:end)];
        poss = [poss; gpsData.lat_deg(1:df:end), gpsData.lon_deg(1:df:end)];
    end
end
ts = datenum(ts); % convert datetime to datenum (just because I don't know datetime)

% Vessel-whale distance
ellipsoid = almanac('earth','wgs84','meters');
dswh = nan(length(ts),1);  %  distance to HUS
dnfwh = nan(length(ts),2); %  TEMP: distance to nearest fishing vessel
for i=1:length(ts)
    [tmp,ind] = min(abs(ts(i)-twh));
    [dswh(i,1),tmp] = distance(poswh(ind,:), poss(i,:), ellipsoid);
    [dtmp(1),tmp] = distance(posf(1,:), poss(i,:), ellipsoid);
    [dtmp(2),tmp] = distance(posf(2,:), poss(i,:), ellipsoid);
    [dtmp(3),tmp] = distance(posf(3,:), poss(i,:), ellipsoid);
    [dtmp(4),tmp] = distance(posf(4,:), poss(i,:), ellipsoid);
    [dtmp(5),tmp] = distance(posf(5,:), poss(i,:), ellipsoid);
    [dtmp(6),tmp] = distance(posf(6,:), poss(i,:), ellipsoid);
    [dtmp(7),tmp] = distance(posf(7,:), poss(i,:), ellipsoid);
    [dnfwh(i,1),dnfwh(i,2)] = min(dtmp); % [minimum distance, vessel index];
end


%% Map
sdf = df/(1/fs); % resolution difference HUS and whale track
figure(1); colormap('colorcube'); 

for i=1:2 % loop over two subplots
subplot(1,2,i); box on;

% Plot HUS track
if i==2 % color gradient only in panel 2
    [tmp, is] = min(abs(ts-tson(1))); [tmp, ie] = min(abs(ts-tson(2)));
    hc1 = colline(poss(is:ie,2),poss(is:ie,1),1:(ie-is+1));
    set(hc1,'LineWidth',6,'Marker','.','MarkerSize',18);
end
ind = ts>=datenum(tson(1)) & ts<=datenum(tson(2)); 
hold on; plot(poss(ind,2),poss(ind,1),'k:','LineWidth',0.5)
ind = find(ts>=datenum(tson(1)) & ts<=datenum(tson(2))); 

% Plot whale track
if i==2 % Only in panel 2:
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
    plot(pos_gps(:,2), pos_gps(:,1),'r.');     % plot gps locations
end

% Fit window of panel 2 to whale track
if i==2
    set(gca,'XLim',[min(poswh(:,2)), max(poswh(:,2))],...
        'YLim',[min(poswh(:,1)), max(poswh(:,1))])
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

%% Timeseries plot
figure(3); colormap('colorcube'); 

%%%%%%%%% Manmade noise panel
h(1)=subplot(8,1,1); 
plot(tutc,inoise2,'LineWidth',2); datetickzoom('x',15)

%%%%%%%%% Vessel distance panel
h(2)=subplot(8,1,2);
ind = ts>=tson(1) & ts<=tson(2);
plot(ts(ind),dswh(ind)/1000,'k-','LineWidth',1); % distance to HUS
hold on; plot(tutc([1 end]),[.5 .5],'k:',tutc([1 end]),[1 1],'k:'); datetickzoom('x',15) 
ylabel({'Vessel','distance (km)'})

% Distance to fake stationary fishing vessels (temporary)
plot(ts,dnfwh(:,1)/1000,'-','Color','#2AAA8A','LineWidth',1); 
ichange = find(diff(dnfwh(:,2))>0);
plot(ts(ichange),dnfwh(ichange,1)/1000,'+','LineWidth',1);

%%%%%%%%% Speed panel
h(3)=subplot(8,1,3); plot(twh,swcor,'.:','Color',[.6 .6 .6]); datetickzoom('x',15)
hold on; plot(twh,swcorh,'k-','LineWidth',1); hold off
ylabel('Speed (ms^{-1})')

%%%%%%%%% Heading and turning angle panel
h(4)=subplot(8,1,4); plot(thead,turnangle,'-','Color','#FFA500','LineWidth',1);
hold on; plot(thead,headm,'k.:','LineWidth',0.5); datetickzoom('x',15)
ylabel({'Heading (\circ)','Turn angle (\circ)'})

%%%%%%%%% Dive profile panel
h(5)=subplot(8,1,5:8);

% Plot color gradient
yoffset = 80;
[tmp, is] = min(abs(tutc-tson(1))); [tmp, ie] = min(abs(tutc-tson(2)));
hc3 = colline(tutc(is:ie,1),ones(ie-is+1,1)*yoffset,1:(ie-is+1));
set(hc3,'LineWidth',6,'Marker','.','MarkerSize',8);
xlabel('Time (UTC)'); ylabel('Depth (m)'); box on

% Plot sun elevation angle
[tmp,SunAngle] = SolarAzEl(tutc,pos_tagon(1),pos_tagon(2),0);
ind = SunAngle>-6; % Only show civil twilight or higher
hold on; plot(tutc(ind),SunAngle(ind)+yoffset,'-',...
    tutc([1 end]),[yoffset yoffset],':','Color',[.6 .6 .6])
tsun0 = tutc(find(abs(diff(SunAngle>=0))==1)); % times of sunrise and sunset

% plot dive profile
plot(tutc,p,'k-','LineWidth',1);  datetickzoom('x',15)
plot(tutc([1 end]),[0,0],':','Color',[.6 .6 .6])

% plot social sounds above the dive profile
tss = taudit(iss) + (duraudit(iss)/2)/86400; % one marker in the middle
plot(tss,ones(length(tss),1)*-5,'b.'); 

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

% plot a few FAKE hf whistles (remove later!)
plot(ttailslap(7:11)+(1/24),ones(5,1)*-5,'s','Color','#2AAA8A','LineWidth',2); 

hold off

%%%%%%%%% Plot start/end of experimental phases
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
    plot([tsun0(1),tsun0(1),nan,tsun0(2),tsun0(2)],[ylow(i),180,nan,ylow(i),180],':','Color',[.6 .6 .6],'LineWidth',0.5)
    hold off

    % Set axis properties
    if i==1
        set(gca,'YLim',[0.5,5.5],'YTick',1:5,'YTickLabel',noise_labels,...
            'XTickLabel','')
    elseif i==2
        set(gca,'YLim',[0.2,20],'YTick',[0.5 1 2 5 10],'XTickLabel','',...
            'YScale','log','YDir','reverse')
    elseif i==3
        set(gca,'YLim',[0,3],'YTick',[0 1 2 3],'XTickLabel','')
    elseif i==4
        set(gca,'YLim',[-180,180],'YTick',[-150 0 150],'XTickLabel','')
    elseif i==5
        set(gca,'YLim',[-9, max(max(p)+5, 95)],'YDir','reverse')
    end
end
set(gca,'XLim',[ttagon,ttagoff]) % show from tag-on to tag-off

% Zoom in on other time periods:
% set(gca,'XLim',[tbas,tson(1)]) % baseline period
% set(gca,'XLim',[tson(1)-0.5/24,tson(2)+0.5/24]) % sonar period +/- 30 min

% set(gcf,'color','w');
% export_fig([name_pdf,'.pdf'], '-painters', '-q101', '-append','-nocrop')

