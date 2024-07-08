% Calculate simple pseudotrack anchored to FGPS positions and tag-on
clear all; close all

% User defined (better to get some of these from the CAL file)
tag = 'oo23_292b';  % Scooby (C311) / 215244
focal = 1;                                  
ttagon = datenum('19-Oct-2023 21:38');      % tag-on time
ttagoff = datenum('20-Oct-2023 18:47:15');  % tag-off time
tstart = datenum('19-Oct-2023 21:37:29');   % reported recording start time
tbas = datenum('20-Oct-2023 02:00:00');     % baseline period start time (from Bridge log)
tson = datenum(['20-Oct-2023 10:10:00';...  % actual sonar CEE start/stop times
                '20-Oct-2023 16:46:00']);
tgood = 1.203;                              % Hours into recording when good depth data starts
pos_tagon = [70.29322, 20.98697];           % lat/lon of tag-on
pos_tagrecov = [70.17487, 21.10716];        % lat/lon of tag recovery


% Load DTAG data (10 Hz)
settagpath('prh','prh\')
loadprh(tag,'p','Aw','Mw','fs')
tutc = (tstart + (0:(1/fs):((length(p)-1)/fs))/86400)'; % 10 Hz time vector
ind = find(tutc<=ttagoff); tutc=tutc(ind); p=p(ind); Aw=Aw(ind,:); Mw=Mw(ind,:); % keep data before tag-off
p(1:(tgood*3600*fs)) = nan; % set bad depths to nan

% Load speed from acceleration jiggle (10 Hz)
psurf = 5; % depth criterion
offset = 0.5; % small difference with DTAG data in this case (check later)
load('speed from jiggle\oo23_292bspeed.mat','speed')
sw = [nan((tgood*3600*fs)-1-(0.5*fs),1); speed.JJ]; % set to nan when depth was bad
sw(p<psurf)=nan; % remove speeds near the surface
clear speed

% Interpolate when speed is nan
inonan = ~isnan(sw);
swint = interp1(tutc(inonan),sw(inonan),tutc,'linear', 1);

% Downsample DTAG data to 1 Hz and recalculate pitch and heading
p = decdc(p, fs);
Aw = decdc(Aw, fs);
Mw = decdc(Mw, fs);
[pitch, roll] = a2pr(Aw);
head = m2h(Mw, pitch, roll);
DECL = 11.94; % magn. declination in degrees
head = head + DECL*pi/180;
swint = downsample(swint, fs); swint=swint(1:end-1);
ttag = downsample(tutc', fs);
ttag = ttag(2:end); % Compensate for time lag caused by decdc
fs = 1;

% Load FGPS data
load('FastGPS\fastgps_export_id_0215144_240223164528.mat')
tgps = datevec(date); tmp=datevec(time); tgps(:,4:6)=tmp(:,4:6);
tgps = datenum(tgps);
pos_gps = [lat,lon];
clear date time lat lon

% Only keep FPGS data during DTAG record
ind = find(tgps>=ttag(1) & tgps<=ttag(end));
tgps = tgps(ind(1:end),1);
pos_gps = pos_gps(ind(1:end),:);
nsats = nsats(ind(1:end),1);

% Remove obvious outliers
ikeep = find(pos_gps(:,1)>70.16 & pos_gps(:,1)<70.32);
ikeep = ikeep(ikeep~=100 & ikeep~=117); % two manually
pos_gps = pos_gps(ikeep,:);
tgps = tgps(ikeep);
nsats = nsats(ikeep);
% plot(pos_gps(:,2),pos_gps(:,1),'o-')

% Combine FGPS fixes with tag-on and convert to UTM
tw = [ttagon; tgps];
posw = [pos_tagon; pos_gps]; 
[yw(:,1),yw(:,2),utmzone] = deg2utm(posw(:,1), posw(:,2));

% Plot uncorrected pseudotrack with position fixes
vwx = swint .* cos(pitch) .* sin(head); % uncorrected x-velocity (m/s)
vwy = swint .* cos(pitch) .* cos(head); % uncorrected y-velocity
ifirst = find(ttagon==ttag); % index of tag-on time
pt = cumsum([vwx(ifirst:end), vwy(ifirst:end)] .* 1/fs); % pseudotrack
% plot(pt(:,1)+yw(1,1),pt(:,2)+yw(1,2),'g-', yw(:,1),yw(:,2),'r.')

% Calculate corrected pseudotrack
vwx2 = vwx;
vwy2 = vwy;
for i=1:length(tw)-1 
    dgps = yw(i+1,:) - yw(i,:); % displacement from gps fixes in m
    dtseg = (tw(i+1) - tw(i))*86400; % segment length in s
    ind = find(ttag>=tw(i) & ttag<tw(i+1));
    ddr = sum([vwx(ind), vwy(ind)] .* 1/fs); % displacement from ptrack in m
    vcor = (dgps-ddr) ./ dtseg; 
    vwx2(ind) = vwx(ind) + repmat(vcor(:,1),length(ind),1); % corrected x-velocity
    vwy2(ind) = vwy(ind) + repmat(vcor(:,2),length(ind),1); % corrected y-velocity
end

ilast = find(tw(end)==ttag); % index of last gps fix
ptcor = cumsum([vwx2(ifirst:ilast), vwy2(ifirst:ilast)] .* 1/fs); % corrected pseudotrack
% hold on; plot(ptcor(:,1)+yw(1,1), ptcor(:,2)+yw(1,2),'k-'); hold off

%% Corrected pseudotrack in lat-lon
nx = 10; % Plot only every nx-th sample (in sec)
poswh=[];
[poswh(:,1), poswh(:,2)] = utm2deg(ptcor(1:nx:end,1)+yw(1,1), ptcor(1:nx:end,2)+yw(1,2), repmat(utmzone(1,:),length(ptcor(1:nx:end,1)),1));
figure(2); plot(poswh(:,2), poswh(:,1),'b-',pos_gps(1,2), pos_gps(1,1),'go',...
    pos_gps(2:end,2), pos_gps(2:end,1),'r.')
xlabel('Longitude (\circE)'); ylabel('Latitude (\circN)'); 

% Fix latlon aspectratio
latlim = get(gca,'YLim'); lonlim = get(gca,'XLim');
latmean = ((latlim(1)+latlim(2))/2);
aspectratio = 1/cos(latmean*(pi/180));
set(gca,'DataAspectRatio',[aspectratio 1 1])


%% Speed and heading from corrected pseudotrack
tcor = ttag(ifirst:ilast)';
vv = diff(p); % vertical velocity
swcor = sqrt((vwx2(ifirst:ilast)).^2 + (vwy2(ifirst:ilast)).^2 + (vv(ifirst:ilast)).^2); % corrected speed in m/s
headcor = atan2(vwy2(ifirst:ilast), vwx2(ifirst:ilast)); 
headcor = wrapToPi(headcor + pi/2); % corrected heading in rad (-pi,pi)

% Horizontal speed, 5-min averaged
swcorh = sqrt((vwx2(ifirst:ilast)).^2 + (vwy2(ifirst:ilast)).^2); % corrected hor. speed in m/s
swcorh = smooth(tcor,swcorh,301); % moving average

figure(3); h(1)=subplot(2,1,1); plot(tcor(1:nx:end),swcor(1:nx:end),'.:','Color',[0.87 0.49 0]); datetick('x',15); xlabel('Time (UTC)'); ylabel('Speed (m/s)')
hold on; plot(tcor(1:nx:end),swcorh(1:nx:end),'k-'); hold off
set(gca,'XLim',[ttag(1), ttag(end)])

subplot(2,1,2); plot(tcor(1:nx:end),rad2deg(headcor(1:nx:end)),'.:','Color',[0 0.5 0]); datetick('x',15); xlabel('Time (UTC)'); ylabel('Direction (\circ)')
set(gca,'Ylim',[-180,180],'YTick',-180:90:180,'XLim',[ttag(1), ttag(end)])

% Save to mat file (thinned to 0.1 Hz; just for plotting)
% twh = tcor(1:nx:end);
% swcor = swcor(1:nx:end);
% swcorh = swcorh(1:nx:end);
% headcor = headcor(1:nx:end);
% tgps = tgps(2:end);               % remove tag-on
% pos_gps = pos_gps(2:end,:);       % remove tag-on
% save(['pseudotrack\' tag '_pt.mat'],'twh','poswh','tgps','pos_gps','swcor','swcorh','headcor')

