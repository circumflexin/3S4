% if strcmp(tag,'oo23_292a')  
%     ttagon = datenum('19-Oct-2023 23:36:00');      % tag-on time
%     ttagoff = datenum('19-Oct-2023 23:48:14');  % tag-off time
%     tstart = datenum('19-Oct-2023 21:26:23');   % reported recording start time
%     tbas = datenum('');     % baseline period start time (from Bridge log)
%     tson = datenum(['';...  % actual sonar CEE start/stop times
%         '']);
%     tgood = ;                              % Hours into recording when good depth data starts
%     pos_tagon = [, ];           % lat/lon of tag-on
%     pos_tagrecov = [, ];        % lat/lon of tag recovery

if strcmp(tag,'oo23_292b')  % Scooby (C311)
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

elseif strcmp(tag,'oo23_295a')  %
    focal = ;
    ttagon = datenum('22-Oct-2023 10:20:00');      % tag-on time
    ttagoff = datenum('23-Oct-2023 13:07:03');  % tag-off time
    tstart = datenum('22-Oct-2023 08:19:48');   % reported recording start time
    tbas = datenum('');     % baseline period start time (from Bridge log)
    tson = datenum(['';...  % actual sonar CEE start/stop times
        '']);
    tgood = 0;   %stops at 24h                     % Hours into recording when good depth data starts
    pos_tagon = [70.32043, 20.95468];           % lat/lon of tag-on
    pos_tagrecov = [70.3700, 20.0747];        % lat/lon of tag recovery


elseif strcmp(tag,'oo23_295b')  %
    focal = ;
    ttagon = datenum('22-Oct-2023 21:43:00');      % tag-on time
    ttagoff = datenum('23-Oct-2023 22:25:00');  % tag-off time
    tstart = datenum('22-Oct-2023 19:42:41');   % reported recording start time
    tbas = datenum('');     % baseline period start time (from Bridge log)
    tson = datenum(['';...  % actual sonar CEE start/stop times
        '']);
    tgood = 0;                              % Hours into recording when good depth data starts
    pos_tagon = [70.29518, 20.84145];           % lat/lon of tag-on
    pos_tagrecov = [70.76227, 20.08];        % lat/lon of tag recovery

elseif strcmp(tag,'oo23_297a')  %
    focal = ;
    ttagon = datenum('24-Oct-2023 22:27:00');      % tag-on time
    ttagoff = datenum('25-Oct-2023 23:00:05');  % tag-off time
    tstart = datenum('24-Oct-2023 20:27:00');   % reported recording start time
    tbas = datenum('');     % baseline period start time (from Bridge log)
    tson = datenum(['';...  % actual sonar CEE start/stop times
        '']);
    tgood = ;                              % Hours into recording when good depth data starts
    pos_tagon = [];           % lat/lon of tag-on
    pos_tagrecov = [];        % lat/lon of tag recovery

elseif strcmp(tag,'')  %
    focal = ;
    ttagon = datenum('');      % tag-on time
    ttagoff = datenum('');  % tag-off time
    tstart = datenum('');   % reported recording start time
    tbas = datenum('');     % baseline period start time (from Bridge log)
    tson = datenum(['';...  % actual sonar CEE start/stop times
        '']);
    tgood = ;                              % Hours into recording when good depth data starts
    pos_tagon = [];           % lat/lon of tag-on
    pos_tagrecov = [];        % lat/lon of tag recovery

end
