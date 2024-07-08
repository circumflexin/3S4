if strcmp(tag,'oo23_292a')  
    ttagon = datenum('19-Oct-2023 23:36:00');      % tag-on time
    ttagoff = datenum('19-Oct-2023 23:48:14');  % tag-off time
    tstart = datenum('19-Oct-2023 21:26:23');   % reported recording start time
    tbas = datenum('');     % baseline period start time (from Bridge log)
    tson = datenum(['';...  % actual sonar CEE start/stop times
        '']);
    tgood = ;                              % Hours into recording when good depth data starts
    pos_tagon = [, ];           % lat/lon of tag-on
    pos_tagrecov = [, ];        % lat/lon of tag recovery

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

elseif strcmp(tag,'')  %
    focal = ;
    ttagon = datenum('');      % tag-on time
    ttagoff = datenum('');  % tag-off time
    tstart = datenum('');   % reported recording start time
    tbas = datenum('');     % baseline period start time (from Bridge log)
    tson = datenum(['';...  % actual sonar CEE start/stop times
        '']);
    tgood = ;                              % Hours into recording when good depth data starts
    pos_tagon = [, ];           % lat/lon of tag-on
    pos_tagrecov = [, ];        % lat/lon of tag recovery


end
