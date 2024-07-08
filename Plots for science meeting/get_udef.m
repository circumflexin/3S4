% Function to get user-defined meta-data for each tag deployment
% Version 12-April-2024

function x = get_udef(tag)

if strcmp(tag,'oo23_292b')  % Scooby (C311)
    focal = 1;
    ttagon = datenum('19-Oct-2023 21:38');      % tag-on time
    ttagoff = datenum('20-Oct-2023 18:47:15');  % tag-off time
    tstart = datenum('19-Oct-2023 21:37:31');   % reported recording start time
    tbas = datenum('20-Oct-2023 02:00:00');     % baseline period start time (from Bridge log)
    tson = datenum(['20-Oct-2023 10:10:00';...  % actual sonar CEE start/stop times
        '20-Oct-2023 16:46:00']);
    tsgood = 1.203;                             % Hours into recording when good depth data starts
    tegood = NaN;                               % Hours into recording when good depth data ends
    pos_tagon = [70.29322, 20.98697];           % lat/lon of tag-on
    pos_tagrecov = [70.17487, 21.10716];        % lat/lon of tag recovery
    
elseif strcmp(tag,'mn23_293a')  % Homer (C311)
    focal = 2;
    ttagon = datenum('20-Oct-2023 02:35:00');   % tag-on time
    ttagoff = datenum('20-Oct-2023 20:13:19');  % tag-off time
    tstart = datenum('20-Oct-2023 00:35:02');   % reported recording start time
    tbas = datenum('20-Oct-2023 02:00:00');     % baseline period start time (from Bridge log)
    tson = datenum(['20-Oct-2023 10:10:00';...  % actual sonar CEE start/stop times
        '20-Oct-2023 16:46:00']);
    tsgood = 0;                             % Hours into recording when good depth data starts
    tegood = NaN;                               % Hours into recording when good depth data ends
    pos_tagon = [70.303 20.9582];           % lat/lon of tag-on
    pos_tagrecov = [70.303 20.9582];        % lat/lon of tag recovery

elseif strcmp(tag,'oo23_295a')  % Scooby (C311)
    focal = 2;
    ttagon = datenum('22-Oct-2023 08:20:00');      % tag-on time
    ttagoff = datenum('23-Oct-2023 11:07:03');  % tag-off time
    tstart = datenum('22-Oct-2023 08:19:48');   % reported recording start time
    tbas = datenum('22-Oct-2023 09:20:00');     % baseline period start time (from Bridge log)
    tson = datenum(['23-Oct-2023 05:45';...  % actual sonar CEE start/stop times
        '23-Oct-2023 13:45']);
    tsgood = 0;                                 % Hours into recording when good depth data starts
    tegood = 23.6333;                           % Hours into recording when good depth data ends
    pos_tagon = [70.32043, 20.95468];           % lat/lon of tag-on
    pos_tagrecov = [70.3700, 20.0747];        % lat/lon of tag recovery

elseif strcmp(tag,'oo23_295b')  % Elmo (C317)
    focal = 1;
    ttagon = datenum('22-Oct-2023 19:43:00');      % tag-on time
    ttagoff = datenum('23-Oct-2023 20:25:00');  % tag-off time
    tstart = datenum('22-Oct-2023 19:42:41');   % reported recording start time
    tbas = datenum('22-Oct-2023 21:30:00');     % baseline period start time (from Bridge log)
    tson = datenum(['23-Oct-2023 05:45';...  % actual sonar CEE start/stop times
        '23-Oct-2023 13:45']);
    tsgood = 0;                              % Hours into recording when good depth data starts
    tegood = NaN;                               % Hours into recording when good depth data ends
    pos_tagon = [70.29518, 20.84145];           % lat/lon of tag-on
    pos_tagrecov = [70.76227, 20.08];        % lat/lon of tag recovery

elseif strcmp(tag,'oo23_297b')  % Homer(C330)
    focal = 1;
    ttagon = datenum('24-Oct-2023 20:27:00');      % tag-on time
    ttagoff = datenum('25-Oct-2023 21:00:05');  % tag-off time
    tstart = datenum('24-Oct-2023 20:27:00');   % reported recording start time
    tbas = datenum('24-Oct-2023 23:03:43');     % baseline period start time (from Bridge log)
    tson = datenum(['25-Oct-2023 07:20';...  % actual sonar CEE start/stop times
        '25-Oct-2023 15:20']);
    tsgood = 0;                              % Hours into recording when good depth data starts
    tegood = NaN;                               % Hours into recording when good depth data ends
    pos_tagon = [70.5424, 21.43015];           % lat/lon of tag-on
    pos_tagrecov = [70.60808, 19.37787];        % lat/lon of tag recovery

elseif strcmp(tag,'oo23_299a')  %Marge (C330)
    focal = 1;
    ttagon = datenum('26-Oct-2023 21:57:00');      % tag-on time
    ttagoff = datenum('27-Oct-2023 16:46:00');  % tag-off time
    tstart = datenum('26-Oct-2023 20:43:21');   % reported recording start time
    tbas = datenum('27-Oct-2023 00:30:00');     % baseline period start time (from Bridge log)
    tson = datenum(['27-Oct-2023 08:28';...  % actual sonar CEE start/stop times
        '27-Oct-2023 16:28']);
    tsgood = 0;                              % Hours into recording when good depth data starts
    tegood = NaN;                               % Hours into recording when good depth data ends
    pos_tagon = [70.5527, 21.55147];           % lat/lon of tag-on
    pos_tagrecov = [70.51667, 21.52045];        % lat/lon of tag recovery

elseif strcmp(tag,'oo23_299b')  % Simba (C329)
    focal = 0;
    ttagon = datenum('26-Oct-2023 22:12:15');      % tag-on time (changed from tag table to as to not be before tag activation)
    ttagoff = datenum('27-Oct-2023 10:22:57');  % tag-off time 
    tstart = datenum('26-Oct-2023 22:12:15');   % reported recording start time
    tbas = datenum('27-Oct-2023 00:30:00');     % baseline period start time (from Bridge log)
    tson = datenum(['27-Oct-2023 08:28';...  % actual sonar CEE start/stop times
        '27-Oct-2023 16:28']);
    tsgood = 0;                              % Hours into recording when good depth data starts
    tegood = NaN;                               % Hours into recording when good depth data ends
    pos_tagon = [70.5527, 21.5702];           % lat/lon of tag-on
    pos_tagrecov = [70.6832, 21.5863];        % lat/lon of tag recovery

    elseif  strcmp(tag,'oo23_302a')% Marge (C330)
    focal = NaN;
    ttagon = datenum('29-Oct-2023 17:24:00');      % tag-on time
    ttagoff = datenum('30-Oct-2023 11:08:43');  % tag-off time
    tstart = datenum('29-Oct-2023 17:13:32');   % reported recording start time
    tbas = datenum('29-Oct-2023 19:00:00');     % baseline period start time (from Bridge log)
    tson = [NaN; NaN];                          % actual sonar CEE start/stop times
    tsgood = 0;                                 % Hours into recording when good depth data starts
    tegood = NaN;                               % Hours into recording when good depth data ends
    pos_tagon = [70.48296, 21.30765];           % lat/lon of tag-on
    pos_tagrecov = [70.44475, 21.32752];        % lat/lon of tag recovery

    else  %
    error("Tag not in deploy list")
end

assignin('base','focal',focal)
assignin('base','ttagon',ttagon)
assignin('base','ttagoff',ttagoff)
assignin('base','tstart',tstart)
assignin('base','tbas',tbas)
assignin('base','tson',tson)
assignin('base','tsgood',tsgood)
assignin('base','tegood',tegood)
assignin('base','pos_tagon',pos_tagon)
assignin('base','pos_tagrecov',pos_tagrecov)

