classdef AnnotateAnimationApp < matlab.apps.AppBase
    % App properties and UI components
    
    properties (Access = public)
        % UI components
        UIFigure       matlab.ui.Figure
        StartButton    matlab.ui.control.Button
        PauseButton    matlab.ui.control.Button
        StepButton     matlab.ui.control.Button
        AnnotateButton matlab.ui.control.Button
        TextBox        matlab.ui.control.EditField
        Axes           matlab.ui.control.UIAxes
    end
    
    % Variables for animation and annotations
    properties (Access = private)
        t % Time vector
        data % Animation data (multiple time series)
        frame % Current frame
        annotations % Store annotations
        isPaused % Flag to control pause/play
    end
    
    methods (Access = private)
        
        % Function to initialize the animation data
        function initializeData(app)
            app.t = linspace(0, 10, 100); % Time vector
            app.data = sin(2*pi*app.t) + 0.5 * randn(1, 100); % Example sine wave with noise
            app.frame = 1; % Start at frame 1
            app.annotations = {}; % Initialize annotations
            app.isPaused = false; % Start with animation running
        end
        
        % Function to update the plot for the current frame
        function updatePlot(app)
            plot(app.Axes, app.t(1:app.frame), app.data(1:app.frame), 'b', 'LineWidth', 2);
            hold(app.Axes, 'on');
            
            % Display annotations for the current frame
            for i = 1:length(app.annotations)
                annotation = app.annotations{i};
                if annotation.frame == app.frame
                    text(app.Axes, annotation.time, annotation.value, annotation.text, ...
                        'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right', 'Color', 'red');
                end
            end
            hold(app.Axes, 'off');
            xlim(app.Axes, [0, 10]);
            ylim(app.Axes, [-2, 2]);
            xlabel(app.Axes, 'Time');
            ylabel(app.Axes, 'Value');
        end
        
        % Function to start or resume the animation
        function startAnimation(app)
            app.isPaused = false; % Set to play
            while app.frame <= length(app.t) && ~app.isPaused
                app.updatePlot();
                app.frame = app.frame + 1;
                pause(0.05); % Adjust speed of animation
            end
        end
        
        % Function to pause the animation
        function pauseAnimation(app)
            app.isPaused = true; % Pause the animation
        end
        
        % Function to step forward one frame
        function stepForward(app)
            if app.frame < length(app.t)
                app.frame = app.frame + 1;
                app.updatePlot();
            end
        end
        
        % Function to annotate the current frame
        function annotateCurrentFrame(app)
            annotationText = app.TextBox.Value;
            if ~isempty(annotationText)
                % Store annotation with the current frame, time, and value
                newAnnotation.frame = app.frame;
                newAnnotation.time = app.t(app.frame);
                newAnnotation.value = app.data(app.frame);
                newAnnotation.text = annotationText;
                app.annotations{end+1} = newAnnotation;
            end
        end
        
    end
    
    methods (Access = public)
        
        % Constructor
        function app = AnnotateAnimationApp
            % Create the UI components
            createComponents(app);
            
            % Initialize animation data
            initializeData(app);
            
        end
        
        % Component initialization
        function createComponents(app)
            % Create the main figure
            app.UIFigure = uifigure('Position', [100, 100, 600, 400]);
            
            % Create UIAxes for plotting the animation
            app.Axes = axes(app.UIFigure, 'Position', [0.1, 0.3, 0.8, 0.6]);
            
            % Create Start button
            app.StartButton = uibutton(app.UIFigure, 'Text', 'Start', ...
                'Position', [20, 20, 100, 30], 'ButtonPushedFcn', @(src, event) startAnimation(app));
            
            % Create Pause button
            app.PauseButton = uibutton(app.UIFigure, 'Text', 'Pause', ...
                'Position', [130, 20, 100, 30], 'ButtonPushedFcn', @(src, event) pauseAnimation(app));
            
            % Create Step Forward button
            app.StepButton = uibutton(app.UIFigure, 'Text', 'Step Forward', ...
                'Position', [240, 20, 100, 30], 'ButtonPushedFcn', @(src, event) stepForward(app));
            
            % Create Annotate button
            app.AnnotateButton = uibutton(app.UIFigure, 'Text', 'Annotate', ...
                'Position', [350, 20, 100, 30], 'ButtonPushedFcn', @(src, event) annotateCurrentFrame(app));
            
            % Create TextBox for annotation input
            app.TextBox = uieditfield(app.UIFigure, 'text', 'Position', [470, 20, 100, 30]);
        end
        
    end
end