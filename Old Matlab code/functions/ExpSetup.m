function ExpSetup
% Performs Experiment and Screen set-up
% Martin Vasilev, 2017

global Visual const Monitor el Audio;

% get participant number:
const.ID= input('Please enter participant number: ');
const.edffilename= [const.expName '' num2str(const.ID) '' '.edf'];
if exist(const.edffilename, 'file')==2 %check if file already exists
    error('Participant file already exists!');
end

% Open screen
oldVisualDebugLevel = Screen('Preference', 'VisualDebugLevel', 3);
oldSupressAllWarnings = Screen('Preference', 'SuppressAllWarnings', 1);
	
% Find out how many screens and use largest screen number.
whichScreen = max(Screen('Screens'));
%whichScreen = 2;

% Setup window:
Monitor.window = Screen('OpenWindow', whichScreen);
Screen(Monitor.window, 'TextSize', Visual.FontSize);
Screen(Monitor.window, 'TextFont', Visual.Font);
Screen('FillRect', Monitor.window, Visual.BGC);
Screen('Flip', Monitor.window);

for i=1:3
   Monitor.buffer(i)= Screen(Monitor.window, 'OpenOffscreenWindow');
end

for i=1:3
   Screen(Monitor.buffer(i), 'TextSize', Visual.FontSize);
   Screen(Monitor.buffer(i), 'TextFont', Visual.Font);
   Screen(Monitor.buffer(i), 'TextStyle', 0); % normal
end

% buffer 3 is for instructions (use larger font):
Screen(Monitor.buffer(3), 'TextSize', Visual.InstrTextSize);

% Open Eyelink connection:
dummymode=0;
el=EyelinkInitDefaults(Monitor.window);

% Initialization of the connection with the Eyelink Gazetracker.
% exit program if this fails.
if ~EyelinkInit(dummymode)
    fprintf('Eyelink Init aborted.\n');
    cleanup;  % cleanup function
    return;
end

% open file to record data to
Eyelink('openfile', const.edffilename);

% Eyelink setup:

% log some details to EDF file:
Eyelink('Message', ['DISPLAY COORDS ' num2str(0) ' ' num2str(0) ' ' num2str(Visual.resX-1) ' ' num2str(Visual.resY-1)]);
Eyelink('Message', ['FRAMERATE ' num2str(Visual.frameRate)]);

% send commands to tracker:
Eyelink('command', ['screen_pixel_coords = 0 0 ' num2str(Visual.resX-1) ' ' num2str(Visual.resY-1)]);
Eyelink('command', ['saccade_velocity_threshold = ' num2str(const.saccvelthresh)]);
Eyelink('command', ['saccade_acceleration_threshold = ' num2str(const.saccaccthresh)]);
Eyelink('command', ['calibration_type= ' '' const.caltype]);
Eyelink('command', 'file_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON');
Eyelink('command', 'file_sample_data  = LEFT,RIGHT,GAZE,AREA,GAZERES,STATUS,HTARGET');
Eyelink('command', 'link_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,BUTTON');
Eyelink('command', 'link_sample_data  = LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS,HTARGET');
Eyelink('command', 'sample_rate= 1000');

end


 