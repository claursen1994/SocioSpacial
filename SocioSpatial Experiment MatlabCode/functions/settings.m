global Visual const;

% Visual
Visual.resX= 1920; % Screen resolution, X-axis; P111
Visual.resY= 1080; % Screen resolution, Y-axis; P111
Visual.frameRate= 120; % frame rate of monitor (saved to edf file)
Visual.offsetX= 100; %200; % X offset of the text
Visual.offsetY= 150; % Y offset of the text
Visual.sentPos= [Visual.offsetX Visual.offsetY]; % sentence position
Visual.FGC= [0 0 0]; % stimuli colour
Visual.BGC= [255 255 255]; % background colour
Visual.Pix_per_Letter= 13; % letter width in pixels
Visual.FontSize= 16; % Consolas: 16: 14ppl # Courier New: 18pt: 14ppl; 16pt: 13ppl; 14pt: 11ppl
Visual.TextSpacing= 2; % text spacing between lines (multi-line only)
Visual.LetterHeight= 22; % letter height in pixels  (multi-line only)
Visual.Font= 'Consolas'; %'Courier New';
Visual.TextSize= 18; % possibly unused?
Visual.InstrTextSize= 24; % unused
Visual.GazeBoxSize= 50; % gaze box size in pixels
Visual.GazeBoxColor= [0 0 0]; % colour of the gaze-box
Visual.gazeBoxDur= 100; % how many ms the eye needs to stay on the gaze box before triggering it
Visual.gazeBoxDisplayTime= 7; % how many seconds to wait to trigger the gaze box

% Const (experiment-wide)
const.breakTime= 2*60; % break time in the experiment (in seconds)
const.hasAudio= false; % is there audio in the experiment (used for set-up)
const.TrialTimeout= 3*60; % automatically terminates trial after x seconds
const.ncond= 8; % number of conditions, currently not in use?
const.Maxtrials= 80; % number of experimental trials 
const.seeEye= false; % if true, shows gaze position as a dot on the screen (for testing only!!)
const.checkPPL= false;  % if true, draws a rectangle around sentence to make sure letter width is correct
const.expName = 'SoSpa'; % used for saving edf data (keep at <= 5 letters)
const.caltype= 'HV9'; % calibration type; use 'HV9' for 9-point grid and 'H3' for three-point calibration
const.saccvelthresh = 35; % degrees per second, saccade velocity threshold; don't change
const.saccaccthresh = 9500; % degrees per second, saccade acceleration threshold; don't change
