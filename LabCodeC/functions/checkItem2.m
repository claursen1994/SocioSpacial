function [] = checkItem2(sent, item)
% Displays Item on the screen to check formatting, etc.
%   Press a key to exit window
global Visual Monitor;

settings; % load settings

% get item from sentence frame:
whichRow= find(sent.item== item & sent.cond== 1, 1);
sentenceString= char(sent.stimuli(whichRow));
sentenceString1= strjoin(strsplit(sentenceString, '"'));

whichRow= find(sent.item== item & sent.cond== 2, 1);
sentenceString= char(sent.stimuli(whichRow));
sentenceString2= strjoin(strsplit(sentenceString, '"'));


% open screen:
Screen('Preference', 'SkipSyncTests', 1); 
oldVisualDebugLevel = Screen('Preference', 'VisualDebugLevel', 3);
oldSupressAllWarnings = Screen('Preference', 'SuppressAllWarnings', 1);
	
% Find out how many screens and use largest screen number.
whichScreen = max(Screen('Screens'));

% Setup window:
Monitor.window = Screen('OpenWindow', whichScreen);
Screen(Monitor.window, 'TextSize', Visual.FontSize-5);
Screen(Monitor.window, 'TextFont', Visual.Font);
Screen('FillRect', Monitor.window, Visual.BGC);
Screen('Flip', Monitor.window);

% print sentence on screen:
DrawFormattedText(Monitor.window, sentenceString1, Visual.sentPos(1)-400, Visual.sentPos(2), Visual.FGC, ...
                          [], [], [], Visual.TextSpacing*1.95);
                      
DrawFormattedText(Monitor.window, sentenceString2, Visual.sentPos(1)+600, Visual.sentPos(2), Visual.FGC, ...
                          [], [], [], Visual.TextSpacing*1.95);                      
                       
Screen('Flip', Monitor.window); % present sentence

% wait for keypress to terminate:

KbWait();
Screen('CloseAll');


end

