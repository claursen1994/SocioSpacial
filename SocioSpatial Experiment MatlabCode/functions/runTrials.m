% Presentation of Experimental trials
% Martin Vasilev, 2017

% init
global const Visual sent Monitor el;

% hide the mouse cursor
HideCursor; 

% Calibrate the eye tracker
EyelinkDoTrackerSetup(el);

% Trial presentation loop:
for i=1:const.ntrials
    % stimuli set-up:
    trialEnd= false; 
	item= design(i,1); % item is 1st column
    cond= design(i,2); % condition is 2nd column
    
    if cond< 5 % Social 1st (cond 1 - 4)
        whichRow= find(sent.item== item & sent.cond== cond, 1);
    else % Spatial 1st (cond 5 - 8)
        whichRow= find(sent.item== item & sent.cond== cond, 1); 
    end
   
    sentenceString = char(sent.Stimulus(whichRow));
    sentenceString = strjoin(strsplit(sentenceString, '"'));
    sentenceString = format_text(sentenceString , Visual.resX, Visual.Pix_per_Letter, Visual.offsetX);

    % drift check:
    EyelinkDoDriftCorrection(el);
    
    % Eyelink & Screen trial set-up:
	stimuliOn= false;
    
    while ~stimuliOn
        if practice
            if item> const.Maxtrials 
                Eyelink('Message', ['TRIALID ' 'P' num2str(cond) 'I' num2str(item) 'D0']);
                % print trial ID on tracker screen:
                Eyelink('command', ['record_status_message ' [ num2str(round((i/const.ntrials)*100)) 'Prcnt:' 'P' num2str(cond) 'I' num2str(item) 'D0']]);
            else
                Eyelink('Message', ['TRIALID ' 'E' num2str(cond) 'I' num2str(item) 'D0']);
                % print trial ID on tracker screen:
                Eyelink('command', ['record_status_message ' [ num2str(round((i/const.ntrials)*100)) 'Prcnt:' 'E' num2str(cond) 'I' num2str(item) 'D0']]); 
            end
        
        stim2edfML(sentenceString); % Multi-line
        
        % prepare Screens:
        Screen('FillRect', Monitor.buffer(2), Visual.BGC);
              
        % Draw the paragraph to back buffer
        DrawFormattedText(Monitor.buffer(2), sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC, [], [], [], Visual.TextSpacing*1.95);
        
        % don't know what PPL is
        if const.checkPPL
            MLcheck= strfind(sentenceString, '\n');
            
            if ~isempty(MLcheck)
                sentenceString= strrep(sentenceString, '\n', '@');
                sentenceString= strsplit(sentenceString, '@');
                sentenceString= char(sentenceString{1});
            end
            
			lngth= length(sentenceString)*Visual.Pix_per_Letter;
            Screen('FrameRect', Monitor.buffer(2), Visual.FGC, ...
                [Visual.offsetX Visual.offsetY- Visual.GazeBoxSize/2 ...
                Visual.offsetX+lngth Visual.offsetY+ Visual.GazeBoxSize]);
        end
        
        % Print stimuli to Eyelink monitor:
        % draw gaze box on tracker monitor:
        sendScreenshot(Monitor.buffer(2));
        
        % Present Gaze-box:
        stimuliOn= gazeBox(stimuliOn);
    end
    
    %% Present text stimuli:
  
    Screen('CopyWindow', Monitor.buffer(2), Monitor.window);
    Screen('Flip', Monitor.window); % present paragraph
    Eyelink('Message', 'DISPLAY ON');
    Eyelink('Message', 'SYNCTIME');

	trialStart= GetSecs;
    
    while ~trialEnd
        trialTime= GetSecs- trialStart;
        [x,y,buttons] = GetMouse(Monitor.window);
        trialEnd= buttons(1); %KbCheck; 
        
        if const.seeEye % for testing only (enable code above)
            Screen('FillRect', Monitor.window, Visual.BGC);
            Screen('DrawText', Monitor.window, sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence
            Screen('DrawDots', Monitor.window, [xpos, 540], 10, [0 0 0], [],2);
            Screen('Flip', Monitor.window);
        end
        
        % end trial automatically if no response by participant
        if trialTime> const.TrialTimeout 
             trialEnd= true;
             tracker.log('TRIAL ABORTED')
 			 Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
             Screen('Flip', Monitor.window);
        end
        
    end	
	
	% end of trial messages:
    Eyelink('Message', 'ENDBUTTON 5');
    Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
    Screen('Flip', Monitor.window);
    Eyelink('Message', 'DISPLAY OFF');
    Eyelink('Message', 'TRIAL_RESULT 5');
    Eyelink('Message', 'TRIAL OK');
    Eyelink('StopRecording');
    Eyelink('command', 'clear_screen 0'); % clear tracker screen

    my_indexer = ((item-1) * 8) + cond; % my_indexer picks the row
    
    % Question 1 ? for eyelink?
    options= [ '1)  ' char(Quest.Q1O1(my_indexer)) '/n' '2)  ' char(Quest.Q1O2(my_indexer)) '/n' '3)  ' char(Quest.Q1O3(my_indexer))];
    options= strjoin(strsplit(options, '"'));    
    question= char(Quest.Q1(my_indexer));
    question= strjoin(strsplit(question, '"'));
    answer= QuestionMC(question, strsplit(options, '/n'), Quest.Q1corr_ans(my_indexer), item, cond, 1);
    
    % Question 2 ? for eyelink?
    options= [ '1)  ' char(Quest.Q2O1(my_indexer)) '/n' '2)  ' char(Quest.Q2O2(my_indexer)) '/n' '3)  ' char(Quest.Q2O3(my_indexer))];
    options= strjoin(strsplit(options, '"'));     
    question= char(Quest.Q2(my_indexer));
    question= strjoin(strsplit(question, '"'));
    answer= QuestionMC(question, strsplit(options, '/n'), Quest.Q1corr_ans(my_indexer), item, cond, 1);
    
    % Question 2 ? for eyelink?
    options= [ '1)  ' char(Quest.Q3O1(my_indexer)) '/n' '2)  ' char(Quest.Q3O2(my_indexer)) '/n' '3)  ' char(Quest.Q3O3(my_indexer))];
    options= strjoin(strsplit(options, '"'));    
    question= char(Quest.Q3(my_indexer));
    question= strjoin(strsplit(question, '"'));
    answer= QuestionMC(question, strsplit(options, '/n'), Quest.Q1corr_ans(my_indexer), item, cond, 1);

end

% end of Experiment text:
text= 'The experiment is finished! Thank you for participating!';
Screen(Monitor.window, 'TextSize', Visual.InstrTextSize);
Screen('DrawText', Monitor.window, text, Visual.resX/2- (Visual.Pix_per_Letter+3)*length(text)/2, Visual.resY/2, [139, 0, 0]);
Screen('Flip', Monitor.window);
