% Presentation of Experimental trials
% Martin Vasilev, 2017

% init
global const Visual sent Monitor el;

% hide the mouse cursor
HideCursor; 

% Calibrate the eye tracker
EyelinkDoTrackerSetup(el);
% deComment if testing, Comment if not testing 
%  const.ntrials= 1;

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
   

    P1_text= char(sent.P1(whichRow));
    P1_text = format_text(P1_text , Visual.resX, Visual.Pix_per_Letter, Visual.offsetX);
    P2_text= char(sent.P2(whichRow));
    P2_text = format_text(P2_text , Visual.resX, Visual.Pix_per_Letter, Visual.offsetX);
    
    if strcmp(P1_text(length(P1_text)-1:end), '\n')
        sentenceString= [P1_text '\n' P2_text];
    else
        sentenceString= [P1_text '\n\n' P2_text];
    end
    
    
%     sentenceString = char(sent.Stimulus(whichRow));
%     sentenceString = strjoin(strsplit(sentenceString, '"'));
%     sentenceString = format_text(sentenceString , Visual.resX, Visual.Pix_per_Letter, Visual.offsetX);
    %Alternative presentation
%     sentenceString= char(sent.P1(whichRow));
%     sentenceString=strjoin(strplit(sentenceString,'"'));
%     sentenceString=format_text(sentenceString, Visual.resX, Visual.Pix_per_Letter, Visual.offsetX);
%     
%     sentenceString2=char(sent.P2(whichRow));
%     sentenceString2=strjoin(strplit(sentenceString2,'"'));
%     sentenceString2=format_text(sentenceString2, Visual.resX, Visual.Pix_per_Letter, Visual.offsetX);
%     
%     sentenceString=strjoin(sentenceString,sentenceString2)
    % drift check:
    EyelinkDoDriftCorrection(el);
    
    % Eyelink & Screen trial set-up:
	stimuliOn= false;
    
    while ~stimuliOn
        %if practice
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
    options= [ '1)  ' char(Quest.Q1o1(my_indexer)) '/n' '2)  ' char(Quest.Q1o2(my_indexer)) '/n' '3)  ' char(Quest.Q1o3(my_indexer))];
    options= strjoin(strsplit(options, '"'));    
    question= char(Quest.Q1(my_indexer));
%     question= strjoin(strsplit(question, '"'));
%     question= format_text(question , Visual.resX, Visual.Pix_per_Letter, Visual.offsetX);
    question= strjoin(strsplit(question, '"'));

    answer1= QuestionMC(question, strsplit(options, '/n'), Quest.Q1corr_ans(my_indexer), item, cond, 1);
    
    % Question 2 ? for eyelink?
    options= [ '1)  ' char(Quest.Q2o1(my_indexer)) '/n' '2)  ' char(Quest.Q2o2(my_indexer)) '/n' '3)  ' char(Quest.Q2o3(my_indexer))];
    options= strjoin(strsplit(options, '"'));     
    question= char(Quest.Q2(my_indexer));
    question= strjoin(strsplit(question, '"'));
%     question= format_text(question , Visual.resX, Visual.Pix_per_Letter, Visual.offsetX);
%     question= strjoin(strsplit(question, '"'));
    answer2= QuestionMC(question, strsplit(options, '/n'), Quest.Q2corr_ans(my_indexer), item, cond, 2);
    
    % Question 2 ? for eyelink?
    options= [ '1)  ' char(Quest.Q3o1(my_indexer)) '/n' '2)  ' char(Quest.Q3o2(my_indexer)) '/n' '3)  ' char(Quest.Q3o3(my_indexer))];
    options= strjoin(strsplit(options, '"'));    
    question= char(Quest.Q3(my_indexer));
    question= strjoin(strsplit(question, '"'));
%     question= format_text(question , Visual.resX, Visual.Pix_per_Letter, Visual.offsetX);
%     question= strjoin(strsplit(question, '"'));
    answer3= QuestionMC(question, strsplit(options, '/n'), Quest.Q3corr_ans(my_indexer), item, cond, 3);
    
    end


% end of Experiment text:
text= 'The experiment is finished! Thank you for participating!';
Screen(Monitor.window, 'TextSize', Visual.InstrTextSize);
Screen('DrawText', Monitor.window, text, Visual.resX/2- (Visual.Pix_per_Letter+3)*length(text)/2, Visual.resY/2, [139, 0, 0]);
Screen('Flip', Monitor.window);
