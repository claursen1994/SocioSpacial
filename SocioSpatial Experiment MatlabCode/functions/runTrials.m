% Presentation of Experimental trials
% Martin Vasilev, 2017

global const Visual sent Monitor el Audio; 

% Blackbox toolkit testing:
%s= serial('COM11');
%set(s, 'BaudRate', 115200, 'DataBits', 8, 'StopBits', 1, 'Parity', 'none')
%fopen(s);
%fprintf(s, 'RR');
%fprintf(s,'FF');

%const.ntrials=3; % TEMPORARY!!! Use only for testing

HideCursor; % hide the mouse cursor
  
% Calibrate the eye tracker
EyelinkDoTrackerSetup(el);

% Trial presentation loop:
for i=1:const.ntrials
    
    %calResult = Eyelink('CalResult');
%%  stimuli set-up:
    trialEnd= false; 
	item= design(i,1); % item is 1st column
    cond= design(i,2); % condition is 2nd column
    
    if cond< 3 % high frequency (cond 1 and 2)
      % get sent string:
        whichRow= find(sent.item== item & sent.cond== 1, 1);
    else % low frequency conditions (3 and 4)
       whichRow= find(sent.item== item & sent.cond== 2, 1); 
    end
   
    sentenceString= char(sent.stimuli(whichRow));
    sentenceString= strjoin(strsplit(sentenceString, '"'));
    % Block instructions:
    if i== length(design)-const.Maxtrials+1 || i== length(design)-const.Maxtrials/2+1
        
        if ismember(cond, [1, 3])
            instrText= const.SilentInstr; % 1 & 3
            blockStr= 'SILENT';
        else
            instrText= const.AloudInstr; % 2 & 4
            blockStr= 'ALOUD';
        end
        
        Eyelink('Message', [blockStr ' BLOCK INSTRUCTION PRESENTED']);
        
        DrawFormattedText(Monitor.buffer(3), instrText, Visual.sentPos(1), ...
            Visual.sentPos(2) + 250, [139, 0, 0], ...
            [], [], [], Visual.TextSpacing*1.95);
        
        text=  'Please click the mouse to continue!';
        
        Screen('DrawText', Monitor.buffer(3), text,  Visual.sentPos(1), Visual.resY/1.4, Visual.FGC);
        Screen('CopyWindow', Monitor.buffer(3), Monitor.window);
        Screen('Flip', Monitor.window);
        sendScreenshot(Monitor.buffer(3));
        
        InstrDone= false;
        ShowCursor(0); 
        
        while ~InstrDone
            [x,y,buttons] = GetMouse(Monitor.window);
            InstrDone= buttons(1); %KbCheck; 
        end
        HideCursor
        
        % clear main screen:
        Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
        Screen('FillRect', Monitor.buffer(3), Visual.BGC); % clear subject screen
        Screen('Flip', Monitor.window);
        
    end
    
	% get image dir:
    %imageFile= ['img/Item' num2str(item) '_Cond' num2str(cond) '.bmp'];
    %img= imread(imageFile); % read in image
    
    % recording dir:
    if ismember(cond, [2,4])
        Audio.filename= ['Audio/S' num2str(const.ID) 'E' num2str(cond) 'I' num2str(item) '.wav'];
    end
       
    % drift check:
    EyelinkDoDriftCorrection(el);
    
    %% Eyelink & Screen trial set-up:
	stimuliOn= false;
    
    while ~stimuliOn
        if item> const.Maxtrials % if practice
            Eyelink('Message', ['TRIALID ' 'P' num2str(cond) 'I' num2str(item) 'D0']);
			% print trial ID on tracker screen:
            Eyelink('command', ['record_status_message ' [ num2str(round((i/const.ntrials)*100)) 'Prcnt:' 'P' num2str(cond) 'I' num2str(item) 'D0']]);
        else
			Eyelink('Message', ['TRIALID ' 'E' num2str(cond) 'I' num2str(item) 'D0']);
			% print trial ID on tracker screen:
			Eyelink('command', ['record_status_message ' [ num2str(round((i/const.ntrials)*100)) 'Prcnt:' 'E' num2str(cond) 'I' num2str(item) 'D0']]); 
        end
        
        % print image url to edf:
        %Eyelink('Message', imageFile);

        % print text stimuli to edf:
        %stim2edf(sentenceString); % Single line
        stim2edfML(sentenceString); % Multi-line
        
        % prepare Screens:
        % sentence presentation:
        Screen('FillRect', Monitor.buffer(2), Visual.BGC);
        % put image on the screen:
        %Screen('PutImage', Monitor.buffer(2), img);
       
        % Use this for printing text as a string:
        %Screen('DrawText', Monitor.buffer(2), sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence
        DrawFormattedText(Monitor.buffer(2), sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC, ...
                          [], [], [], Visual.TextSpacing*1.95);
        
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
        
        %% Present Gaze-box:
        stimuliOn= gazeBox(stimuliOn);
        
    end
    
    %% Present text stimuli:
    Eyelink('Message', 'GAZE TARGET OFF');
    if ismember(cond, [2,4])
        % start recording audio
        Eyelink('Message', 'INITIALISE MICROPHONE');
        PsychPortAudio('Start', Audio.pahandle, 0, 0, 1);
        Eyelink('Message', 'MICROPHONE ON');
    end
 
    Screen('CopyWindow', Monitor.buffer(2), Monitor.window);
    Screen('Flip', Monitor.window); % present sentence
    Eyelink('Message', 'DISPLAY ON');
    Eyelink('Message', 'SYNCTIME');

	trialStart= GetSecs;
    
    while ~trialEnd
        trialTime= GetSecs- trialStart;
        [x,y,buttons] = GetMouse(Monitor.window);
        trialEnd= buttons(1); %KbCheck; 
        
        % use this for gaze-contingent manipulations:
        %evt= Eyelink('NewestFloatSample');
        %xpos = evt.gx(2);

        
        if const.seeEye % for testing only (enable code above)
            Screen('FillRect', Monitor.window, Visual.BGC);
            Screen('DrawText', Monitor.window, sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence
            Screen('DrawDots', Monitor.window, [xpos, 540], 10, [0 0 0], [],2);
            Screen('Flip', Monitor.window);
        end
        
        % end trial automatically if no response by participant
        if trialTime> const.TrialTimeout 
             trialEnd= true;
             %tracker.log('TRIAL ABORTED')
 			 Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
             Screen('Flip', Monitor.window);
        end
        
    end	
	
	% end of trial messages:
    Eyelink('Message', 'ENDBUTTON 5');
    Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
    Screen('Flip', Monitor.window);
    Eyelink('Message', 'DISPLAY OFF');
    
    if ismember(cond, [2,4])
        % Stop Audio capture:
        Eyelink('Message', 'STOP MICROPHONE');
        PsychPortAudio('Stop', Audio.pahandle);
        Eyelink('Message', 'MICROPHONE OFF');
    end
    
    Eyelink('Message', 'TRIAL_RESULT 5');
    Eyelink('Message', 'TRIAL OK');
    
    Eyelink('StopRecording');
    
    Eyelink('command', 'clear_screen 0'); % clear tracker screen
    
    %% save recorded audio:
    
    if ismember(cond, [2,4])
        % Get sound data from the capture engine:
        audiodata = PsychPortAudio('GetAudioData', Audio.pahandle);
        % before saving, we need to transpose matrix, otherwise matlab will
        % complain:
        audiodata= audiodata';
        audiowrite(Audio.filename, audiodata, Audio.freq);
    end
    
     %% Questioms:
    
     options= [ '1)  ' char(quest.O1(item)) '/n' '2)  ' char(quest.O2(item)) ...
               '/n' '3)  ' char(quest.O3(item)) '/n' '4)  ' char(quest.O4(item))];
     options= strjoin(strsplit(options, '"'));    
     
     question= char(quest.Q(item));
     question= strjoin(strsplit(question, '"'));
     
     % present question:
      answer= QuestionMC(question, strsplit(options, '/n'), ...
        quest.corr_answ(item), item, cond, 1);
    
  %% Abbrev recognition questions:
 
  if ismember(i,abb_items)
     %Present instructions
     Eyelink('Message', 'Abbreviations instruction presented');
            
        
        DrawFormattedText(Monitor.buffer(3), const.abbInstr, Visual.sentPos(1), ...
            Visual.sentPos(2) + 250, [139, 0, 0], ...
            [], [], [], Visual.TextSpacing*1.95);
        
        text=  'Please click the mouse to continue!';
        
        Screen('DrawText', Monitor.buffer(3), text,  Visual.sentPos(1), Visual.resY/1.4, Visual.FGC);
        Screen('CopyWindow', Monitor.buffer(3), Monitor.window);
        Screen('Flip', Monitor.window);
        sendScreenshot(Monitor.buffer(3));
        
        InstrDone= false;
        ShowCursor(0); 
        
        while ~InstrDone
            [x,y,buttons] = GetMouse(Monitor.window);
            InstrDone= buttons(1); %KbCheck; 
        end
        HideCursor
        
        % clear main screen:
        Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
        Screen('FillRect', Monitor.buffer(3), Visual.BGC); % clear subject screen
        Screen('Flip', Monitor.window);
        
        %Present the abbreviation questions
        for j=0:const.Maxtrials/4-1
            start=i-const.Maxtrials/4+1;
            rowN= start+j;
            abb_item= design(rowN,1);
            abb_cond= design(rowN,2);
            
            options= [ '1)  ' char(abb.O1(abb_item)) '/n' '2)  ' char(abb.O2(abb_item)) ...
               '/n' '3)  ' char(abb.O3(abb_item)) '/n' '4)  ' char(abb.O4(abb_item))];
            options= strjoin(strsplit(options, '"'));    
     
            question= char(abb.ABBREV(abb_item));
            question= ['What does ' question ' mean?'];
     
             if ismember(abb_cond, [1,2])
                 corr_ans = abb.HF(abb_item);
             else
                 corr_ans = abb.LF(abb_item);
             end
            % present question:
            answer= QuestionMC(question, strsplit(options, '/n'), ...
           corr_ans, abb_item, abb_cond, 2);
        end
        
        % make break unless it's last item
        if i< const.ntrials
            MakeBreak(const.breakTime);
        end
  end
end


% end of Experiment text:
text= 'The experiment is finished! Thank you for participating!';
Screen(Monitor.window, 'TextSize', Visual.InstrTextSize);
Screen('DrawText', Monitor.window, text, Visual.resX/2- (Visual.Pix_per_Letter+3)*length(text)/2, Visual.resY/2, [139, 0, 0]);
Screen('Flip', Monitor.window);
