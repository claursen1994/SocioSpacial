function [answer]=QuestionMC(question, opts, corr_ans, item, cond, D)

global Visual const Monitor;

% prepare string to print coords to edf file:
comp_qn_and_all_answers= [question '\n' strjoin(opts, '\n')];

% clock on
trialStart= GetSecs;

% clear the screen
Screen('FillRect', Monitor.window, Visual.BGC);
Screen('Flip', Monitor.window);

% Defines the location of the click boxes
% rect arguments are [top, left, right, bottom]
dim1= [Visual.sentPos(1)-10, Visual.sentPos(2)+Visual.LetterHeight*3-Visual.LetterHeight/2, Visual.sentPos(1)+40, Visual.sentPos(2)+Visual.LetterHeight*4+Visual.LetterHeight/2];
dim2= [Visual.sentPos(1)-10, Visual.sentPos(2)+Visual.LetterHeight*5+Visual.LetterHeight/2, Visual.sentPos(1)+40, Visual.sentPos(2)+Visual.LetterHeight*7+Visual.LetterHeight/2];
dim3= [Visual.sentPos(1)-10, Visual.sentPos(2)+Visual.LetterHeight*8+Visual.LetterHeight/2, Visual.sentPos(1)+40, Visual.sentPos(2)+Visual.LetterHeight*10+Visual.LetterHeight/2];

% put the comprehension question into the backbuffer: two possible ways:
%   draws the comprehension question only
Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); 
%   draws the comprehension question and the (text of the) answers
%DrawFormattedText(Monitor.window, comp_qn_and_all_answers, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC, [], [], [], Visual.TextSpacing*1.95);

% Put the click boxes into the back buffer
Screen('FillRect', Monitor.window , [210 210 210], dim1);
Screen('FillRect', Monitor.window , [210 210 210], dim2);
Screen('FillRect', Monitor.window , [210 210 210], dim3);

% put the answer numbers, and the text of the answers, into the backbuffer: 
Screen('DrawText', Monitor.window, char(opts(1)), dim1(1)+15, dim1(2)+15, Visual.FGC);
Screen('DrawText', Monitor.window, char(opts(2)), dim2(1)+15, dim2(2)+15, Visual.FGC); 
Screen('DrawText', Monitor.window, char(opts(3)), dim3(1)+15, dim3(2)+15, Visual.FGC); 

% flip the backbuffer to the visible screen
Screen('Flip', Monitor.window);

% Comprehension questions and answers have been flipped to the visible screen at this point

% show the cursor
ShowCursor(0); 

imageArray= Screen('GetImage', Monitor.window, [0 0 Visual.resX Visual.resY]);
imwrite(imageArray, 'disp.bmp');

Eyelink('Command', 'set_idle_mode');
Eyelink('Command', 'clear_screen 0');
status= Eyelink('ImageTransfer', 'disp.bmp', 0, 0, 0, 0,0, 0, 16);
 
% Initial question stamps:
Eyelink('Message', ['TRIALID F' num2str(cond) 'I' num2str(item) 'D' num2str(D)]);
stim2edfML(comp_qn_and_all_answers); % print question string to edf file:
 
Eyelink('Message', ['QUESTION_ANSWER ' num2str(corr_ans)]);
Eyelink('Message', 'DELAY 500 MS');
Eyelink('StartRecording');
WaitSecs(0.05);
Eyelink('command', ['record_status_message ' ['Question ' 'F' num2str(cond) 'I' num2str(item) 'D' num2str(D)]]);
 
WaitSecs(0.5);
Eyelink('Message', 'DISPLAY ON');
Eyelink('Message', 'SYNCTIME');

answer=-1; 
escapeKey= KbName('ESCAPE');
confirmKey= KbName('Y');

while answer<0
    % timeout escape
    trialTime= GetSecs- trialStart;
    if trialTime> const.TrialTimeout
        answer=0;
    end
    % get current mouse position
    [x,y,buttons] = GetMouse(Monitor.window);
    % check for the escape key
    [keyIsDown, seconds, keyCode]= KbCheck;
    keyCode= find(keyCode,1);
    if keyCode== escapeKey
        % status= Eyelink('ReceiveFile');
        % Eyelink('Shutdown');
        Screen('CloseAll');
        error('Experiment terminated by user');
    end
    % check for a mouse click
    if buttons(1)==1
        % check whether the mouse clicked click box 1
        if IsInRect(x,y, dim1) 
            % redraw the comprehension question, and change the colour of the clicked-in box
            Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % draws the comprehension question only
            % Put the click boxes into the back buffer
            Screen('FillRect', Monitor.window , [189 255 183], dim1);
            Screen('FillRect', Monitor.window , [210 210 210], dim2);
            Screen('FillRect', Monitor.window , [210 210 210], dim3);
            % put the answer numbers, and the text of the answers, into the backbuffer: 
            Screen('DrawText', Monitor.window, char(opts(1)), dim1(1)+15, dim1(2)+15, Visual.FGC);
            Screen('DrawText', Monitor.window, char(opts(2)), dim2(1)+15, dim2(2)+15, Visual.FGC); 
            Screen('DrawText', Monitor.window, char(opts(3)), dim3(1)+15, dim3(2)+15, Visual.FGC); 
            % flip, wait, return answer
            Screen('Flip', Monitor.window);
            WaitSecs(0.5);
            answer = 1;
        end
        % check whether the mouse clicked click box 2
        if IsInRect(x,y, dim2) 
            % redraw the comprehension question, and change the colour of the clicked-in box
            Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % draws the comprehension question only
            % Put the click boxes into the back buffer
            Screen('FillRect', Monitor.window , [210 210 210], dim1);
            Screen('FillRect', Monitor.window , [189 255 183], dim2);
            Screen('FillRect', Monitor.window , [210 210 210], dim3);
            % put the answer numbers, and the text of the answers, into the backbuffer: 
            Screen('DrawText', Monitor.window, char(opts(1)), dim1(1)+15, dim1(2)+15, Visual.FGC);
            Screen('DrawText', Monitor.window, char(opts(2)), dim2(1)+15, dim2(2)+15, Visual.FGC); 
            Screen('DrawText', Monitor.window, char(opts(3)), dim3(1)+15, dim3(2)+15, Visual.FGC); 
            % flip, wait, return answer
            Screen('Flip', Monitor.window);
            WaitSecs(0.5);
            answer= 2;
        end
        % check whether the mouse clicked click box 3
        if IsInRect(x,y, dim3) 
            % redraw the comprehension question, and change the colour of the clicked-in box
            Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % draws the comprehension question only
            % Put the click boxes into the back buffer
            Screen('FillRect', Monitor.window , [210 210 210], dim1);
            Screen('FillRect', Monitor.window , [210 210 210], dim2);
            Screen('FillRect', Monitor.window , [189 255 183], dim3);
            % put the answer numbers, and the text of the answers, into the backbuffer: 
            Screen('DrawText', Monitor.window, char(opts(1)), dim1(1)+15, dim1(2)+15, Visual.FGC);
            Screen('DrawText', Monitor.window, char(opts(2)), dim2(1)+15, dim2(2)+15, Visual.FGC); 
            Screen('DrawText', Monitor.window, char(opts(3)), dim3(1)+15, dim3(2)+15, Visual.FGC); 
            % flip, wait, return answer
            Screen('Flip', Monitor.window);
            WaitSecs(0.5);
            answer= 3;
        end
    end
end

% tell the edf about the accuracy of the answer
if answer==corr_ans
    Eyelink('command', ['record_status_message ' 'CORRECT!']);
else
    Eyelink('command', ['record_status_message ' 'INCORRECT!']);
end
WaitSecs(0.5);
Eyelink('StopRecording');

% Print end of question stamps to edf:
Eyelink('Message', ['ENDBUTTON ' num2str(answer)]);
Eyelink('Message', 'DISPLAY OFF');
Eyelink('Message', ['TRIAL_RESULT ' num2str(answer)]);
Eyelink('Message', 'TRIAL OK');

Screen('FillRect', Monitor.window, Visual.BGC);
Screen('Flip', Monitor.window);
Eyelink('command', 'clear_screen 0'); % clear tracker screen
HideCursor;
