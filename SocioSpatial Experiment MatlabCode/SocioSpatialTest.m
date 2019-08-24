Screen('Preference', 'SkipSyncTests', 0); 
%change to 1 if it's getting fucky
global const; 

% settings:
clear all;
clear mex;
clear functions;

%Directory set

%%%%%%%%%%%%%%
%Windows P111%
%%%%%%%%%%%%%%
%cd('C:\Users\Eyetracker\Desktop\Calvin Laursen\worb\SocioSpacial\SocioSpatial Experiment MatlabCode');

%%%%%%%%%%%%%%%%%%%
%Directory in P104%
%%%%%%%%%%%%%%%%%%%
%cd('H:\Profile\Desktop\worb\SocioSpacial\SocioSpatial Experiment MatlabCode');
%addpath([cd '\functions'], [cd '\corpus'], [cd '\corpus\Sorted Texts'], [cd '\design']);

%%%%%%%%%%%%
%Linux P111%
%%%%%%%%%%%%
cd('/home/experimenter/Desktop/SocioSpatial Experiment MatlabCode');
%Directory in P104
%cd('H:\Profile\Desktop\worb\SocioSpacial\SocioSpatial Experiment MatlabCode');
%addpath([cd '\functions'], [cd '\corpus'], [cd '\corpus\Sorted Texts'], [cd '\design']);
addpath(genpath(cd));

% functs
settings; % load settings
ExpSetup; % do window and tracker setup

% Load stimuli and design:
load('sent.mat');
load('Quest.mat'); % questions

% generate the design matrix for this subject
design = genDesign(); 
const.ntrials = length(design);

% Run Experiment:
runTrials;

% Save file & Exit:
status= Eyelink('ReceiveFile');
Eyelink('Shutdown');

Screen('CloseAll');