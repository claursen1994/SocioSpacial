global const; 

%% settings:
clear all;
clear mex;
clear functions;
%directory in P111
%cd('C:\Users\Eyetracker\Desktop\Calvin Laursen\worb\SocioSpacial\SocioSpatial Experiment MatlabCode')
%Directory in P104
cd('H:\Profile\Desktop\worb\SocioSpacial\SocioSpatial Experiment MatlabCode');
%addpath([cd '\functions'], [cd '\corpus'], [cd '\corpus\Sorted Texts'], [cd '\design']);
addpath(genpath(cd));

settings; % load settings
ExpSetup; % do window and tracker setup

%% Load stimuli and design:
load('sent.mat');
load('QuestMaster.mat'); % questions
%importDesign; % old fun for loading txt files
design= genDesign(); % generate the design matrix for this subject
const.ntrials= length(design);
%% Run Experiment:
runTrials;
%% Save file & Exit:
status= Eyelink('ReceiveFile');
Eyelink('Shutdown');

Screen('CloseAll');