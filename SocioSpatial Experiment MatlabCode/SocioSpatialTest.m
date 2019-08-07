global const; 

%% settings:
clear all;
clear mex;
clear functions;

cd('H:\Profile\Desktop\worb\SocioSpacial\SocioSpatial Experiment MatlabCode');
addpath([cd '\functions'], [cd '\corpus'], [cd '\design'], [cd '\img']);

settings; % load settings
ExpSetup; % do window and tracker setup

%% Load stimuli and design:
load('AllStim.xlsx');

%importDesign; % old fun for loading txt files
design= genDesign(); % generate the design matrix for this subject
const.ntrials= length(design);
%% Run Experiment:
runTrials;
%% Save file & Exit:
status= Eyelink('ReceiveFile');
Eyelink('Shutdown');

Screen('CloseAll');