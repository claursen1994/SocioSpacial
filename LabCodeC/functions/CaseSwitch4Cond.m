
item= 1;
cond= 1;

switch cond
    case 1
        P1= char(importdata(['SocAmbi' num2str(item) '.txt'])); 
        P2= char(importdata(['SpaAmbi' num2str(item) '.txt']));  
    case 2
        P1= ['corpus/Sorted Texts/SocNambi/' num2str(item) '.txt']; 
        P2= ['corpus/Sorted Texts/SpaNambi/' num2str(item) '.txt']; 
    case 3
        P1= ['corpus/Sorted Texts/SocAmbi/' num2str(item) '.txt']; 
        P2= ['corpus/Sorted Texts/SpaNambi/' num2str(item) '.txt']; 
    case 4
        P1= ['corpus/Sorted Texts/SocNambi/' num2str(item) '.txt']; 
        P2= ['corpus/Sorted Texts/SpaNambi/' num2str(item) '.txt'];
    case 5
        P1= ['corpus/Sorted Texts/SpaAmbi/' num2str(item) '.txt']; 
        P2= ['corpus/Sorted Texts/SocAmbi/' num2str(item) '.txt'];
        
    case 6
        P1= ['corpus/Sorted Texts/SpaNambi/' num2str(item) '.txt']; 
        P2= ['corpus/Sorted Texts/SocNambi/' num2str(item) '.txt'];
    case 7
        P1= ['corpus/Sorted Texts/SpaAmbi/' num2str(item) '.txt']; 
        P2= ['corpus/Sorted Texts/SocNambi/' num2str(item) '.txt'];
    case 8
        P1= ['corpus/Sorted Texts/SpaNambi/' num2str(item) '.txt']; 
        P2= ['corpus/Sorted Texts/SocAmbi/' num2str(item) '.txt'];
end


% 2 question

% Q1= 
% Q2=

% Opts1= 
% Opts2=

% Corr_ans1= 
% Corr_ans2=