function design = genDesign()
% Generate a design matrix for the current subject
% Victoria Adedeji & Martin Vasilev, 2019

global const;
rng('shuffle');

design_same = load('design_same.dat');% load the data (from xLatinSquare):

if const.ID>990 % for testing
    cps = design_same(:,[1, 2]);
    warning('Using a test subject number!');
    
else % real subject
   cps = design_same(:,[1, const.ID+1]); % +1 because the first column is item number(%cps is conditions per subject) 
end

% sf = cps(find(mod(cps(:,end),2)==1),:); %b1 is silent
% bf = cps(find(mod(cps(:,end),2)==0),:); %b2 is aloud

%Randomise the items within the blocks:
cpsr= cps(randperm(length(cps)),:);

exp_items= cpsr;

%Practice items:
% condition always the same (e.g. 9)
c1 = (25:26)'; % column 1 is item number
c2 = [1,1]'; %repmat(9,6,1); % column 2 is condition (always 9)
pr = horzcat(c1,c2); % 2- column matrix 

%Randomise practice items as above:
prr = pr(randperm (length(pr)),:);

% % combine practice & experimental items:
%design = [prr; exp_items];
design = [prr; exp_items];


% save design matrix for our records:
savefile= ['design/sub_matrix/s_' num2str(const.ID) '.mat'];
save(savefile, 'design');

end
