%%% this is the main analysis file for the experiment ===
% it plots the results in bar/line plot
% and will write group data to a excel for better view

close all;
clear all;
current=pwd();

addpath('z-helper/')
%read raw data file
nParticipants=24;
strtSubNum=1;
nCond=8;
trialPerCond=90;
organizedfile='OrganizedData_Exp_Rerun.xlsx';
headerSubj=[];
excludedTrial=0;

%% toggle what to do
barplot=1;  % plot results with bar graphs
subploting=0; % whether subplot four graphs in one figure window

lineplot=1; % plot results with line graphs

AllData = [];
all_accuracy=[];

for iParticipant = 1 : nParticipants
    
    thissubtemp=[];
    thissub = [];
    fprintf('loading participant %d...\n', iParticipant);
    %     cd([current '/Data']);
    filename=sprintf('Expt__NewSet_%0.3d',strtSubNum+iParticipant-1);
    datafile=['Data/' filename '.xlsx'];    %new filename (xlsx)
    nRow=nCond*trialPerCond;
    headerSubj=[headerSubj;iParticipant];
    
    %% preprocess data
    thissubtemp = readtable(datafile);
    thissub = thissubtemp(:,1:17);
    
    Participant = num2str(repmat(iParticipant,size(thissub,1),1),'%02.f');
    
    participant = repmat(iParticipant,size(thissub,1),1);
    
    thissub = addvars(thissub,Participant,'Before','timestamp');
    thissub = addvars(thissub,participant);
    condlist = unique(thissub.image);
    
    CurvCond = [];
    
    for i = 1:size(thissub.image,1)
        if isempty(strfind( thissub.image{i},'Curvy') )
            CurvCond(i,:) ='Bo';
        else
            CurvCond(i,:) ='Cu';
        end
    end
    CurvCond = char(CurvCond);
    
    thissub = addvars(thissub,CurvCond,'After','correctRTs');
    
    SizeCond = [];
    for i = 1:size(thissub.image,1)
        if isempty(strfind( thissub.image{i},'Big') )
            SizeCond(i,:) ='Sm';
        else
            SizeCond(i,:) ='Bi';
        end
    end
    SizeCond = char(SizeCond);
    
    thissub = addvars(thissub,SizeCond,'After','CurvCond');
    
    %remove incorrect trials ans save them in a struct
    if ~isempty(find(thissub.correct_resp~= thissub.actual_resp))
        uniqueTask = unique(thissub.current_task);
        percent_accurate_task =[];
        what_incorrect_percent(iParticipant) = size(find(thissub.correct_resp ~= thissub.actual_resp),1)/size(thissub.correct_resp,1);
        for iTask = 1:2
            %what is incorrect and is of this task
            what_incorrect = intersect(find(thissub.correct_resp ~= thissub.actual_resp), find(strcmp(thissub.current_task,uniqueTask{iTask})));
            thissub_incorrect = thissub(what_incorrect,:);
            cond_incorrect = unique(thissub_incorrect.image);
            percent_accurate=[];
            
            for iCond = 1:length(condlist)
                if isempty(find(strcmp(condlist{iCond},cond_incorrect)))
                    %percent accuracy
                    percent_accurate(iCond) = 1 ;
                else
                    percent_accurate(iCond) = 1 - size(thissub_incorrect(strcmp(thissub_incorrect.image,condlist{iCond}),:),1)/size(thissub(strcmp(thissub.image,condlist{iCond}),:),1);
                end
                
            end
            percent_accurate_task = [percent_accurate_task, percent_accurate];
        end
        
        Tasks = [cellstr(char(repmat(uint8(uniqueTask{1}),length(condlist),1)));cellstr(char(repmat(uint8(uniqueTask{2}),length(condlist),1)))];
        thissub_accuracy_task = table(repmat(iParticipant,length(condlist)*2,1),[condlist;condlist],Tasks,percent_accurate_task','VariableNames',{'participant','image','current_task','accuracy'});
        %%here we actually remove the incorrect trials!
        thissub = thissub(thissub.correct_resp== thissub.actual_resp,:);
    else
        what_incorrect_percent(iParticipant) = 0;
    end
    all_accuracy = [all_accuracy;thissub_accuracy_task];
    %%remove RTs>3 SD from mean
    %         for iCond = 1 : size(unique(thissub.condition),1)
    
    %     statarray = grpstats(thissub,'condition',...
    %         {'mean','std'},'DataVars',{'correctRTs'});
    
    
    %     upbound=statarray.mean_correctRTs+3*statarray.std_correctRTs;
    
    condList = unique(thissub.condition);
    
    tokeep=[];
    thissub_trimmed=[];
    for iCond = 1 : size(unique(thissub.condition),1)
        thisCondsub_temp=[];
        thisCondsub=[];
        
        thisCondsub_temp = thissub(strcmp(thissub.condition,condList(iCond)),:);
        %find the correct RTs within 3SD and retain those
        [thisCondsub,count] = RemoveRTs_sigma(thisCondsub_temp, 3,0);
        thissub_trimmed = [thissub_trimmed;thisCondsub];
        
        sizeOriginalTrials(iCond) = size(thisCondsub_temp,1);
        sizeaftertrimming(iCond) = size(thisCondsub,1);   
    end
   alloriginal = size(thissub.correctRTs,1) ;
   allafter = size(find(isnan(thissub_trimmed.correctRTs)),1) - size(find(isnan(thissub.correctRTs)),1);
   percentsigmatrimmed(iParticipant) = allafter/alloriginal;
    
    
    RT_log = log(thissub_trimmed.RT);
    thissub_trimmed = addvars(thissub_trimmed,RT_log);
    
    
    thissub_trimmed.SizeCond = categorical(cellstr(thissub_trimmed.SizeCond));
    thissub_trimmed.CurvCond = categorical(cellstr(thissub_trimmed.CurvCond));
    thissub_trimmed.SizeCurv = thissub_trimmed.SizeCond.*thissub_trimmed.CurvCond;
    
    %%add congruent variable
    key1 = thissub_trimmed.correct_resp(intersect(find(strcmp(thissub_trimmed.current_task, 'JudgeSize')),find(thissub_trimmed.SizeCurv== 'Bi Bo')));
    key2 = thissub_trimmed.correct_resp(intersect(find(strcmp(thissub_trimmed.current_task, 'JudgeCurviness')),find(thissub_trimmed.SizeCurv== 'Bi Bo')));
    if key1(1) == key2(1)
        congruency = 1;
    else
        congruency = 0;
    end
    
    thissub_trimmed.congruent = repmat(congruency,size(thissub_trimmed,1),1);
    AllData = [AllData; thissub_trimmed];
end
% AllData = addvars(AllData,RT_log,'After','SizeCond');
head(AllData)

% boundACC=mean(overallACC)-2*std(overallACC);
% boundRT=mean(statarray.mean_correctRTs)+2*mean(statarray.std_correctRTs);
save('Analysis_cm/AllData.mat', 'AllData')

%%mean percent of trials removed?
mean(what_incorrect_percent)
mean(percentsigmatrimmed)

condList = unique(AllData.condition)
condmat =[];
for i = 1:length(condList)
    condmat = [condmat;cellstr(char(repmat(uint8(condList{i}) ,45,1)))]
end

%%add size and curvature conds
Size = [char(repmat(66,45*2,1));char(repmat(83,45*2,1))];
SizeCond = repmat([Size;Size],24,1);
Curv = [char(repmat(66,45,1));char(repmat(67,45,1));char(repmat(66,45,1));char(repmat(67,45,1))];
CurvCond = repmat([Curv;Curv],24,1);
condition = repmat(condmat,24,1);

all_accuracy = addvars(all_accuracy,SizeCond);
all_accuracy = addvars(all_accuracy,CurvCond);
all_accuracy = addvars(all_accuracy,condition);


save('Analysis_cm/all_accuracy.mat', 'all_accuracy')