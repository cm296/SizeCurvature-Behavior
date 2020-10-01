close all
clear all
clc
load('Analysis_cm/AllData')
head(AllData)

%combine with accuracy
load('Analysis_cm/all_accuracy')
head(all_accuracy)

CondList = unique(AllData.condition);

AllData.Participant = categorical(cellstr(AllData.Participant));
categories(AllData.Participant)

Data_byCond = [];
RT=[]
condition =[]
participant =[]
congruent=[];
row = 0;

for isub = 1 : size(unique(AllData.Participant),1)
    
    thissub = AllData(AllData.Participant==num2str(isub,'%02.f'),:);
    thissub_accuracy = all_accuracy(all_accuracy.participant==isub,:);
    for iCond = 1 : size(unique(AllData.condition),1)
        
        
        thissub_thiscond = thissub(strcmp(thissub.condition, CondList{iCond}),:);
        
        row = row +1;
        RT(row,:) = nanmean(thissub_thiscond.RT);
        RT_log(row,:) = nanmean(thissub_thiscond.RT_log);
        condition = [condition; CondList(iCond)];
        participant(row,:) = isub;
        current_task(row,:) = thissub_thiscond.current_task(1);
        CurvCond(row,:) = thissub_thiscond.CurvCond(1);
        SizeCond(row,:) = thissub_thiscond.SizeCond(1);
        SizeCurv(row,:) = thissub_thiscond.SizeCurv(1);
        congruent(row,:) = thissub_thiscond.congruent(1);
        accuracy(row,:) = mean(thissub_accuracy.accuracy(strcmp(thissub_accuracy.condition,CondList{iCond}),:));
        %probably unnecessary becuase listed in same order but better be sure
    end
    
%     thissub.SizeCond = categorical(cellstr(thissub.SizeCond))
%     thissub.CurvCond = categorical(cellstr(thissub.CurvCond))
%     thissub.SizeCurv = thissub.SizeCond.*thissub.CurvCond;
%     key1 = thissub.correct_resp(intersect(find(strcmp(thissub.current_task, 'JudgeSize')),find(thissub.SizeCurv== 'Bi Bo')));
%     key2 = thissub.correct_resp(intersect(find(strcmp(thissub.current_task, 'JudgeCurviness')),find(thissub.SizeCurv== 'Bi Bo')));
%     if key1(1) == key2(1)
%         congruency = 1;
%     else
%         congruency = 0;
%     end
%     congruent = [congruent;repmat(congruency,size(unique(AllData.condition),1),1)];
end

Data_byCond.RT = RT;
Data_byCond.RT_log = RT_log;
Data_byCond.condition = condition;
Data_byCond.participant = participant;
Data_byCond.current_task = current_task;
Data_byCond.CurvCond = CurvCond;
Data_byCond.SizeCond = SizeCond;
Data_byCond.SizeCurv = SizeCurv;
Data_byCond.congruent = congruent;
Data_byCond.accuracy = accuracy;

Data_byCond = struct2table(Data_byCond)
head(Data_byCond)



save('Analysis_cm/AllData_conds','Data_byCond')
writetable(Data_byCond,'Analysis_cm/AllData_conds.csv')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Make one distinguishing between block# 
close all
clear all
clc
load('Analysis_cm/AllData')

CondList = unique(AllData.condition);

%combine with accuracy
load('Analysis_cm/all_accuracy')
head(all_accuracy)

AllData.Participant = categorical(cellstr(AllData.Participant));
head(AllData)
categories(AllData.Participant)

Data_byCond_block = [];
RT=[];
condition =[];
participant =[];
row = 0;

for isub = 1 : size(unique(AllData.Participant),1)
    
    thissub = AllData(AllData.Participant==num2str(isub,'%02.f'),:);
        thissub_accuracy = all_accuracy(all_accuracy.participant==isub,:);

    for iCond = 1 : size(unique(AllData.condition),1)
        
        
        thissub_thiscond = thissub(strcmp(thissub.condition, CondList{iCond}),:);
        thissub_thiscond_block = unique(thissub_thiscond.block);
        for iblock = 1 : size(unique(thissub_thiscond.block),1)

        thissub_thiscond_thisblock = thissub_thiscond(thissub_thiscond.block==thissub_thiscond_block(iblock),:);
       
        
        row = row +1;
        RT(row,:) = nanmean(thissub_thiscond.RT);
        RT_log(row,:) = nanmean(thissub_thiscond.RT_log);

        condition = [condition; CondList(iCond)];
        participant(row,:) = isub;
        block(row,:) = thissub_thiscond_block(iblock);
        current_task(row,:) = thissub_thiscond.current_task(1);
        CurvCond(row,:) = thissub_thiscond.CurvCond(1);
        SizeCond(row,:) = thissub_thiscond.SizeCond(1);
        accuracy(row,:) = nanmean(thissub_accuracy(strcmp(thissub_accuracy.condition,CondList{iCond}),:).accuracy);

        end
    end
end

Data_byCond_block.RT = RT;
Data_byCond_block.RT_log = RT_log;
Data_byCond_block.current_task = current_task;
Data_byCond_block.CurvCond = CurvCond;
Data_byCond_block.SizeCond = SizeCond
Data_byCond_block.accuracy = accuracy;

Data_byCond_block.condition = condition;
Data_byCond_block.participant = participant;
Data_byCond_block.block = block;

Data_byCond_block = struct2table(Data_byCond_block)
save('Analysis_cm/AllData_conds_block','Data_byCond_block')
writetable(Data_byCond_block,'Analysis_cm/AllData_conds_block.csv')