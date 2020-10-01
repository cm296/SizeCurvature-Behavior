close all
clear all
clc
load('Analysis_cm/AllData')
head(AllData)

%combine with accuracy
load('Analysis_cm/all_accuracy')
head(all_accuracy)

itemList = unique(AllData.image);

AllData.Participant = categorical(cellstr(AllData.Participant));
categories(AllData.Participant)

Data_byCond = [];
RT=[]
condition =[]
participant =[]
row = 0;

AllData.participant = categorical(AllData.participant);
AllData.current_task = categorical(cellstr(AllData.current_task));
AllData.image = categorical(cellstr(AllData.image));

inter_temp = AllData.CurvCond.*AllData.SizeCond;
categories(inter_temp)
AllData.inter_temp = removecats(inter_temp); %drop unused categories
categories(AllData.inter_temp)


%%create table only for first block
AllData_block1 = AllData(AllData.block==1,:);


%%Add interaction term
ParticipantxTaskxImage = AllData_block1.participant.*AllData_block1.current_task.*AllData_block1.image;

% categories(ParticipantxTaskxImage)
AllData_block1.ParticipantxTaskxImage = categorical(ParticipantxTaskxImage);

%% Compute mean for item*task
statarray = grpstats(AllData_block1,{'ParticipantxTaskxImage','congruent','SizeCond', 'CurvCond','participant','image','current_task'},...
    {'mean'},'DataVars',{'correctRTs','RT_log'});

listuniqe = unique(AllData_block1.ParticipantxTaskxImage);
databyItem.ParticipantxTaskxImage = [];
for i = 1:length(listuniqe)
    databyItem.ParticipantxTaskxImage = [databyItem.ParticipantxTaskxImage, listuniqe(i)];
    if find(statarray.ParticipantxTaskxImage == listuniqe(i)); %%if this person was accurate..
        
        databyItem.correctRTs(i,:) = statarray.mean_correctRTs(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
        databyItem.RT_log(i,:) = statarray.mean_RT_log(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
        databyItem.congruent(i,:) = statarray.congruent(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
        databyItem.SizeCond(i,:) =  statarray.SizeCond(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
        databyItem.CurvCond(i,:) =  statarray.CurvCond(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
        databyItem.participant(i,:) =  statarray.participant(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
        databyItem.image(i,:) =  statarray.image(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
        databyItem.current_task(i,:) =  statarray.current_task(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
        
    else
        databyItem.correctRTs(i,:) = NaN;
        databyItem.RT_log(i,:) = NaN;
        databyItem.congruent(i,:) = NaN;
        databyItem.SizeCond(i,:) =  NaN;
        databyItem.CurvCond(i,:) =  NaN;
        databyItem.participant(i,:) =  NaN;
        databyItem.image(i,:) =  NaN;
        databyItem.current_task(i,:) =  NaN;
        
    end
end


save('Analysis_cm/databyItem_block1','databyItem')

databyItem_table_block1 = table(databyItem.participant,databyItem.correctRTs,databyItem.RT_log,databyItem.congruent,databyItem.SizeCond,databyItem.CurvCond,databyItem.image,databyItem.current_task,...
    'VariableNames',{'participant','correctRTs','RT_log','congruent','SizeCond','CurvCond','image','current_task'})
head(databyItem_table_block1)
save('Analysis_cm/databyItem_table_block1','databyItem_table_block1')
writetable(databyItem_table_block1,'Analysis_cm/databyItem_table_block1.csv')





