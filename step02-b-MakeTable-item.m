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

%%Add interaction term
ParticipantxTaskxImage = AllData.participant.*AllData.current_task.*AllData.image;
% categories(ParticipantxTaskxImage)
AllData.ParticipantxTaskxImage = categorical(ParticipantxTaskxImage); 

all_accuracy.participant = categorical(all_accuracy.participant);
all_accuracy.current_task = categorical(cellstr(all_accuracy.current_task));
all_accuracy.image = categorical(cellstr(all_accuracy.image));


ParticipantxTaskxImage_accuracy = all_accuracy.participant.*all_accuracy.current_task.*all_accuracy.image;
% categories(ParticipantxTaskxImage)
all_accuracy.ParticipantxTaskxImage = categorical(ParticipantxTaskxImage_accuracy); 




%% Compute mean for item*task
statarray = grpstats(AllData,{'ParticipantxTaskxImage','congruent'},...
    {'mean'},'DataVars',{'correctRTs','RT_log'});

listuniqe = unique(all_accuracy.ParticipantxTaskxImage);

for i = 1:length(listuniqe)
      databyItem.ParticipantxTaskxImage =  listuniqe(i);
    if find(statarray.ParticipantxTaskxImage == listuniqe(i)); %%if this person was accurate..
        
        databyItem.correctRTs(i,:) = statarray.mean_correctRTs(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
        databyItem.RT_log(i,:) = statarray.mean_RT_log(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
       databyItem.congruent(i,:) = statarray.congruent(find(statarray.ParticipantxTaskxImage == listuniqe(i)));
    else
        databyItem.correctRTs(i,:) = NaN;
        databyItem.RT_log(i,:) = NaN;
        databyItem.congruent(i,:) = NaN;
        
    end
end



databyItem.accuracy = all_accuracy.accuracy;
condList = unique(AllData.condition);
condmat =[];
for i = 1:length(condList)
   condmat = [condmat;cellstr(char(repmat(uint8(condList{i}) ,45,1)))]
end
    
%%add size and curvature conds


Size = [char(repmat(66,45*2,1));char(repmat(83,45*2,1))];
databyItem.SizeCond = repmat([Size;Size],24,1);

Curv = [char(repmat(66,45,1));char(repmat(67,45,1));char(repmat(66,45,1));char(repmat(67,45,1))];
databyItem.CurvCond = repmat([Curv;Curv],24,1);

databyItem.condition = repmat(condmat,24,1);


databyItem.participant = all_accuracy.participant;
databyItem.current_task = all_accuracy.current_task;

%%give NaNs to accuracy of all removed RTs
databyItem.accuracy(isnan(databyItem.correctRTs)) = NaN;

save('Analysis_cm/databyItem','databyItem')

databyItem_table = table(databyItem.participant,databyItem.correctRTs,databyItem.RT_log,databyItem.accuracy,databyItem.congruent,databyItem.SizeCond,databyItem.CurvCond,databyItem.condition,all_accuracy.image,all_accuracy.current_task,...
    'VariableNames',{'participant','correctRTs','RT_log','accuracy','congruent','SizeCond','CurvCond','condition','image','current_task'})
head(databyItem_table)
save('Analysis_cm/databyItem_table','databyItem_table')
writetable(databyItem_table,'Analysis_cm/databyItem_table.csv')



