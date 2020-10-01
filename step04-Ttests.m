clear all
clc
load('Analysis_cm/AllData_conds')

Data_byCond.current_task = categorical(cellstr(Data_byCond.current_task));
Data_byCond.CurvCond = categorical(cellstr(Data_byCond.CurvCond));
Data_byCond.SizeCond = categorical(cellstr(Data_byCond.SizeCond));

inter_temp = Data_byCond.SizeCond.*Data_byCond.CurvCond;
categories(inter_temp);
Data_byCond.inter_temp = removecats(inter_temp); %drop unused categories


tasklist = unique(Data_byCond.current_task);

interList = unique(Data_byCond.inter_temp);


fileID = fopen(fullfile('Analysis_cm/',sprintf('ttests_RT_%s.txt',datestr(datetime('now'),'mmdd_yyyy_HHMM'))),'w');

for iTask = 1:length(tasklist)
    ThisTaskData = Data_byCond(Data_byCond.current_task==tasklist(iTask),:);
    
    for iinter1 = 1:(length(interList)-1)
        for iinter2 = iinter1+1:length(interList)
            %signficiant for curv task
            [H,P,~,STATS] = ttest(ThisTaskData.RT(ThisTaskData.inter_temp ==interList(iinter1),:),ThisTaskData.RT(ThisTaskData.inter_temp ==interList(iinter2),:));
            
            fprintf(fileID,'t-test for task %s between conditions %s and %s\n',tasklist(iTask),interList(iinter1),interList(iinter2));
            fprintf(fileID,'H: %d p-value: %.3f tstat: %.3f df: %d\n\n\n',H,P,STATS.tstat,STATS.df);
            fprintf(fileID,'***************************************\n');
        end
    end
end
fclose(fileID);

%% with log RT

fileID = fopen(fullfile('Analysis_cm/',sprintf('ttests_RTlog_%s.txt',datestr(datetime('now'),'mmdd_yyyy_HHMM'))),'w');

for iTask = 1:length(tasklist)
    ThisTaskData = Data_byCond(Data_byCond.current_task==tasklist(iTask),:);
    
    for iinter1 = 1:(length(interList)-1)
        for iinter2 = iinter1+1:length(interList)
            %signficiant for curv task
            [H,P,~,STATS] = ttest(ThisTaskData.RT_log(ThisTaskData.inter_temp ==interList(iinter1),:),ThisTaskData.RT_log(ThisTaskData.inter_temp ==interList(iinter2),:));
            
            fprintf(fileID,'t-test for task %s between conditions %s and %s\n',tasklist(iTask),interList(iinter1),interList(iinter2));
            fprintf(fileID,'H: %d p-value: %.3f tstat: %.3f df: %d\n\n\n',H,P,STATS.tstat,STATS.df);
            fprintf(fileID,'***************************************\n');
        end
    end
end
fclose(fileID);


%% with accuracy

fileID = fopen(fullfile('Analysis_cm/',sprintf('ttests_accuracy_%s.txt',datestr(datetime('now'),'mmdd_yyyy_HHMM'))),'w');

for iTask = 1:length(tasklist)
    ThisTaskData = Data_byCond(Data_byCond.current_task==tasklist(iTask),:);
    
    for iinter1 = 1:(length(interList)-1)
        for iinter2 = iinter1+1:length(interList)
            %signficiant for curv task
            [H,P,~,STATS] = ttest(ThisTaskData.accuracy(ThisTaskData.inter_temp ==interList(iinter1),:),ThisTaskData.accuracy(ThisTaskData.inter_temp ==interList(iinter2),:));
            
            fprintf(fileID,'t-test for task %s between conditions %s and %s\n',tasklist(iTask),interList(iinter1),interList(iinter2));
            fprintf(fileID,'H: %d p-value: %.3f tstat: %.3f df: %d\n\n\n',H,P,STATS.tstat,STATS.df);
            fprintf(fileID,'***************************************\n');
        end
    end
end
fclose(fileID);

