close all
clear all
clc
load('Analysis_cm/databyItem_table')
head(databyItem_table)


%% Make variables categorical
databyItem_table.participant = categorical(databyItem_table.participant);
categories(databyItem_table.participant)

databyItem_table.CurvCond = categorical(cellstr(databyItem_table.CurvCond));
databyItem_table.SizeCond = categorical(cellstr(databyItem_table.SizeCond));
databyItem_table.current_task = categorical(cellstr(databyItem_table.current_task));

%%Add interaction term
TaskxImage = databyItem_table.current_task.*databyItem_table.image;
categories(TaskxImage)
databyItem_table.TaskxImage = categorical(TaskxImage); %drop unused categories


%%Add interaction term
TaskxImage = databyItem_table.current_task.*databyItem_table.image;
categories(TaskxImage)
databyItem_table.TaskxImage = categorical(TaskxImage); %drop unused categories

%%Add interaction term
PartxTaskxImage = databyItem_table.participant.*databyItem_table.current_task.*databyItem_table.image;
categories(PartxTaskxImage)
databyItem_table.PartxTaskxImage = categorical(PartxTaskxImage); %drop unused categories


statarray = grpstats(databyItem_table,'TaskxImage',...
                      {'mean','sem'},'DataVars',{'correctRTs','RT_log'})
                  
statarray_sub = grpstats(databyItem_table,'PartxTaskxImage',...
                      {'mean','sem'},'DataVars',{'correctRTs','RT_log','accuracy'})
 
  %%Add interaction term
SizexCurv = databyItem_table.SizeCond.*databyItem_table.CurvCond;
categories(SizexCurv)
databyItem_table.SizexCurv = categorical(SizexCurv); %drop unused categories

databyItem_table_Size = databyItem_table(databyItem_table.current_task == 'JudgeSize',:);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% compare models with and without interaction                 
Mixedlm = fitlme(databyItem_table_Size, 'correctRTs ~  SizeCond + CurvCond + (1+SizeCond+CurvCond|participant)  + (1|image)  ', 'DummyVarCoding','effects');
Mixedlm_inter = fitlme(databyItem_table_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)   + (1|image) ', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_inter,'CheckNesting',true)
% The small p-value of 0 indicates that model altlme is significantly better than the simpler model lme.
%interaction is better!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compare models with and without random slope                 
Mixedlm = fitlme(databyItem_table_Size, 'correctRTs ~  SizeCond * CurvCond + (1|participant) + (1|image) ', 'DummyVarCoding','effects');
Mixedlm_slope = fitlme(databyItem_table_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)  + (1|image)', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_slope,'CheckNesting',true)
% The small p-value of 0 indicates that model altlme is significantly better than the simpler model lme.
%random slope is better!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compare models with and without random image                 
Mixedlm = fitlme(databyItem_table_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)   ', 'DummyVarCoding','effects');
Mixedlm_image = fitlme(databyItem_table_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image) ', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_image)
% The small p-value indicates that model altlme is significantly better than the simpler model lme.
%model with image is better!


Mixedlm_winner_RT = fitlme(databyItem_table_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image)  ', 'DummyVarCoding','effect');
anova(Mixedlm_winner_RT)
Mixedlm_winner_dummy = fitlme(databyItem_table_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image)');




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




databyItem_table_Curv = databyItem_table(databyItem_table.current_task == 'JudgeCurviness',:);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% compare models with and without interaction                 
Mixedlm = fitlme(databyItem_table_Curv, 'correctRTs ~  SizeCond + CurvCond + (1+SizeCond+CurvCond|participant)  + (1|image)  ', 'DummyVarCoding','effects');
Mixedlm_inter = fitlme(databyItem_table_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)   + (1|image) ', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_inter,'CheckNesting',true)
% The small p-value of 0 indicates that model altlme is significantly better than the simpler model lme.
%interaction is better!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compare models with and without random slope                 
Mixedlm = fitlme(databyItem_table_Curv, 'correctRTs ~  SizeCond * CurvCond + (1|participant) + (1|image) ', 'DummyVarCoding','effects');
Mixedlm_slope = fitlme(databyItem_table_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)  + (1|image)', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_slope,'CheckNesting',true)
% The small p-value of 0 indicates that model altlme is significantly better than the simpler model lme.
%random slope is better!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compare models with and without random image                 
Mixedlm = fitlme(databyItem_table_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)   ', 'DummyVarCoding','effects');
Mixedlm_image = fitlme(databyItem_table_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image) ', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_image)
% The small p-value indicates that model altlme is significantly better than the simpler model lme.
%model with image is better!


Mixedlm_winner_RT = fitlme(databyItem_table_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image)  ', 'DummyVarCoding','effect');
anova(Mixedlm_winner_RT)
Mixedlm_winner_dummy = fitlme(databyIdatabyItem_table_Curvtem_table, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image)');
