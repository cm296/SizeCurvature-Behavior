close all
clear all
clc
load('Analysis_cm/databyItem_table_block1')
head(databyItem_table_block1)


%% Make variables categorical
databyItem_table_block1.participant = categorical(databyItem_table_block1.participant);
categories(databyItem_table_block1.participant)

databyItem_table_block1.CurvCond = categorical(cellstr(databyItem_table_block1.CurvCond));
databyItem_table_block1.SizeCond = categorical(cellstr(databyItem_table_block1.SizeCond));
databyItem_table_block1.current_task = categorical(cellstr(databyItem_table_block1.current_task));

%%Add interaction term
TaskxImage = databyItem_table_block1.current_task.*databyItem_table_block1.image;
categories(TaskxImage)
databyItem_table_block1.TaskxImage = categorical(TaskxImage); %drop unused categories


%%Add interaction term
TaskxImage = databyItem_table_block1.current_task.*databyItem_table_block1.image;
categories(TaskxImage)
databyItem_table_block1.TaskxImage = categorical(TaskxImage); %drop unused categories

%%Add interaction term
PartxTaskxImage = databyItem_table_block1.participant.*databyItem_table_block1.current_task.*databyItem_table_block1.image;
categories(PartxTaskxImage)
databyItem_table_block1.PartxTaskxImage = categorical(PartxTaskxImage); %drop unused categories


statarray = grpstats(databyItem_table_block1,'TaskxImage',...
                      {'mean','sem'},'DataVars',{'correctRTs','RT_log'})
                  
% statarray_sub = grpstats(databyItem_table_block1,'PartxTaskxImage',...
%                       {'mean','sem'},'DataVars',{'correctRTs','RT_log','accuracy'})
 
  %%Add interaction term
SizexCurv = databyItem_table_block1.SizeCond.*databyItem_table_block1.CurvCond;
categories(SizexCurv)
databyItem_table_block1.SizexCurv = categorical(SizexCurv); %drop unused categories



databyItem_table_block1_Size = databyItem_table_block1(databyItem_table_block1.current_task == 'JudgeSize',:);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% compare models with and without interaction                 
Mixedlm = fitlme(databyItem_table_block1_Size, 'correctRTs ~  SizeCond + CurvCond + (1+SizeCond+CurvCond|participant)  + (1|image)  + (1|current_task)', 'DummyVarCoding','effects');
Mixedlm_inter = fitlme(databyItem_table_block1_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)   + (1|image) + (1|current_task)', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_inter,'CheckNesting',true)
% The small p-value of 0 indicates that model altlme is significantly better than the simpler model lme.
%interaction is better!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compare models with and without random slope                 
Mixedlm = fitlme(databyItem_table_block1_Size, 'correctRTs ~  SizeCond * CurvCond + (1|participant) + (1|image) + (1|current_task)', 'DummyVarCoding','effects');
Mixedlm_slope = fitlme(databyItem_table_block1_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)  + (1|image)  + (1|current_task)', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_slope,'CheckNesting',true)
% The small p-value of 0 indicates that model altlme is significantly better than the simpler model lme.
%random slope is better!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compare models with and without random image                 
Mixedlm = fitlme(databyItem_table_block1_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)  ', 'DummyVarCoding','effects');
Mixedlm_image = fitlme(databyItem_table_block1_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image) ', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_image)
% The small p-value indicates that model altlme is significantly better than the simpler model lme.
%model with image is better!


Mixedlm_winner_RT = fitlme(databyItem_table_block1_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image)  ', 'DummyVarCoding','effect');
anova(Mixedlm_winner_RT)
Mixedlm_winner_RT.Rsquared.Adjusted
Mixedlm_winner_dummy = fitlme(databyItem_table_block1_Size, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image) ');
Mixedlm_winner_dummy.Rsquared.Adjusted


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% NOW WITH only block 1 %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

databyItem_table_block1_Curv = databyItem_table_block1(databyItem_table_block1.current_task == 'JudgeCurviness',:);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% compare models with and without interaction                 
Mixedlm = fitlme(databyItem_table_block1_Curv, 'correctRTs ~  SizeCond + CurvCond + (1+SizeCond+CurvCond|participant)  + (1|image)  + (1|current_task)', 'DummyVarCoding','effects');
Mixedlm_inter = fitlme(databyItem_table_block1_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)   + (1|image) + (1|current_task)', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_inter,'CheckNesting',true)
% The small p-value of 0 indicates that model altlme is significantly better than the simpler model lme.
%interaction is better!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compare models with and without random slope                 
Mixedlm = fitlme(databyItem_table_block1_Curv, 'correctRTs ~  SizeCond * CurvCond + (1|participant) + (1|image) + (1|current_task)', 'DummyVarCoding','effects');
Mixedlm_slope = fitlme(databyItem_table_block1_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)  + (1|image)  + (1|current_task)', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_slope,'CheckNesting',true)
% The small p-value of 0 indicates that model altlme is significantly better than the simpler model lme.
%random slope is better!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compare models with and without random image                 
Mixedlm = fitlme(databyItem_table_block1_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant)  ', 'DummyVarCoding','effects');
Mixedlm_image = fitlme(databyItem_table_block1_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image) ', 'DummyVarCoding','effects');

compare(Mixedlm,Mixedlm_image)
% The small p-value indicates that model altlme is significantly better than the simpler model lme.
%model with image is better!


Mixedlm_winner_RT = fitlme(databyItem_table_block1_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image)  ', 'DummyVarCoding','effect');
anova(Mixedlm_winner_RT)
Mixedlm_winner_RT.Rsquared.Adjusted
Mixedlm_winner_dummy = fitlme(databyItem_table_block1_Curv, 'correctRTs ~  SizeCond * CurvCond + (1+SizeCond * CurvCond|participant) + (1|image) ');
Mixedlm_winner_dummy.Rsquared.Adjusted

