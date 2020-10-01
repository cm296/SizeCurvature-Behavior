clear all
close all
clc

load('Analysis_cm/AllData')
head(AllData)

%% Make variables categorical
AllData.Participant = categorical(cellstr(AllData.Participant));
categories(AllData.Participant)

AllData.CurvCond = categorical(cellstr(AllData.CurvCond));
AllData.SizeCond = categorical(cellstr(AllData.SizeCond));
AllData.current_task = categorical(cellstr(AllData.current_task));

%interaction term
inter_temp = AllData.CurvCond.*AllData.SizeCond;
categories(inter_temp)
AllData.inter_temp = removecats(inter_temp); %drop unused categories
categories(AllData.inter_temp)

AllData.buttonpress = removecats(buttonpress); %drop unused categories

%%run mixed effect model with subject 
%%RT has interaction of size and curvature rating, with random effect of
%%task and participant
head(AllData)
Mixedlm_fs = fitlme(AllData, 'RT_log ~  inter_temp + (inter_temp|Participant)  + (1|current_task)', 'DummyVarCoding','effects');
Mixedlm_fs

Mixedlm_fs.Coefficients(1:2,1:2)  %Starts with CondIndex1, sanity check that it's effect coding and not dummy coding!

FixedCoeffs_fs = Mixedlm_fs.Coefficients(:,2);
FixedCoeffs_fs = double(FixedCoeffs_fs);
FixedCoeffs_fs_NoInter = FixedCoeffs_fs(2:end);  %%without intercept
RandomEffects_fs = Mixedlm_fs.randomEffects;
%%missing coefficient is just the difference between sum and zero since all
%%missing coefficient is just the difference between sum and zero since all
%%coefficients need to sum up to zero
FixedCoeffs_fs_NoInter(size(FixedCoeffs_fs_NoInter,1)+1)= 0 - sum(FixedCoeffs_fs_NoInter);

pVal = coefTest(Mixedlm_fs)
%%interaction of curvature and size is statistically significant

