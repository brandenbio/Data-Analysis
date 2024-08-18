clear

%pSet = 0;
t1 = [];
for i = 1:3
pSet = i-1;
if pSet == 0
    subs = {'s001','s002','s003','s004','s005'};
    fPath = ['C:' filesep 'Users' filesep 'bbio' filesep 'Documents' filesep 'GrazianoLab' filesep...
        '2-Spatial_Transfer_in_Patients' filesep 'cc' filesep '2_Head' filesep 'raw_data'];
elseif pSet == 1
    subs = {'s006','s007','s008','s010','s011'};
    fPath = ['C:' filesep 'Users' filesep 'bbio' filesep 'Documents' filesep 'GrazianoLab' filesep...
        '2-Spatial_Transfer_in_Patients' filesep 'cc' filesep '1_Head_Tilt' filesep 'raw_data'];
elseif pSet == 2
    subs = {'s012','s013','s014','s015'};
    fPath = ['C:' filesep 'Users' filesep 'bbio' filesep 'Documents' filesep 'GrazianoLab' filesep...
        '2-Spatial_Transfer_in_Patients' filesep 'cc' filesep '1_Head_Tilt' filesep 'raw_data'];
end
subs = string(subs);
acc_all = [];
numAns_array = [];
for subjectID = subs
filename = sprintf('CCTM_%s_results.mat',subjectID);
resFile = [fPath filesep filename];
if exist(resFile, 'file')
  load(resFile)
else
  continue
end

N_runs = numel(data.conds(end,:));
N_trials = numel(data.conds(:,end));

corrResp = cell2mat(data.corrResp(:));
correct_left = corrResp == 1;
correct_right = corrResp == 2;
subjResp = cell2mat(data.subjResp(:));

subjLeft = subjResp == 1;
subjRight = subjResp == 2;

N_total_trials = numel(data.conds);
missed_responses = isnan(subjResp);
cue_onsets = cell2mat(data.cueStart(:));
percent_answered = ((N_total_trials-sum(missed_responses))/N_total_trials) * 100;
numAns_array = [numAns_array; percent_answered];

conds = data.conds(:);
corrLeft = {'A1NH';'A1NL';'A1SH';'A2SL';'B1NH';'B1NL';'B2SH';'B1SL'};
corrRight = {'A2NH';'A2NL';'A2SH';'A1SL';'B2NH';'B2NL';'B1SH';'B2SL'};

switch_trial = nan(N_total_trials,1);
noswitch_trial = nan(N_total_trials,1);
blocked_trial = nan(N_total_trials,1);
unblocked_trial = nan(N_total_trials,1);
left_trial = nan(N_total_trials,1);
right_trial = nan(N_total_trials,1);
trialType = cell(N_total_trials,1);
subjName = cell(N_total_trials,1);

for t = 1:N_total_trials
    condition = conds{t};
    subjName{t} = subjectID;
    if any(strfind(condition,'S'))
        switch_trial(t) = 1;
        noswitch_trial(t) = 0;
    elseif any(strfind(condition,'N'))
        switch_trial(t) = 0;
        noswitch_trial(t) = 1;
    end
    if (any(strfind(condition,'A')) && any(strfind(condition,'H'))) || ...
            (any(strfind(condition,'B')) && any(strfind(condition,'L')))
        blocked_trial(t) = 1;
        unblocked_trial(t) = 0;
    elseif (any(strfind(condition,'A')) && any(strfind(condition,'L'))) || ...
            (any(strfind(condition,'B')) && any(strfind(condition,'H')))
        blocked_trial(t) = 0;
        unblocked_trial(t) = 1;
    end
    if any(strcmp(condition, corrLeft))
        left_trial(t) = 1;
        right_trial(t) = 0;
    elseif any(strcmp(condition, corrRight))
        left_trial(t) = 0;
        right_trial(t) = 1;
    end
    if any(strfind(condition,'S'))
        if (any(strfind(condition,'A')) && any(strfind(condition,'H'))) || ...
            (any(strfind(condition,'B')) && any(strfind(condition,'L')))
            if any(ismember(corrLeft,condition))
                trialType{t} = 'BS-L';
            elseif any(ismember(corrRight,condition))
                trialType{t} = 'BS-R';
            end
        elseif (any(strfind(condition,'A')) && any(strfind(condition,'L'))) || ...
                (any(strfind(condition,'B')) && any(strfind(condition,'H')))
            if any(ismember(corrLeft,condition))
                trialType{t} = 'nBS-L';
            elseif any(ismember(corrRight,condition))
                trialType{t} = 'nBS-R';
            end
        end
    elseif any(strfind(condition,'N'))
        if (any(strfind(condition,'A')) && any(strfind(condition,'H'))) || ...
            (any(strfind(condition,'B')) && any(strfind(condition,'L')))
        if any(ismember(corrLeft,condition))
                trialType{t} = 'BnS-L';
            elseif any(ismember(corrRight,condition))
                trialType{t} = 'BnS-R';
        end
        elseif (any(strfind(condition,'A')) && any(strfind(condition,'L'))) || ...
                (any(strfind(condition,'B')) && any(strfind(condition,'H')))
            if any(ismember(corrLeft,condition))
                trialType{t} = 'nBnS-L';
            elseif any(ismember(corrRight,condition))
                trialType{t} = 'nBnS-R';
            end
        end
    end
end

blocked_trial_nm = logical(blocked_trial(~missed_responses));
unblocked_trial_nm = logical(unblocked_trial(~missed_responses));
switch_trial_nm = logical(switch_trial(~missed_responses));
noswitch_trial_nm = logical(noswitch_trial(~missed_responses));
left_trial_nm = logical(left_trial(~missed_responses));
right_trial_nm = logical(right_trial(~missed_responses));
BS_trial_nm = blocked_trial_nm & switch_trial_nm;
BnS_trial_nm = blocked_trial_nm & noswitch_trial_nm;
nBS_trial_nm = unblocked_trial_nm & switch_trial_nm;
nBnS_trial_nm = unblocked_trial_nm & noswitch_trial_nm;

%BS=Blocked-Switch
%BnS=Blocked-No Switch
%nBS=Not Blocked-Switch
%nBnS=Not Blocked-No Switch
correctResps = corrResp == subjResp;
responded_correctly = corrResp(~missed_responses) == subjResp(~missed_responses);
responded_correctly_blocked = responded_correctly(blocked_trial_nm);
responded_correctly_unblocked = responded_correctly(unblocked_trial_nm);
responded_correctly_switch = responded_correctly(switch_trial_nm);
responded_correctly_noswitch = responded_correctly(noswitch_trial_nm);
responded_correctly_BS = responded_correctly(BS_trial_nm);
responded_correctly_BnS = responded_correctly(BnS_trial_nm);
responded_correctly_nBS = responded_correctly(nBS_trial_nm);
responded_correctly_nBnS = responded_correctly(nBnS_trial_nm);
responded_correctly_left = responded_correctly(left_trial_nm);
responded_correctly_right = responded_correctly(right_trial_nm);

N_left_detections = sum(responded_correctly_left);
N_left_trials = sum(left_trial_nm);
left_detection_rate = N_left_detections / N_left_trials;
CI_left = binomial_mean(left_detection_rate,N_left_trials);

N_right_detections = sum(responded_correctly_right);
N_right_trials = sum(right_trial_nm);
right_detection_rate = N_right_detections / N_right_trials;
CI_right = binomial_mean(right_detection_rate,N_right_trials);

N_total_detections = N_right_detections+N_left_detections;
N_total_trials = N_right_trials+N_left_trials;
total_detection_rate = N_total_detections / N_total_trials;
CI_total = binomial_mean(total_detection_rate,N_total_trials);

N_BS_detections = sum(responded_correctly_BS);
N_BS_trials = sum(BS_trial_nm);
BS_detection_rate = N_BS_detections / N_BS_trials;
CI_BS = binomial_mean(BS_detection_rate,N_BS_trials);

N_BnS_detections = sum(responded_correctly_BnS);
N_BnS_trials = sum(BnS_trial_nm);
BnS_detection_rate = N_BnS_detections / N_BnS_trials;
CI_BnS = binomial_mean(BnS_detection_rate,N_BnS_trials);

N_nBS_detections = sum(responded_correctly_nBS);
N_nBS_trials = sum(nBS_trial_nm);
nBS_detection_rate = N_nBS_detections / N_nBS_trials;
CI_nBS = binomial_mean(nBS_detection_rate,N_nBS_trials);

N_nBnS_detections = sum(responded_correctly_nBnS);
N_nBnS_trials = sum(nBnS_trial_nm);
nBnS_detection_rate = N_nBnS_detections / N_nBnS_trials;
CI_nBnS = binomial_mean(nBnS_detection_rate,N_nBnS_trials);

acc_BS = mean(responded_correctly_BS);
acc_BnS = mean(responded_correctly_BnS);
acc_nBS = mean(responded_correctly_nBS);
acc_nBnS = mean(responded_correctly_nBnS);

condAcc = [BS_detection_rate*100, BnS_detection_rate*100, nBS_detection_rate*100, nBnS_detection_rate*100];
acc_all = [acc_all; condAcc];
end

acc_all_means = mean(acc_all);
acc_all_se = se_mean(acc_all);

condSet = ["BS" "BnS" "nBS" "nBnS"];

%% Group Bar Graphs
figure
hold on
bar(acc_all_means, 'FaceColor', [0 0 0]);
errorbar(acc_all_means,acc_all_se,'k.')
set(gca,'XTickLabel',{'BS','BnS','nBS','nBnS'})
title('Group Accuracies')
ylabel('Accuracy')
set(gcf,'color','w')
ylim([0 100])
set(gca,'XTick',1:4)
%%
%% Group Violin Graphs
% condSet = cellstr(condSet);
% figure
% vs = violinplot(acc_all, condSet);
% %title('Group Accuracies')
% ylabel('Accuracy')
% xlim([0.5, 4.5]);
% %%
% 
% a1 = acc_all(:,1);
% b1 = acc_all(:,2);
% c1 = acc_all(:,3);
% d1 = acc_all(:,4);
% t0 = vertcat(a1,b1,c1,d1);
% if i == 1
%     t1a = t0;
% elseif i == 2
%     t1b = t0;
% end
end
% t2 = ["BS" "BS" "BS" "BS" "BS" "BnS" "BnS" "BnS" "BnS" "BnS" "nBS" "nBS" "nBS" "nBS" "nBS"...
%    "nBnS" "nBnS" "nBnS" "nBnS" "nBnS"]';

% tbl1 = table(t1a,t2);
% tbl2 = table(t1b,t2);
% x1 = categorical(tbl1.t2,condSet);
% y1 = tbl1.t1a;
% x2 = categorical(tbl2.t2,condSet);
% y2 = tbl2.t1b;
% swarmchart(x1,y1,20,'filled');
% hold on
% swarmchart(x2,y2,20,'filled');
