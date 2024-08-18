clear
subs = {'s010','s011','s012','s013','s014','s015','s016','s017','s018','s019',...
    's020','s021','s022','s023','s024','s025','s026','s027','s028','s029',...
    's030','s031','s032','s033','s034','s035','s036','s037','s038','s039',...
    's040','s041','s042'};
subs = string(subs);
RTs_all = [];
DRs_all = [];
acc_all = [];
acc_se_all = [];
numAns_array = [];
for subjectID = subs

filename = sprintf('fMRITM_%s_results.mat',subjectID);
resFile = [filesep filesep 'bucket' filesep 'graziano' filesep 'branden'...
     filesep 'fMRI_Project' filesep 'raw_data' filesep 'raw_behavioral' filesep filename];
 
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

