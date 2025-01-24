import pandas as pd
import os


# Load raw data
save_dir = 'LLM-Persuasion-FINAL-July1'
os.makedirs(f'data/cleaned/{save_dir}/', exist_ok=True)

file = 'data/raw/LLM-Persuasion-FINAL-July1.csv'
demographics_file = 'data/raw/LLM-Persuasion-FINAL-July1_demographics.csv'

header = pd.read_csv(file, nrows=2, low_memory=False)
raw = pd.read_csv(file, skiprows=2, header=0, low_memory=False)
raw.columns = header.columns
raw = raw.set_index('ResponseId')

raw_demographics = pd.read_csv(demographics_file)

# Identify necessary columns
with open(f'data/columns/{save_dir}/columns_controls.txt') as f:
    controls_cols = [line.strip("'\n,") for line in f]

with open(f'data/columns/{save_dir}/columns_metadata.txt') as f:
    metadata_cols = [line.strip("'\n,") for line in f]

with open(f'data/columns/{save_dir}/columns_responses.txt') as f:
    responses_cols = [line.strip("'\n,") for line in f]

with open(f'data/columns/{save_dir}/columns_demographics.txt') as f:
    demographics_cols = [line.strip("'\n,") for line in f]

raw = raw[controls_cols+metadata_cols+responses_cols].merge(raw_demographics[demographics_cols+['Participant id', 'Status']], left_on='PROLIFIC_PID', right_on='Participant id')
controls_cols = controls_cols + demographics_cols
metadata_cols += ['Status']


def filter_participants(df):
    valid_responses = (df['WORKERID']!='test') & \
                      (df['Progress']==100) & \
                      (df['Finished']==True) & \
                      (df['CONSENT_SELECT']=='I agree to participate in this research.') & \
                      (df['DEBRIEF']=='I confirm my consent to participate in this research.') & \
                      (df['Status']=='APPROVED')
    
    valid_participants = valid_responses[valid_responses].index
    passed_attc = (df['ATTC']=='Online sources only,About half online sources').rename('passed_attc')
    return valid_participants, passed_attc


def clean_controls(df):
    df.topic = df.topic.astype(int)

    df_copy = df.copy()
    df_copy['LLM_HUMAN_2'] = df_copy['LLM_HUMAN_2'].replace({
        'Definitely human-written':1,
        'Possibly human-written':2, 
        'Not sure':3, 
        'Possibly AI-generated':4,
        'Definitely AI-generated':5, 
    })

    passed_manip = ((df_copy['condition']=='NONE') | (df_copy['LLM_HUMAN_2']==df_copy['condition']) | (df_copy['LLM_HUMAN_3']=='No')).rename('passed_manip')
    return df, passed_manip


def clean_responses(df, topics):
    grouped_cols = list(zip(*(iter(responses_cols),) * 10))
    grouped_names = [
        'pre_knowledge',
        'pre_support',
        'pre_confidence',
        'expert',
        'expert_no_label',
        'confirm_read',
        'post_support',
        'post_confidence',
        'post_sharing',
        'post_accuracy',
    ]
    
    cleaned_responses = pd.DataFrame(index=df.index, columns=grouped_names)
    for participant in df.index:
        participant_condition = int(topics.loc[participant])
        participant_cols = grouped_cols[participant_condition - 1]

        participant_responses = df.loc[participant, list(participant_cols)]
        participant_responses.index = grouped_names
        cleaned_responses.loc[participant] = participant_responses

    cleaned_responses = cleaned_responses.drop(columns=['expert', 'expert_no_label', 'confirm_read'])
    return cleaned_responses


def clean_data(raw, filter_manip):
    participants, passed_attc = filter_participants(raw[metadata_cols].copy())
    
    controls_raw = raw[controls_cols].merge(passed_attc, left_index=True, right_index=True)
    controls_raw = controls_raw[controls_raw.index.isin(participants)]
    controls = controls_raw.copy()
    if filter_manip:
        controls, passed_manip = clean_controls(controls_raw)
        controls = controls.merge(passed_manip, left_index=True, right_index=True)

    responses_raw = raw[responses_cols].copy()
    responses_raw = responses_raw[responses_raw.index.isin(participants)]
    responses = clean_responses(responses_raw, controls.topic)

    return responses, controls


def main(filter_manip=False):
    responses, controls = clean_data(raw, filter_manip)
    df = responses.merge(controls, left_index=True, right_index=True)
    
    df['dv_support'] = df['post_support'] - df['pre_support']
    df['dv_confidence'] = df['post_confidence'] - df['pre_confidence']
    df['dv_sharing'] = df['post_sharing']
    df['dv_accuracy'] = df['post_accuracy']

    if filter_manip:
        df = df[df.passed_attc & df.passed_manip]
    else:
        df = df[df.passed_attc]

    df.to_csv('data/LLM-Persuasion-FINAL-July1_cleaned.csv', index=False)

    df['LLM_HUMAN_2'] = df['LLM_HUMAN_2'].replace({
        'Definitely human-written':1,
        'Possibly human-written':2, 
        'Not sure':3, 
        'Possibly AI-generated':4,
        'Definitely AI-generated':5, 
    })

    R_columns = [
        'pre_support',
        'post_support',
        'pre_confidence',
        'post_confidence',
        'post_sharing',
        'post_accuracy',
        'topic',
        'condition',
        'pre_knowledge',
        'EXPERIENCE',
        'EDUC',
        'PARTY',
        'Age',
        'Sex',
        'Ethnicity simplified',
        'Nationality',
        'LLM_HUMAN_2',
        'LLM_HUMAN_3'
    ]
    df[R_columns].to_csv(f'data/LLM-Persuasion-FINAL-July1_R.csv', index=False)


if __name__ == "__main__":
    filter_manip = False
    main(filter_manip)
    