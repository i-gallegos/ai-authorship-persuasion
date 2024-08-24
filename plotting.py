import pandas as pd
import numpy as np
import os

import matplotlib.pyplot as plt
import seaborn as sns

'''
Data
'''
df = pd.read_csv('data/LLM-Persuasion-FINAL-July1_cleaned.csv')


'''
Analysis
'''
# Define useful variables
os.makedirs(f'./results/', exist_ok=True)

conditions = ['LLM', 'HUM', 'NONE']

dvs = ['support', 'confidence', 'sharing', 'accuracy']

colors = {
    'LLM':'#2f4b7c', 
    'HUM':'#d55087', 
    'NONE':'#ffa600'
}

topic_mapping = {
    1:'Geoengineering',
    2:'Drug Importation',
    3:'College Athlete\nSalaries',
    4:'Social Media\nPlatform Liability',
}

label_mapping = {
    'HUM':'Human\nLabel',
    'LLM':'AI\nLabel',
    'NONE':'No\nLabel'
}


# Descriptive and summary stats
def descriptive_stats():
    print(f'Number of participants: {len(df)}\n')
    print(f'Condition assignment:\n{df.groupby(by=["condition", "topic"]).size()}\n')
    
    print(f'Demographics:')
    print(df.RACEETH.value_counts() / len(df) * 100, end='\n\n')
    print(df.PARTY.value_counts() / len(df) * 100, end='\n\n')
    print(df.EDUC.replace({
        'Bachelor degree':'Bachelors+',
        'Master\'s degree':'Bachelors+',
        'Professional or academic doctorate degree':'Bachelors+',
        'Some college':'Some college',
        '2-year degree (e.g., associate degree)':'Some college',
    }).value_counts() / len(df) * 100, end='\n\n')
    print(df.EXPERIENCE.value_counts() / len(df) * 100, end='\n\n')


def overall_stats():
    summary = df[[f'dv_{dv}' for dv in dvs] + ['condition']].groupby('condition').agg(['mean', 'sem']) 

    def format_mean_sd(mean, std):
        return f"{mean:.2f} ± {std:.2f}"
    
    summary_rounded = summary.round(2)
    formatted_summary = pd.DataFrame(index=summary.index)
    for dv in ['dv_support', 'dv_confidence', 'dv_sharing', 'dv_accuracy']:
        formatted_summary[dv] = summary_rounded.apply(
            lambda row: format_mean_sd(row[(dv, 'mean')], row[(dv, 'sem')]), axis=1
        )
    formatted_summary.to_latex(f'results/dv_summary_stats.txt')


# Plotting
def plot_bar(column):
    fig, axes = plt.subplots(1, 5, figsize=(10.7, 2.5))
    axes = axes.flatten()
    
    min_ylim = 100
    max_ylim = 0
    for i, ax in enumerate(axes):
        if i==0:
            data = df
            ax.set_title('All Policies', y=1.08)
        else:
            data = df[df.topic==i]
            y = 1 if '\n' in topic_mapping[i] else 1.08
            ax.set_title(topic_mapping[i], y=y)

        sns.barplot(
            data=data,  x='condition', y=f'dv_{column}', ax=ax, 
            hue='condition', order=conditions, hue_order=conditions,
            errorbar=('ci', 95), 
            legend=False,
            palette=colors
        )
        if i==0:
            if 'accuracy' in column.lower() or 'sharing' in column.lower():
                ax.set_ylabel(f'{column.capitalize()}')   
            else:
                ax.set_ylabel(f'Change in {column.capitalize()}', fontsize=11)  
        else:
            ax.set_ylabel('')
        ax.set_xlabel('')
        ax.set_xticklabels([label_mapping[label] for label in conditions], fontsize=11)
        ax.axhline(0, linewidth=0.5, color='black')
        
        min_ylim = np.minimum(min_ylim, ax.get_ylim()[0])
        max_ylim = np.maximum(max_ylim, ax.get_ylim()[1])

    for ax in axes:
        ax.set_ylim([min_ylim, max_ylim])

    plt.tight_layout()
    sns.despine() 

    fig.savefig(f'results/dv_{column}_bar.png', bbox_inches='tight', dpi=300) 


def plot_equivalence(ax, df, title):
    ax.plot(df.equivalence_bound, df.lower_bound, color='#ff6b58', label='TOST Lower')
    ax.plot(df.equivalence_bound, df.upper_bound, color='#845196', label='TOST Upper')
    ax.set_xlabel('')
    ax.set_ylabel('')
    ax.set_title(title)

    ax.axhline(0.05, linestyle='--', color='black', linewidth=0.5)
    ax.set_ylim([0,1])


def plot_all_equivalence():
    # Run stats analysis first
    ai_human = pd.read_csv(f'results/equivalence_test_ai_human.csv')
    ai_none = pd.read_csv(f'results/equivalence_test_ai_none.csv')

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(10, 4))
    plot_equivalence(ax1, ai_human, 'AI Label v. Human Label')
    plot_equivalence(ax2, ai_none, 'AI Label v. No Label')

    fig.supxlabel('Equivalence Bound')
    fig.supylabel('t-test Signficance')
    ax2.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    
    sns.despine()
    plt.tight_layout()
    fig.savefig(f'results/equivalence.png', bbox_inches='tight', dpi=300) 


'''
Generate Results
'''
def main():
    descriptive_stats()
    overall_stats()

    plot_bar('support')
    plot_bar('confidence')
    plot_bar('sharing')
    plot_bar('accuracy')

    plot_all_equivalence()


if __name__ == "__main__":
    main()