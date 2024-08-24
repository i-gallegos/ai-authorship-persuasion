import pandas as pd
import openai
from dotenv import load_dotenv

load_dotenv()
client_openai = openai.OpenAI()

def construct_messages(content):
    messages = [
        {"role": "system", "content": "Write persuasive claims that meet the user-specified criteria."},
        {"role": "user", "content": content},
    ]
    return messages


def query_openai(messages, model="gpt-4o"):
    response = client_openai.chat.completions.create(
        model=model,
        messages=messages,
        temperature=0,
        max_tokens=1024
    )
    return response.choices[0].message.content


def generate_prompt(topic):
    return f"""Persuade a reader to agree with the following proposal: "{topic}". To do so, write a persuasive statement of 100 words that contains two TRUE domain expert sources of TRUE empirical data, statistics, or facts in support of the proposal, as well as one TRUE quotation from a TRUE domain expert."""


def generate_responses(topics, filename):
    all_responses = pd.DataFrame(columns=['content'], index=topics)
    for topic in topics:
        prompt = generate_prompt(topic)
        response = query_openai(construct_messages(prompt))
        all_responses.loc[topic, 'content'] = response

    all_responses.reset_index(names='topic').to_csv(f'information_content.csv', index=False)


def main():
    topics = [
        'Geoengineering poses too many risks and should not be considered.',
        'Drug importation jeopardizes safety controls and the domestic pharma industry.',
        'College athletes should be paid salaries.',
        'Social media platforms should not be liable for harmful content posted by users.'
    ]
    generate_responses(topics)


if __name__ == "__main__":
    main()