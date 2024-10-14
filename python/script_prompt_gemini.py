import google.generativeai as genai
import os
import pandas as pd

genai.configure(api_key=os.environ["GEMINI_API_KEY"])


model = genai.GenerativeModel("gemini-1.5-flash")
response1 = model.generate_content("Write a story about a magic backpack.")
print(response1.text)



filename = 'data/output/merged_file_ACDC_reports.txt'  
with open(filename, 'r') as file:
    text = file.read()


model = genai.GenerativeModel("gemini-1.5-pro-latest")
response = model.generate_content(["Give me a summary of this text", text])
print(response.text)


model = genai.GenerativeModel("gemini-1.5-pro-001")
response = model.generate_content(["""I will give you a text input that was the result of some processing with OCR algorithms.
The text comes from the Africa CDC weekly reports that publish data on epidemiological situation for various diseases for many countries in the Africa continent.
The reports include a Date of issue usually at the top of their first page.
Could you please extract only the Mpox virus relevant data from the text I have uploaded? 
The data are usually presented in tables with headers such as Agent/Syndrome, Country, Risk:Human, Risk:Animal, Type, Suspected (New), Probable (New), Confirmed (New), Deaths (New).
There should be data from multiple dates in the text. 
There might be data for Mpox virus found in more than one table in the text. Please make an exhaustive search before you return results. 
Please make sure you add the date column, coming from the date of issue of the report, in the table in this format: DD-MM-YYYY. 
Please make sure the numbers in the parentheses are in new columns you wil create which will contain the _new suffix.
Also, please make sure you include all the dates found in the text.
As an output I would like a markdown table with the numbers of cases and another markdown table with all the dates of submissions.
"""

, text])
print(response.text)





# Assuming 'markdown_table' is the string containing the markdown table
from io import StringIO

df = pd.read_csv(StringIO(response.text), sep="|", header=0)

# Clean up the dataframe by removing extra whitespace and the initial '---' separator row
df = df.iloc[1:].apply(lambda x: x.str.strip())

# Export to CSV
df.to_csv("mpox_data_ACDC_reports_updated.csv", index=False)


