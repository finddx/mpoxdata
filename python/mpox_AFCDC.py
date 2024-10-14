#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os
import re
import requests
import time
from urllib.parse import urlparse, parse_qs
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.service import Service
from selenium.common.exceptions import NoSuchElementException
from webdriver_manager.firefox import GeckoDriverManager

download_dir = os.path.join(os.getcwd(), "data", "pdf")
os.makedirs(download_dir, exist_ok=True)

url = "https://africacdc.org/resources/?wpv_aux_current_post_id=217&wpv_view_count=549&wpv-resource-type=ebs-weekly-reports"

#Firefox WebDriver
driver = webdriver.Firefox(service=Service(GeckoDriverManager().install()))

#Open the website
driver.get(url)
time.sleep(2)

row = 1

#Iterate over rows until no more rows are found
while True:
    try:
        #Loop through the two columns (1 and 2) per row
        for col in [1, 2]:
            #XPath for each link
            xpath = f"/html/body/div[2]/section[2]/div/div/div/div/div/div[2]/div[{row}]/div[{col}]/a"
            month_link = driver.find_element(By.XPATH, xpath)
            month_url = month_link.get_attribute("href")
            
            #Open the month page
            driver.get(month_url)
            time.sleep(3)
            
            #Find all PDF on the month page
            pdf_links = []
            for pdf_row in range(1, 6):  #(up to 5 weeks per month)
                pdf_xpath = f"/html/body/div[2]/section[2]/div/div/div/section/div/div[1]/div/div/div/div/div/div[2]/div/table/tbody/tr[{pdf_row}]/td[2]/a"
                try:
                    pdf_link = driver.find_element(By.XPATH, pdf_xpath)
                    pdf_links.append(pdf_link)
                except NoSuchElementException:
                    print(f"No PDF link found for row {pdf_row}, skipping.")
            
            #Iterate over all PDF and download them
            for pdf_link in pdf_links:
                pdf_url = pdf_link.get_attribute("href")

                #Parse the URL to extract the filename
                parsed_url = urlparse(pdf_url)
                query_params = parse_qs(parsed_url.query)

                #Extract the filename from the query parameters
                if 'filename' in query_params:
                    pdf_name = query_params['filename'][0]
                else:
                    #If filename is not found; use the last part of the URL
                    pdf_name = pdf_url.split("/")[-1].split("?")[0]

                #Ensure has a .pdf extension
                if not pdf_name.endswith('.pdf'):
                    pdf_name += '.pdf'

                #Sanitize the PDF name and save PDF
                pdf_name = re.sub(r'[<>:"/\\|?*]', '', pdf_name)
                pdf_path = os.path.join(download_dir, pdf_name)
                print(f"Downloading PDF: {pdf_url} to {pdf_path}")

                #Download the PDF file
                response = requests.get(pdf_url, stream=True)

                if response.status_code == 200:
                    with open(pdf_path, 'wb') as pdf_file:
                        pdf_file.write(response.content)
                    print(f"Downloaded: {pdf_name}")
                else:
                    print(f"Failed to download: {pdf_url} - Status Code: {response.status_code}")
            
            #Go back to the main page
            driver.get(url)
            time.sleep(2)

    except NoSuchElementException:
        #If no more elements are found break the loop
        print(f"No more elements found in row {row}, stopping.")
        break
    

    row += 1

driver.quit()

