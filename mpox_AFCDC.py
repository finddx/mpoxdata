#!/usr/bin/env python
# coding: utf-8

# In[99]:


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

# Ensure the directory exists
os.makedirs(download_dir, exist_ok=True)

# Set up the Firefox WebDriver
driver = webdriver.Firefox(service=Service(GeckoDriverManager().install()))

# URL of the Africa CDC resource page
url = "https://africacdc.org/resources/?wpv_aux_current_post_id=217&wpv_view_count=549&wpv-resource-type=ebs-weekly-reports"

# Open the main website
driver.get(url)
time.sleep(5)  # Allow the page to load

row = 3

# Outer loop: We iterate over rows until no more rows are found
while row < 4:  # Change this condition if you want more rows
    try:
        # Inner loop: Loop through the two columns (1 and 2) per row
        for col in [1, 2]:
            # Dynamically construct the XPath for each link (month link)
            xpath = f"/html/body/div[2]/section[2]/div/div/div/div/div/div[2]/div[{row}]/div[{col}]/a"
            
            # Try to locate the element using the dynamic XPath
            month_link = driver.find_element(By.XPATH, xpath)
            month_url = month_link.get_attribute("href")
            
            # Open the month page
            driver.get(month_url)
            time.sleep(5)  # Allow the page to load
            
            # Find all PDF download links on the month page
            pdf_links = []
            for pdf_row in range(1, 6):  # Iterate over rows (up to 5)
                pdf_xpath = f"/html/body/div[2]/section[2]/div/div/div/section/div/div[1]/div/div/div/div/div/div[2]/div/table/tbody/tr[{pdf_row}]/td[2]/a"
                try:
                    pdf_link = driver.find_element(By.XPATH, pdf_xpath)
                    pdf_links.append(pdf_link)
                except NoSuchElementException:
                    print(f"No PDF link found for row {pdf_row}, skipping.")  # Debugging
            
            # Iterate over all found PDF links and download them
            for pdf_link in pdf_links:
                pdf_url = pdf_link.get_attribute("href")

                # Parse the URL to extract the filename from the query string
                parsed_url = urlparse(pdf_url)  # Use urlparse from urllib.parse
                query_params = parse_qs(parsed_url.query)  # Use parse_qs from urllib.parse

                # Extract the filename from the query parameters
                if 'filename' in query_params:
                    pdf_name = query_params['filename'][0]  # Get the first filename value
                else:
                    # Fallback if filename is not found; use the last part of the URL
                    pdf_name = pdf_url.split("/")[-1].split("?")[0]

                # Ensure the filename has a .pdf extension
                if not pdf_name.endswith('.pdf'):
                    pdf_name += '.pdf'

                # Sanitize the PDF name to make it filesystem-friendly
                pdf_name = re.sub(r'[<>:"/\\|?*]', '', pdf_name)
                pdf_path = os.path.join(download_dir, pdf_name)  # Save PDF to the download directory

                # Debugging: Show the PDF URL and path
                print(f"Downloading PDF: {pdf_url} to {pdf_path}")

                # Download the PDF file using requests
                response = requests.get(pdf_url, stream=True)

                if response.status_code == 200:  # Check if the request was successful
                    with open(pdf_path, 'wb') as pdf_file:
                        pdf_file.write(response.content)
                    print(f"Downloaded: {pdf_name}")
                else:
                    print(f"Failed to download: {pdf_url} - Status Code: {response.status_code}")
            
            # Go back to the main page to process the next month
            driver.get(url)
            time.sleep(5)

    except NoSuchElementException:
        # If no more elements are found in this row, we break out of the loop
        print(f"No more elements found in row {row}, stopping.")
        break  # Stop the while loop when no more rows are found
    
    # Move to the next row after both columns are processed
    row += 1

driver.quit()

