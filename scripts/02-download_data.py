import requests
import pandas as pd
# API Endpoint
url = "https://ieypoqqymehdkesavuan.supabase.co/rest/v1/don_mv"

# Query parameters
# Change donation_year for each year needed
params = {
    "select": "*",
    "limit": "1000",
    "order": "donation_year.desc,amount.desc",
    "donation_year": "eq.2024"
}
# Headers
# Replace # with the appropriate details
headers = {
    "Authorization": "Bearer ##########",
    "Content-Type": "application/json",
    "apikey": "#########"
}

# Make the GET request
response = requests.get(url, params=params, headers=headers)

# Check response status
if response.status_code == 200:
    # Parse the JSON data
    data = response.json()
    print("Data retrieved successfully!")
    print(data)
    df = pd.DataFrame(data)
    file_path = './donation_data_2013.xlsx'
    df.to_excel(file_path, index=False)
else:
    print(f"Failed to fetch data. Status code: {response.status_code}")
    print(response.text)