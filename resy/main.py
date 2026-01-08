import urllib.request
import time

# from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait


# setup selenium
opts = Options()
opts.add_argument('--headless')
driver = webdriver.Chrome(options=opts)
driver.set_window_size(1500,1000)

# get resy homepage
driver.get('https://resy.com')
# soup_file = driver.page_source
# soup = BeautifulSoup(soup_file, features="html.parser")
# print(soup.title.get_text())

# click login button
buttons = driver.find_elements(By.XPATH, '//button[text()="Log in"]')
assert(len(buttons) == 1)
b = buttons[0]
b.click()
WebDriverWait(driver, 10).until(
    EC.element_to_be_clickable((By.XPATH, '//button[text()="Use Email and Password instead"]'))
)
driver.save_screenshot('shots_login_1.png')

# click 'email & password' button
buttons = driver.find_elements(By.XPATH, '//button[text()="Use Email and Password instead"]')
assert(len(buttons) == 1)
b = buttons[0]
b.click()
time.sleep(2)
driver.save_screenshot('shots_login_2.png')

# fill in login info
field_email = driver.find_elements(By.ID, 'email')
field_passw = driver.find_elements(By.ID, 'password')
assert(len(field_email) == 1)
assert(len(field_passw) == 1)

field_email[0].send_keys('resy@davidterei.com')
field_passw[0].send_keys('Nedisgay')

# login
buttons = driver.find_elements(By.XPATH, '//button[text()="Continue"]')
assert(len(buttons) == 1)
b = buttons[0]
b.click()
time.sleep(2)
driver.save_screenshot('shots_login_3.png')

# print session
print(driver.get_cookies())

# browse to TATIANA page
driver.get('https://resy.com/cities/ny/tatiana')
time.sleep(1)
driver.save_screenshot('shots_tatiana.png')

driver.quit()
