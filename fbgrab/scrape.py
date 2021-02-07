from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
import urllib.request

# setup selenium
opts = Options()
opts.add_argument('--headless')
driver = webdriver.Chrome(options=opts)

# set the cookies (requires we load a page first)
driver.get('https://www.facebook.com')
driver.add_cookie({'name': 'sb', 'value': 'XXX'})
driver.add_cookie({'name': 'datr', 'value': 'XXX'})
driver.add_cookie({'name': 'c_user', 'value': 'XXX'})
driver.add_cookie({'name': 'm_pixel_ratio', 'value': 'XXX'})
driver.add_cookie({'name': 'x-referer', 'value': 'XXX'})
driver.add_cookie({'name': 'presence', 'value': 'XXX'})
driver.add_cookie({'name': 'xs', 'value': 'XXX'})
driver.add_cookie({'name': 'fr', 'value': 'XXX'})
driver.add_cookie({'name': 'dpr', 'value': 'XXX'})
driver.add_cookie({'name': 'spin', 'value': 'XXX'})

def save_image(page_url, local_file):
    """Save the main image from the page to the file specified."""
    driver.get(page_url)
    soup_file=driver.page_source
    soup = BeautifulSoup(soup_file)
    print(soup.title.get_text())

    for img in soup.find_all('img'):
        alt = img.get('alt')
        if alt and len(alt) > 0:
            src = img.get('src')
            urllib.request.urlretrieve(src, local_file)

# save all images
with open('../images') as f:
    i = 0
    for l in f.readlines():
        if i > 235:
            photo = f'photo-{i}.jpg'
            save_image(l.strip(), photo)
            print(photo)
            print(l)
        i += 1

driver.quit()
