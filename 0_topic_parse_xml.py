#!/usr/bin/env python
# coding: utf-8

# In[1]:


# pyscience imports
import os
import sys
import glob
import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf

import matplotlib.pyplot as plt
import seaborn as sns
plt.style.use("seaborn-darkgrid")
# plt.style.use("dark_background")
sns.set(style="ticks", context="talk")
# %matplotlib inline
# run for jupyter notebook
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'


# In[2]:


from pprint import pprint
import re
import string
import numpy as np
import pandas as pd
import copy
import itertools
from bs4 import BeautifulSoup
import xml.etree.ElementTree as ET
from urllib import request
from collections import Counter
import json
from joblib import Parallel, delayed
import multiprocessing

import nltk
from nltk import word_tokenize
from nltk import bigrams
from nltk import trigrams
from nltk import ngrams
from nltk.stem import PorterStemmer
from nltk import word_tokenize


# In[3]:


dbhome   = '/home/alal/Dropbox/0_GradSchool/1_HW/452/Hansard/'
ricehome = '/home/apoorval/HW/452/Hansard/'
home = ricehome
twfy_root = home + 'input/twfy/'
twfy = twfy_root + '2010+/'
jsons = home + 'input/twfy/jsons/'
get_ipython().run_line_magic('cd', '$home')


# # Parse XMLs 
# Dry run

# In[6]:


file = twfy+'debates2017-11-23c.xml'


# In[7]:


tree = ET.parse(file)


# In[19]:


for x in ET.iterparse(file):
    obj = x[1]
    tag = obj.tag
    
    text = obj.text
    if text:
        text = text.strip()


# ## Function to parse XML and populate list of speeches 

# In[17]:


def twfy_parser(infile, inpdir = twfy, outdir = jsons):
    tree = ET.parse(inpdir + infile)
    root = tree.getroot()
    list_of_speeches = []
    for sp in tree.findall('speech'):
        # dict with metadata
        d =  sp.attrib
        # speech tag
        s = sp.findall('p')
        if s:
            # extract speeches
            statement = [x.text for x in s if x is not None]
            statement = ' '.join([x for x in statement if x])
        d['speech'] = statement
        list_of_speeches.append(d)
    # if there is a single speech
    if len(list_of_speeches) > 0:
        outfile = outdir + infile.replace('xml', 'json')
        json.dump(list_of_speeches, open(outfile, 'w'))
        return list_of_speeches


# In[18]:


parl = os.listdir(twfy)


# ## Parallelise Parsing and Writing to JSON

# In[19]:


get_ipython().run_cell_magic('time', '', 'num_cores = multiprocessing.cpu_count()\nres = Parallel(n_jobs = num_cores)(delayed(twfy_parser)(p) for p in parl)')


# In[20]:


len(res)


# In[31]:


speeches = [x for x in res if x is not None]
len(speeches)
big_speech_list = [speech for day in speeches for speech in day]


# In[33]:


len(big_speech_list)


# In[35]:


outfile = jsons + '_all_speeches.json'
json.dump(big_speech_list, open(outfile, 'w'))


# # Read JSONs

# In[36]:


big_speech_list = json.load(open(jsons + '_all_speeches.json'))


# In[38]:


len(big_speech_list)


# In[47]:


speakers = {}


# In[48]:


big_speech_list[:5]


# In[49]:


for x in big_speech_list:
    speaker = x.get("speakerid", None)
    if speaker is not None:
        speakers[speaker] = x['speakername']


# In[55]:


speaks = pd.DataFrame.from_dict(speakers, orient = 'index')
speaks.columns = ['person_name']


# In[56]:


speaks.head()


# In[61]:


get_ipython().run_line_magic('cd', '$twfy_root')
get_ipython().run_line_magic('mkdir', 'tmp -p')


# In[62]:


speaks.to_csv(twfy_root + 'tmp/speaker_list.csv')


# In[ ]:




