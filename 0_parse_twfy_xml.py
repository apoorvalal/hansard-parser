#!/usr/bin/env python
# coding: utf-8

# In[1]:


# pyscience imports
import os
import sys
import glob
import numpy as np
import pandas as pd
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

from pprint import pprint
import re
import string
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


#%%

dbhome   = '/home/alal/Dropbox/0_GradSchool/1_HW/452/Hansard/'
ricehome = '/home/apoorval/HW/452/Hansard/'
home = ricehome
twfy_root = home + 'input/twfy/'
twfy = twfy_root + '2010+/'
jsons = home + 'input/twfy/jsons/'
%cd $home
#%%

# # Parse XMLs
# Dry run

# ## Function to parse XML and populate list of speeches


########     ###    ########   ######  ########
##     ##   ## ##   ##     ## ##    ## ##
##     ##  ##   ##  ##     ## ##       ##
########  ##     ## ########   ######  ######
##        ######### ##   ##         ## ##
##        ##     ## ##    ##  ##    ## ##
##        ##     ## ##     ##  ######  ########


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


#%%
parl = os.listdir(twfy)

# ## Parallelise Parsing and Writing to JSON
#%%

%%time
num_cores = multiprocessing.cpu_count()
res = Parallel(n_jobs = num_cores)(delayed(twfy_parser)(p) for p in parl)
#%% massive list
speeches = [x for x in res if x is not None]
len(speeches)
big_speech_list = [speech for day in speeches for speech in day]
len(big_speech_list)
#%%
outfile = jsons + '_all_speeches.json'
json.dump(big_speech_list, open(outfile, 'w'))
#%%
# # Read JSONs

########  ########    ###    ########
##     ## ##         ## ##   ##     ##
##     ## ##        ##   ##  ##     ##
########  ######   ##     ## ##     ##
##   ##   ##       ######### ##     ##
##    ##  ##       ##     ## ##     ##
##     ## ######## ##     ## ########


big_speech_list = json.load(open(jsons + '_all_speeches.json'))
len(big_speech_list)
big_speech_list[:5]

#%% convert to csv
df = pd.DataFrame(big_speech_list)

df.head()

df.to_csv(twfy_root + 'speeches.csv')

#%%

########  ######## ########   ######   #######  ##    ## #### ########
##     ## ##       ##     ## ##    ## ##     ## ###   ##  ##  ##     ##
##     ## ##       ##     ## ##       ##     ## ####  ##  ##  ##     ##
########  ######   ########   ######  ##     ## ## ## ##  ##  ##     ##
##        ##       ##   ##         ## ##     ## ##  ####  ##  ##     ##
##        ##       ##    ##  ##    ## ##     ## ##   ###  ##  ##     ##
##        ######## ##     ##  ######   #######  ##    ## #### ########

speakers = {}
for x in big_speech_list:
    speakername = x.get("speakername", None)
    personid    = x.get("person_id", None)
    speakerid    = x.get("speakerid", None)
    if speakername is not None: # omit interruptions
        if speakers.get(speakername, None) is None:
            speakers[speakername] = {}
        if x.get(personid, None) is not None:
            speakers[speakername]['personid'].append(personid)
        else:
            speakers[speakername]['personid'] = personid
        if x.get(speakerid, None) is not None:
            speakers[speakername]['speakerid'].append(speakerid)
        else:
            speakers[speakername]['speakerid'] = speakerid

#%%


{k: speakers[k] for k in list(speakers)[:5]}


#%%
speaks = pd.DataFrame.from_dict(speakers, orient = 'index')
speaks.columns = ['person_id', 'speaker_id']
#%%
speaks.head()
#%%
%cd $twfy_root
%mkdir tmp -p
speaks.to_csv(twfy_root + 'tmp/speaker_list.csv')
#%%
