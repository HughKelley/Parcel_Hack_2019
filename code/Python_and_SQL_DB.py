# -*- coding: utf-8 -*-
"""
Created on Mon Feb 25 11:04:38 2019

@author: Hugh
"""


#####
# if you got through the week 4 practical, this should work 
# you may need to add the pymsql drivers to the environment you're working in



#create dataframe

import pandas as pd

#this prob needs to be changed to the directory of your data
data = pd.read_csv('Digify/HeightRestrictionDataTfLConfidential.csv')

# More information on how to create these connections are provided here: 
# http://docs.sqlalchemy.org/en/rel_0_9/core/engines.html


# import the SQLAlchemy libraries

# this gives a warning from the idea because of possiblity of name collisions
from sqlalchemy import *

#from sqlalchemy import create_engine

# create the connection string to the MySQL database
# replace USERNAME and PASSWORD with your own credentials 
engine = create_engine('mysql+pymysql://ucfnhke:baleyakuli@dev.spatialdatacapture.org:3306/ucfnhke')

# make the connection to the database
conn = engine.raw_connection()


# TO UPLOAD DATA
# data.to_sql('ph_height_restrictions', con= engine)

# tO RETREIVE DATA
data_from_sql = pd.read_sql('SELECT * FROM PH_Jobs', conn)


##########################################
# Database info

metadata = MetaData()

# lists names (keys) of tables in database
metadata.tables.keys()

# to see database metadata

from sqlalchemy.ext.declarative import *

Base = declarative_base()

metadata.reflect(engine)
