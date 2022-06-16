import os
import pandas as pd
import numpy as np
import geopandas as gpd
import fiona
from functools import reduce
import itertools

station_folder = "G:/My Drive/NRRI/Zillow Project/Water Quality Hedonic/Water_Data/EPA_WQP/Mar 2021"
station_file = os.path.join(station_folder, "station - US - Lake.zip")
result_file = os.path.join(station_folder, "result - US - Lake.zip")
output_file = os.path.join(station_folder, 'us_epa.csv')
char_output = os.path.join(station_folder, 'us_characteristics.csv')
ext_folder = 'D:/NHD_H_National_GDB'

test_csv = os.path.join(station_folder, "test.csv")


#######################################################################################################################
# LOAD WATERBODY THAT HAS WATER DATA
nhd_lake_water = gpd.read_file(os.path.join(ext_folder, 'nhd_lake_water.shp'))

nhd_lake_water_buffer = nhd_lake_water.copy()
nhd_lake_water_buffer = nhd_lake_water_buffer.to_crs('epsg:5070')
nhd_lake_water_buffer['geometry'] = nhd_lake_water_buffer.geometry.buffer(100)
#######################################################################################################################
#LOAD STATION DATA AND KEEP ONLY LAKES AND CREATE GDF
station_all = pd.read_csv(station_file)

keep_station = ['MonitoringLocationIdentifier', 'MonitoringLocationName', 'MonitoringLocationTypeName', 'HUCEightDigitCode',
                'HorizontalCoordinateReferenceSystemDatumName','LatitudeMeasure', 'LongitudeMeasure', 'StateCode']

# the us data we downloaded is after filtering of lakes. the unique values for 'MonitoringLocationTypeName' are ['Lake, Reservoir, Impoundment', 'Lake', 'Riverine Impoundment',
#       'Reservoir', 'Great Lake']. so we filter out specifically for all of these to make sure any data will work.
station = station_all[keep_station][station_all['MonitoringLocationTypeName'].isin(['Lake, Reservoir, Impoundment', 'Lake', 'Riverine Impoundment',
       'Reservoir', 'Great Lake'])]
# there are several projections ('HorizontalCoordinateReferenceSystemDatumName') within the coordinates.
# we only use known three ['NAD83', 'WGS84', 'NAD27'] that covers 87.5% of the data. Following Christoph, I added
# UNKWN as same as WGS84. Total coverage 97.6%
used_projection = {'NAD83':'epsg:4269', 'WGS84': 'epsg:4326', 'NAD27': 'epsg:4267', 'UNKWN': 'epsg:4326'}

station_proj = pd.DataFrame()
for projection in list(used_projection):
    station_part = station[station['HorizontalCoordinateReferenceSystemDatumName']==projection]
    station_part = gpd.GeoDataFrame(station_part,geometry=gpd.points_from_xy(station_part.LongitudeMeasure,
                                                                             station_part.LatitudeMeasure))
    station_part.crs = used_projection[projection]
    if station_part.crs != 'epsg:5070':
        station_part= station_part.to_crs('epsg:5070')
    station_proj = station_proj.append(station_part)

##################################################################################################
#SPATIAL JOIN MN STATION DATA WITH NATIONAL WATERBODY DATA
station_proj.crs = 'epsg:5070'
#use national shapefile directly
lake_epa_nhd_buffer = gpd.sjoin(nhd_lake_water_buffer, station_proj, how='inner', op='contains')
lake_epa_nhd = pd.merge(nhd_lake_water, lake_epa_nhd_buffer[['PERMANENT_']],
                        on='PERMANENT_', how='inner')
#######################################################################################################################
#LOAD RESULT DATA AND KEEP ONLY LAKES THAT IS WITHIN NATIONAL WATERBODY MATCHED DATASET
result = pd.read_csv(result_file)
keep_result = ['ActivityIdentifier', 'MonitoringLocationIdentifier','ActivityStartDate', 'ActivityEndDate',
             'CharacteristicName', 'ResultValueTypeName','ResultMeasureValue', 'ResultMeasure/MeasureUnitCode', 'ResultStatusIdentifier']
result = result[keep_result]

result_keep = pd.merge(result,
                       lake_epa_nhd_buffer,
                       how='left', left_on='MonitoringLocationIdentifier', right_on='MonitoringLocationIdentifier')

result_keep = result_keep[result_keep['PERMANENT_'].notna()]

#Get unique characteristicsName and corresponding units [RUN THIS PART ONLY IF YOU WANT PARAMETERS NAME]
# char_units = result[['CharacteristicName', 'ResultMeasureValue', 'ResultMeasure/MeasureUnitCode']]
# char_units_value = char_units['CharacteristicName'].value_counts()
# char_units.drop_duplicates(subset ="CharacteristicName", keep = 'first', inplace = True)
# char_units.to_csv(char_output,index=False)
#pd.DataFrame(pd.unique(mn_result_keep.CharacteristicName)).to_csv(char_output,index=False)

################################################################################################################################
characteristics_keep = {'secchi': ['Depth, Secchi disk depth', 'Secchi Reading Condition (choice list)',
                                   'Depth, Secchi disk depth (choice list)', 'Secchi, Horizontal Distance',
                                   'Light attenuation, depth at 99%',
                                   'Light attenuation at measurement depth', 'Light attenuation coefficient'],
                        'temp': ['Temperature, water'],
                        'chla': ['Chlorophyll a(probe)', 'Chlorophyll a, uncorrected for pheophytin',
                                 'Chlorophyll a','Chlorophyll a (probe relative fluorescence)', 'Chlorophyll',
                                 'Chlorophyll a - Phytoplankton (suspended)'],
                        'colora': ['Apparent color'],
                        'colort': ['True color'],
                        'doc': ['Organic carbon']}

characteristics_keep_list = reduce(lambda x,y: x+y, list(characteristics_keep.values()))

result_keep_nutr = result_keep[result_keep['CharacteristicName'].isin(characteristics_keep_list)]

#delete Non-numric from 'ResultMeasureValue' and convert to numeric
result_keep_nutr['ResultMeasureValue'] = pd.to_numeric(result_keep_nutr['ResultMeasureValue'], errors='coerce')
result_keep_nutr = result_keep_nutr[result_keep_nutr['ResultMeasureValue'].notna()]

#change Light attenuation at measurement depth,
light_coeff = ['Light attenuation at measurement depth', 'Light attenuation coefficient']

result_keep_nutr.loc[(result_keep_nutr['CharacteristicName'].isin(light_coeff)),'ResultMeasureValue']= (1.45*100)/result_keep_nutr['ResultMeasureValue']
result_keep_nutr.loc[(result_keep_nutr['CharacteristicName'].isin(light_coeff)),'ResultMeasure/MeasureUnitCode']= 'm'


print(result['ResultMeasure/MeasureUnitCode'][result.CharacteristicName == 'Light attenuation at measurement depth'])
print(result['ResultMeasureValue'][result.CharacteristicName == 'Light attenuation at measurement depth'])


for cn in characteristics_keep.keys():
    result_keep_nutr.loc[(result_keep_nutr['CharacteristicName'].isin(characteristics_keep[cn])),'CharacteristicName']=cn

#CLEAN DATA
#drop if it is duplucate in ActivityStartDate, CharacteristicName, ResultMeasureValue, and location (LatitudeMeasure, LongitudeMeasure)
result_keep_nutr = result_keep_nutr.drop_duplicates(subset=['PERMANENT_', 'ActivityStartDate', 'CharacteristicName', 'ResultMeasureValue',
                                            'MonitoringLocationIdentifier'])

#CONVERT UNITS
#convert secchi depth units

result_keep_nutr.loc[(result_keep_nutr['ResultMeasure/MeasureUnitCode'] == 'ft'),'ResultMeasureValue']=result_keep_nutr['ResultMeasureValue']/3.28084

result_keep_nutr.loc[(result_keep_nutr['ResultMeasure/MeasureUnitCode'] == 'ft'),'ResultMeasure/MeasureUnitCode']='m'

result_keep_nutr.loc[(result_keep_nutr['ResultMeasure/MeasureUnitCode'] == 'cm'),'ResultMeasureValue']=result_keep_nutr['ResultMeasureValue']/100

result_keep_nutr.loc[(result_keep_nutr['ResultMeasure/MeasureUnitCode'] == 'cm'),'ResultMeasure/MeasureUnitCode']='m'

result_keep_nutr.loc[(result_keep_nutr['ResultMeasure/MeasureUnitCode'] == 'in'),'ResultMeasureValue']=result_keep_nutr['ResultMeasureValue']/(3.28084*12)

result_keep_nutr.loc[(result_keep_nutr['ResultMeasure/MeasureUnitCode'] == 'in'),'ResultMeasure/MeasureUnitCode']='m'

#convert chlorophyll units

result_keep_nutr.loc[(result_keep_nutr['ResultMeasure/MeasureUnitCode'] == 'mg/l'),'ResultMeasureValue']= 1000*result_keep_nutr['ResultMeasureValue']
result_keep_nutr.loc[(result_keep_nutr['ResultMeasure/MeasureUnitCode'] == 'mg/l'),'ResultMeasure/MeasureUnitCode']='ug/l'

result_keep_nutr.loc[(result_keep_nutr['ResultMeasure/MeasureUnitCode'] == 'ppb'),'ResultMeasure/MeasureUnitCode']='ug/l'

#create unique activity ID based on ActivityIdentifier and ActivityStartDate
result_keep_nutr = result_keep_nutr.assign(unique_id=(result_keep_nutr['PERMANENT_'] + '_' +
                                                      result_keep_nutr['ActivityStartDate'] + '_' +
                                                     result_keep_nutr['MonitoringLocationIdentifier']
                                                            ).astype('category').cat.codes)
result_keep_nutr['unique_id']= result_keep_nutr['unique_id'] + 1

#get mean value for duplicated unique_id and CharacteristicName
result_keep_nutr_dup = result_keep_nutr[result_keep_nutr.duplicated(subset=['unique_id', 'CharacteristicName'], keep=False)]

result_keep_nutr_dup = result_keep_nutr_dup.groupby(['unique_id','CharacteristicName'], as_index=False)['ResultMeasureValue'].mean()
result_keep_nutr_dup = result_keep_nutr_dup.rename({"ResultMeasureValue":"ResultMeasure_dup"}, axis='columns')
#delete duplicated rows for unique_id and CharacteristicName and update ResultMeasureValue
result_keep_nutr = result_keep_nutr.drop_duplicates(subset=['unique_id', 'CharacteristicName'])

result_keep_nutr = pd.merge(result_keep_nutr, result_keep_nutr_dup, how = 'left', on = ['unique_id', 'CharacteristicName'])

result_keep_nutr.loc[result_keep_nutr['ResultMeasure_dup'].notna(), 'ResultMeasureValue'] = result_keep_nutr['ResultMeasure_dup']

result_keep_nutr = result_keep_nutr.drop(columns=['ResultMeasure_dup'])

#from long to wide
values = ['ResultMeasureValue', 'ResultMeasure/MeasureUnitCode']

result_wide = result_keep_nutr.pivot(index='unique_id', columns='CharacteristicName',
                                               values=values)

parameters = list(pd.unique(result_keep_nutr.CharacteristicName))
parameters.sort()
#keep_columns_idx = list(np.arange(0, 2*len(parameters), 1)) + list(np.arange(4, len(values)*len(parameters), len(parameters)))

#all_colnames = list(string.ascii_lowercase[0:len(result_wide2.columns)])
#result_wide2.columns = all_colnames

#result_wide2[all_colnames[2*len(parameters)]] =

#keep_columns = [all_colnames[i] for i in keep_columns_idx]

#result_wide3 = result_wide2[keep_columns]

colnames = list(itertools.chain(*[[par + '_result'] for par in parameters]+ [[par + '_unit'] for par in parameters]))

result_wide.columns = colnames

result_wide.reset_index(level=0, inplace=True)

common_columns = ['ActivityIdentifier', 'MonitoringLocationIdentifier',
       'ActivityStartDate', 'ActivityEndDate', 'StateCode',
        'PERMANENT_', 'FDATE', 'GNIS_ID', 'GNIS_NAME',
       'ELEVATION', 'FTYPE', 'FCODE', 'VISIBILITY', 'SHAPE_Area', 'MonitoringLocationName',
       'MonitoringLocationTypeName', 'HUCEightDigitCode', 'LatitudeMeasure',
       'LongitudeMeasure', 'unique_id'#, 'geometry'
                  ]

result_keep_nutr_non = result_keep_nutr.drop_duplicates(subset=['unique_id'], keep='first')

result_output = pd.merge(result_keep_nutr_non[common_columns], result_wide, on = 'unique_id', how = 'left')

#date validation
mask = pd.to_datetime(result_output.ActivityStartDate, format='%Y-%m-%d', errors='coerce').notna()
result_output = result_output[mask]

#result output to csv
result_output[common_columns+colnames].to_csv(output_file,index=False)