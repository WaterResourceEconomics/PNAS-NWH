import os
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import time
import contextily as cx
import folium
import io
from PIL import Image
import selenium
import mapclassify
import seaborn as sns
import numpy as np
start_time = time.time()

ext_folder = 'D:/NHD_H_National_GDB'
output_folder = 'G:/My Drive/NRRI/Zillow Project/Water Quality Hedonic/Water_Data/lake_maps'

state_boundary = gpd.read_file(os.path.join(output_folder, 'State_borders_WGS_84.shp'))
lake_output = pd.read_csv(os.path.join(output_folder, 'lake_ids.csv'))

nhd_national = os.path.join(ext_folder, "NHD_H_National_GDB.gdb")
national_waterbody = gpd.read_file(nhd_national, driver='FileGDB', layer='NHDWaterbody')
national_waterbody = national_waterbody[national_waterbody.FTYPE.isin([390, 436])]

national_waterbody = national_waterbody.to_crs('epsg:5070')

def keep_first(geo):
    if geo.geom_type == 'Polygon':
        return geo
    elif geo.geom_type == 'MultiPolygon':
        return geo[0]
# add nhd centroid for 5 states
national_waterbody_centroid = national_waterbody.copy()
national_waterbody_centroid.geometry = national_waterbody_centroid.geometry.apply(lambda _geo: keep_first(_geo))
national_waterbody_centroid.geometry = national_waterbody_centroid.centroid

col_maps = {
    'lake_with_house_sales': 'lake with house sales',
    'lake_with_any_water_quality' : 'lake with any water quality',
    'lake_with_both_water_quality': 'lake with both water quality',
    'lake_with_complete_dataset': 'lake with complete dataset (not slim)',
    'lim_obs_100': 'slim model with minimum 100 lakeshore sales',
    'slim_cluster_lim_obs_100_sample_5': 'slim model with minimum 100 lakeshore sales and minimum 5 water quality sample',
    'lim_obs_10_sample_3': 'lake with minimum 10 lakeshore sales and minimum 3 water quality sample',
    'lim_obs_0_sample_0' : 'lake where number of lakeshore sales and water quality is not considered',
    'baseline': 'lake with minimum 1 lakeshore sales and minimum 1 water quality sample'
}
# Get shapefile of analysis used in the baseline model
col = 'baseline'
lake_names_base = lake_output[col]
lake_names_base = lake_names_base[~lake_names_base.isnull()]
lake_shape_base = national_waterbody[national_waterbody.PERMANENT_IDENTIFIER.isin(lake_names_base)]
lake_shape_base.to_file(os.path.join(output_folder, 'baseline_lakes.shp'))
# 'lake_with_house_sales', 'lake_with_any_water_quality',
# 'lake_with_both_water_quality', 'lake_with_complete_dataset',
# 'slim_cluster_lim_obs_100', 'slim_cluster_lim_obs_100_sample_5'
# 'lim_obs_10_sample_3', 'lim_obs_0_sample_0'
for col in col_maps.keys():
    lake_names = lake_output[col]
    lake_names = lake_names[~lake_names.isnull()]
    lake_point = national_waterbody_centroid[national_waterbody_centroid.PERMANENT_IDENTIFIER.isin(lake_names)]
    lake_shape = national_waterbody[national_waterbody.PERMANENT_IDENTIFIER.isin(lake_names)]

    lake_point = lake_point.to_crs(epsg=3857)
    state_boundary = state_boundary.to_crs(epsg=3857)

    fig, ax = plt.subplots(1, figsize=(10, 6))
    # create map
    lake_point.plot(linewidth=0.8, ax=ax)
    state_boundary.geometry.boundary.plot(color=None,edgecolor='k',linewidth = 0.5,ax=ax)

    ax.set_title(f'{col_maps[col]},\n n = {len(lake_point)}', fontdict={'fontsize': '15', 'fontweight' : '2'})
    # remove the axis
    ax.axis('off')
    fig.savefig(os.path.join(output_folder, f'{col}.png'), dpi=600)
    # fig.savefig(os.path.join(output_folder, f'{col}_no_title.png'), dpi=600)
    plt.close('all')

# Folium based solution
for col in lake_output.columns:
    lake_names = lake_output[col]
    lake_names = lake_names[~lake_names.isnull()]
    lake_point = national_waterbody_centroid[national_waterbody_centroid.PERMANENT_IDENTIFIER.isin(lake_names)]
    lake_shape = national_waterbody[national_waterbody.PERMANENT_IDENTIFIER.isin(lake_names)]

    lake_point = lake_point.to_crs(epsg=3857)
    map = folium.Map()

    folium.GeoJson(data=lake_point["geometry"]).add_to(map)

    map.save(os.path.join(output_folder, f'2{col}.html'))
##################################################################################
# get water quality maps by tract id
tract_boundary = gpd.read_file(os.path.join(output_folder, 'cb_conus_tarct.shp'))
tract_boundary['tract_id'] = tract_boundary['STATEFP']+ tract_boundary['STATEFP']+ \
                             tract_boundary['COUNTYFP'] + tract_boundary['TRACTCE']
secchi = pd.read_parquet(os.path.join(output_folder, 'tract_secchi_2022_04_09.pqt'))
chla = pd.read_parquet(os.path.join(output_folder, 'tract_chla_2022_04_09.pqt'))

tract_secchi_chla = pd.merge(tract_boundary, secchi, left_on='tract_id', right_on='tract_id', how='left')
tract_secchi_chla = pd.merge(tract_secchi_chla, chla, left_on='tract_id', right_on='tract_id', how='left')

tract_secchi_chla.to_file(os.path.join(output_folder, 'tract_secchi_chla.shp'))

def plot_wq(gdf, vname, color=None, vmin=None, vmax=None, title = None, filename=None, resolution=None):
    variable = vname
    if color:
        color = color
    else:
        color = 'Blues'
    if vmin:
        vmin = vmin
    else:
        vmin = min(gdf[vname].dropna())
    if vmax:
        vmax = vmax
    else:
        vmax = max(gdf[vname].dropna())

    # create figure and axes for Matplotlib
    fig, ax = plt.subplots(1, figsize=(10, 6))

    gdf = gdf.dropna(subset=[vname])
    # create map
    gdf.plot(column=variable, cmap=color, linewidth=0.8, ax=ax, edgecolor='0.8')
    state_boundary.geometry.boundary.plot(color=None,edgecolor='k',linewidth = 0.5,ax=ax)
    # remove the axis
    ax.axis('off')

    # add a title
    if title:
        ax.set_title(title, fontdict={'fontsize': '15', 'fontweight' : '2'})
    # create an annotation for the data source
    #ax.annotate(‘Source: London Datastore, 2014’,xy=(0.1, .08),  xycoords=’figure fraction’, horizontalalignment=’left’, verticalalignment=’top’, fontsize=12, color=’#555555')

    # Create colorbar as a legend
    sm = plt.cm.ScalarMappable(cmap='Blues', norm=plt.Normalize(vmin=vmin, vmax=vmax))
    # empty array for the data range
    sm._A = []
    # add the colorbar to the figure
    cbar = fig.colorbar(sm)

    if filename:
        fig.savefig(filename, dpi=resolution)
    plt.close('all')


def plot_wq_by_bin(gdf, vname, bins, color=None, title = None, filename=None, resolution=None):
    variable = vname
    if color:
        color = color
    else:
        color = 'Blues'
    if bins:
        bins=bins
    else:
        bins = mapclassify.Quantiles(gdf['quant'], k=5).bins
    # create figure and axes for Matplotlib
    fig, ax = plt.subplots(1, figsize=(10, 6))

    gdf = gdf.dropna(subset=[vname])
    # create map
    state_boundary.boundary.plot(edgecolor='black',linewidth = 0.25,ax=ax, zorder=1)
    gdf.plot(column=variable, cmap=color, scheme="User_Defined",
             legend=True, classification_kwds=dict(bins=bins),
             ax=ax, zorder=2)

    # remove the axis
    ax.axis('off')

    # add a title
    if title !=None:
        ax.set_title(title, fontdict={'fontsize': '15', 'fontweight' : '2'})
    plt.tight_layout()
    if filename:
        fig.savefig(filename, dpi=resolution)
    plt.close('all')

title_secchi = 'Average latest secchi depth in meter by census tract'
title_chla = 'Average latest chla in ug/l by census tract'
output_secchi = os.path.join(output_folder, 'secchi_by_tract_no_title.png')
output_chla = os.path.join(output_folder, 'chla_by_tract_no_title.png')

plot_wq(gdf=tract_secchi_chla, vname='secchi', color = 'Blues', vmin=None, vmax=None, title = None,
        filename=output_secchi, resolution=600)

plot_wq(gdf=tract_secchi_chla, vname='chla', color = 'Blues', vmin=None, vmax=None, title = None,
        filename=output_chla, resolution=600)

secchi_bin = [0.5, 1, 2, 4]
chla_bin = [3, 8, 15, 25, 50, 100]
output_secchi_bin = os.path.join(output_folder, 'secchi_by_tract_bin_no_title.png')
output_chla_bin = os.path.join(output_folder, 'chla_by_tract_bin_no_title.png')

plot_wq_by_bin(gdf=tract_secchi_chla, vname='secchi', bins=secchi_bin, color = 'Blues', title = None,
        filename=output_secchi_bin, resolution=600)

plot_wq_by_bin(gdf=tract_secchi_chla, vname='chla', bins=chla_bin, color = 'Greens', title = None,
        filename=output_chla_bin, resolution=600)

##########################################################################################################
# box and whisker plot of hedonic results
########################################################################################################
result_folder = 'G:/My Drive/NRRI/Zillow Project/Water Quality Hedonic/Water_Data/Results_combined'
results = pd.read_excel(os.path.join(result_folder, 'NWH_2022_04_09.xlsx'),
                        sheet_name='box_plot', usecols=[0,1,2,3,4,5])

results = results.reset_index()
results_long = pd.melt(results, id_vars='model', value_vars=['log_secchi',
                                              'lake_front x log_secchi', 'lake_shore x log_secchi'])
# results_long=results_long.rename(columns = {'variable':''})

def create_plot(type='box', filename=None, title=None, resolution=1200):
    fig, ax1 = plt.subplots(ncols=1, figsize=(6, 7))
    if type == 'box':
        sns.boxplot(data=results_long,  whis=np.inf, y='value', x='variable', ax=ax1)
    else:
        sns.violinplot(data=results_long,y='value', x='variable', linewidth=0, alpha= '0.2', ax=ax1)
    ax1 = sns.stripplot(x="variable", y="value", data=results_long, color=".3")
    sns.stripplot(data=results_long[results_long.model=='base'], x= 'variable', y='value', color='red')

    base = results_long[results_long.model=='base'].reset_index().value
    # base = results_long.groupby(['variable'])['value'].median()
    vertical_offset = results_long['value'].median() * 0.8 # offset from median for display

    for xtick in ax1.get_xticks():
        ax1.text(xtick,base[xtick] + vertical_offset,base[xtick],
                      horizontalalignment='center',size='x-small',color='r',weight='semibold')

    if title:
        ax1.set_title(title, fontdict={'fontsize': '12', 'fontweight' : '1'})
    ax1.set_xlabel('', fontsize = 8)
    ax1.set_ylabel('coefficient', fontsize = 10)

    plt.xticks(fontsize = 8, rotation=0)
    plt.xticks([0, 1,2],['log(secchi)', '100-300m buffer x\nlog(secchi)', '100m buffer x\nlog(secchi)'], fontsize = 8)
    fig.tight_layout()
    if filename:
        fig.savefig(filename, dpi=resolution)
    plt.close('all')

num_models = len(results)
box_output = os.path.join(output_folder, 'box_plot_three.png')
violin_output = os.path.join(output_folder, 'violin_plot_three.png')
create_plot(type='box', filename=box_output,
            title=f'Box plot of selected parameters [number of models = {num_models}]')
create_plot(type='violin', filename=violin_output,
            title=None)

########################################################################################################
# NLA Lakes
nla2012_lakes = gpd.read_file(os.path.join(output_folder, 'NLA2012_lakes.shp'))
baseline_lakes = gpd.read_file(os.path.join(output_folder, 'baseline_lakes.shp'))
baseline_nla_lakes = gpd.sjoin(baseline_lakes, nla2012_lakes[['SITE_ID', 'geometry']], how='inner', op='contains')

baseline_nla_lakes.to_file(os.path.join(output_folder, 'baseline_nla_lakes.shp'))
baseline_nla_lakes['PERMANENT_'].to_csv(os.path.join(output_folder, 'baseline_nla_lakes.csv'), index=False)

