i#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 16 12:46:30 2018

OpenStreetMap

osmnx
https://github.com/gboeing/osmnx
GDAL needed
(ex., in debian/Ubuntu, libgdal-dev is to be installed)
It also dependts geopandas and rtree
rtree dependes libspatialindex
(libspatialindex-dev)

https://github.com/gboeing/osmnx-examples/blob/master/notebooks/17-street-network-orientations.ipynb
@author: ks
"""

import osmnx as ox
import matplotlib.pyplot as plt
import pandas as pd

fig, _ = ox.plot_shape(ox.gdf_from_place('Moscow, Russia'))
fig.savefig('shape_Moscow.png')
fig, _ = ox.plot_graph(
        ox.graph_from_place('Moscow, Russia', network_type='drive'))
fig.savefig('loads_Moscow.png')

fig, ax = ox.plot_shape(
        ox.gdf_from_place('St-Peterburg, Russia'))
fig.savefig('shape_Leningrad.png')
fig, ax = ox.plot_graph(
        ox.graph_from_place('St-Peterburg, Russia', network_type='drive'))
fig.savefig('loads_Leningrad.png')


fig, _ = ox.plot_shape(ox.gdf_from_place('Shibuya, Tokyo, Japan'))
fig.savefig('shape_Shibuya.png')
fig, _ = ox.plot_graph(ox.graph_from_place(
        'Shibuya, Tokyo, Japan', network_type='drive'
        ))
fig.savefig('load_Shibuya.png')

fig, _ = ox.plot_graph(ox.graph_from_place(
        'Toshima-ku, Tokyo, Japan', network_type='drive'
        ))