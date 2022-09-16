#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
For clippping the land cover dataset to a watershed
"""

"""Clip a bunch of geotiffs to the same area"""
import glob
from osgeo import gdal

# watershed
ws = "/Volumes/T7/pa_hydro/Ohio/layers/globalwatershed.shp"

# land cover dataset
ls = glob.glob("/Volumes/T7/pa_hydro/landlayers/*.tif")
# from the projected (QGIS) pixel size:
dx = 0.0003253077953483678891
dy = -0.0003253077953483678891
# FOR: xRes = dx, yRes = dy, 

# subset all image files to common outer coordinates
for f in ls:
    split = f.split("_")
    wout = "/Volumes/T7/pa_hydro/Ohio/" + split[3] + ".crp.tif"
    gdal.Warp(wout, f, srcSRS = 'EPSG:5070', dstSRS = 'EPSG:4326', cutlineDSName = ws, cropToCutline=True)
    print(str(split[0]))

