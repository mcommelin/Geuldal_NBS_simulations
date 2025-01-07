---
title: "Setup GIS dataset Geuldal"
author: "Meindert Commelin"
output: html_document
date: "2024-11-13"
---

## Introduction

This document described the steps to collect GIS data an prepare information for
the simulation of NBS measures. This work is financially supported by the LandEX
project.

Many different data sources are collected. First we start with downloading open 
datasets for topography and elevation.

## Downloaded datasets

### Elevation data
We first start with a 25 meter resolution DTM which we obtain from OpenDEM:
[opendem website](https://www.opendem.info/opendemeu_download_highres.html)

With the GRASS gis tools r.fill.dir -> r.watershed -> r.water.outlet we derive 
the basin of the Geulcatchment with the outlet at the crossing of the Juliana
canal.

DEM from Beglium on 20 resolution from:
https://www.geo.be/catalog/details/6657e6da-7345-416f-bef6-c6a8b2def9bd?l=nl

A 1m resolution DEM for Germany from:
https://www.opendem.info/opendtm_de_download.html

And the AHN4 0.5 or 5m resolution maps for the Netherlands from:
https://www.arcgis.com/apps/mapviewer/index.html?webmap=f94e188409724e67bb6069e33bfc0cd0

DEM for flanders at 1 m resolution:
https://download.vlaanderen.be/product/939/configureer

A DEM for Wallonia on 1 m resolution is not freely available, but can be accesses after requesting a lincense
at:



Step for AHN - create a VRT from all the separate tiles of the AHN4 with the function:
'Build virtual raster'


### Topography
For topography we use the QuickOSM pluging in QGIS. Attribution to OSM:
© OpenStreetMap contributors. We download the preset Urban
data which gives us the layers  

 - roads (lines)
 - buildings (poly)
 
and the following  separate layers:  

 - landuse (poly)
 - amentity:parking (poly) -> the landuse layer has some holes which can be
filled with specific other layers
 - highway (poly)
 - natural (poly)
 - surface (poly)
 - tourism (poly)
 - waterway (lines)

These layers give a full spatial coverage of the area and additional information 
of road type and width and streams.  

From the ArcGIS REST server of the waterboard of limburg the following layers 
are downloaded:

 - Regenwaterbuffer
 - Lijnvormig element
 - Wateroverlast knelpunten (wib)

The first two from the 'legger' :
https://maps.waterschaplimburg.nl/arcgis/rest/services/Legger/Legger/FeatureServer

And the last from 'wib' :
https://maps.waterschaplimburg.nl/arcgis/rest/services/wib/wib_knelpunten/FeatureServer

For the Netherlands the agriculture cultivation history is available in the BRP,
the history from 2009 to 2024 was downloaded from:
https://service.pdok.nl/rvo/brpgewaspercelen/atom/v1_0/basisregistratie_gewaspercelen_brp.xml

A LULC 10m raster based on OSM was downloaded from:
OpenStreetMap land use for Europe "Research Data"
Schultz, Michael; Li, Hao; Wu, Zhaoyhan; Wiell, Daniel; Auer, Michael; Zipf, Alexander, 2024, 
"OpenStreetMap land use for Europe "Research Data"", https://doi.org/10.11588/data/IUTCDN, heiDATA, V2


### Soil and geography
For the Netherlands the soil map was downloaded from:
https://nationaalgeoregister.nl/geonetwork/srv/dut/catalog.search#/metadata/ed960299-a147-4c1a-bc57-41ff83a2264f
An other source is the BOFEK 2020 soil map:
https://www.wur.nl/nl/show/bodemfysische-eenhedenkaart-bofek2020.htm


The soil map of Walony was downloaded with a REST server in QGIS:
https://geoservices.wallonie.be/arcgis/rest/services/SOL_SOUS_SOL/CNSW__PRINC_TYPES_SOLS/MapServer

The soil map of Flanders was downloaded with a WFS server in QGIS:
https://www.dov.vlaanderen.be/geoserver/bodemkaart/wfs?SERVICE=WFS&version=2.0.0&request=GetCapabilities

The soil map of Germany was obtained from the geoportal NRW:
https://www.geoportal.nrw/?activetab=map

map name:  IS BK 50 Bodenkarte von NRW 1 : 50.000 - Datensatz

Not all classifications are the same within the soil maps, so these had to be 
harmonized to be able to use these as one soil map in the model simulations. This
is done in the script 'Harmonize_soilmap.R' which results in the layer 'soils_uniform'.

## Data processing
For data processing from all these downloads see the models that are loaded in 
the file Geuldal_LandEX.qgz

models:  

 - Dem processing + catchment delineation: DEMtoCatchment
 - Prepare landuse map
 - Prepare soil map
 - Prepare additional layers


### DEM and catchment
The initial DEM used was from OpenDEM, however the accuracy of this DEM is not 
very high and for example the valley bottom does not always align with the streams
as loaded from the OSM data.

The DEMs from different sources are reprojected, merged and clipped to the 'region_outline'.
This is done with the project model 'Project models > Preprocessing > DEMprepare'
This rough 1m resolution DEM will be the basis to further produce DTM of lower resolutions.

This output has 0 as nodata value, by manually saving in QGIS and setting 0 as nodata value this is solved.
This map is stored as 'DTM_region_1m_rough.tif' 

Next the exact coordinate of the outlet point in the DTM has to be found. 
This was done manually by running by running the r.fill.dir and r.watershed functions
from GRASS GIS within QGIS, this was done for a small section of the DTM around 
the outlet of the Geul close to the Juliana channel.
