import numpy as np
import pandas as pd
import dask
import intake
import xarray as xr

ds_url = ("https://storage.googleapis.com/ar6-lsl-simulations-public-standard/tide-gauges/full_sample_workflows/wf_1e/ssp119/total-workflow.zarr")

ds = xr.open_dataset(ds_url, engine='zarr', chunks='auto')
ds

wf1e_119 = ds['sea_level_change'].sel(locations=288)

catalog = intake.open_catalog("s3://noaa-nos-cora-pds/CORA_V1.1_intake.yml",storage_options={'anon':True})
list(catalog)
ds = xr.open_dataset(catalog["CORA-V1.1-fort.63-timeseries"], engine='zarr', chunks='auto')

ds = catalog["CORA-V1.1-fort.63-timeseries"].to_dask()
ds

def nearxy(x,y,xi,yi):
    ind = np.ones(len(xi),dtype=int)
    for i in range(len(xi)):
        dist = np.sqrt((x-xi[i])**2+(y-yi[i])**2)
        ind[i] = dist.argmin()
    return ind

  
