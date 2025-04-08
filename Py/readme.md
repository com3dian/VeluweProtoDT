# Python source code for WUR AI team on ProtoDT

...

### How to use...
`class DataloaderMaker`

```python
from dataloadermaker import DataLoaderMaker
testDataLoader = DataLoaderMaker()
testDataLoader.load()
testDataLoader.makeBudBurstDataset()
testDataLoader.makeSpatioTemporalDataset()

# Get the budburst and temperature dataframe
temp_df = testDataLoader.get("temp_climwin_input")
budburst_df = testDataLoader.get("interpolated")
```
