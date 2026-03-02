# Kaggle API Setup

The Kaggle API is installed and configured. One manual step is required:

## Add Your API Credentials

1. Log in to [Kaggle](https://www.kaggle.com/).

2. Click your profile picture (top right) → **Account** (or go to [Account settings](https://www.kaggle.com/settings)).

3. Scroll to the **API** section and click **Create New API Token**.

4. This downloads `kaggle.json` (contains your username and API key).

5. Move the file to:
   ```
   C:\Users\alexm\.kaggle\kaggle.json
   ```

6. On Windows, optionally: Right-click the file → Properties → Advanced → uncheck "Encrypt contents to secure data" (avoids some permission issues).

## Verify

Open a **new** terminal and run:

```
kaggle datasets list
```

If configured correctly, you'll see a list of datasets.

## Download NCAA Data

Once configured, run the project pipeline:

```
Rscript run_all.R
```

Or from R:

```r
source("run_all.R")
```

The `01_download_data.R` script will use the Kaggle CLI to fetch the NCAA basketball dataset automatically.
