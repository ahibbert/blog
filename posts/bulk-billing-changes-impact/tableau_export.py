import argparse
import json
import re
from pathlib import Path

import pandas as pd

try:
    from tableauscraper import TableauScraper as TS
    from tableauscraper import api, utils
except ImportError as exc:
    raise SystemExit(
        "This extractor needs the Python package 'tableauscraper'. "
        "Install with: py -3 -m pip install --user tableauscraper"
    ) from exc


HOST = "https://viz.aihw.gov.au"
WORKBOOK = "HWE-97-MBS-GP-bulk-billing-data_20260526"
VIEW = "MBSDashboardNew"
REFERER = f"{HOST}/t/Public/views/{WORKBOOK}/{VIEW}?:showVizHome=no&:embed=y"
START_URL = (
    f"{HOST}/vizql/t/Public/w/{WORKBOOK}/v/{VIEW}/startSession/viewing"
    "?%3AshowVizHome=no&%3Aembed=y&%3Aredirect=auth"
)
START_HEADERS = {
    "Accept": "application/json",
    "Content-Type": "application/x-www-form-urlencoded",
    "Tableau-Viz-Location": REFERER + "&:redirect=auth",
    "Tableau-Viz-Path": "/t/Public#false",
    "User-Agent": "Mozilla/5.0",
    "Referer": REFERER,
}

SHEETS = {
    "national": "GP attendances 1",
    "age": "GP attendances 2",
    "seifa": "GP attendances 3",
    "benefits": "GP attendances 4",
    "latest_lga_map": "Map",
}


def extract_sheet(sheet_name: str) -> pd.DataFrame:
    scraper = TS()
    api.setSession(scraper)
    scraper.session.headers.update(
        {"User-Agent": "Mozilla/5.0", "Referer": REFERER}
    )

    response = scraper.session.post(START_URL, headers=START_HEADERS)
    response.raise_for_status()

    scraper.tableauData = response.json()
    scraper.tableauData["sheetId"] = sheet_name
    scraper.host = HOST

    raw = api.getTableauData(scraper)
    match = re.search(r"\d+;({.*})\d+;({.*})", raw, re.S)
    if match is None:
        raise RuntimeError(f"Could not parse Tableau bootstrap response for {sheet_name}")

    scraper.info = json.loads(match.group(1))
    scraper.data = json.loads(match.group(2))
    pres_model = scraper.data["secondaryInfo"]["presModelMap"]
    data_model = pres_model["dataDictionary"]["presModelHolder"][
        "genDataDictionaryPresModel"
    ]
    scraper.dataSegments = data_model.get("dataSegments", {})
    scraper.parameters = utils.getParameterControlInput(scraper.info)
    scraper.dashboard = scraper.info["sheetName"]
    scraper.filters = utils.getFiltersForAllWorksheet(
        scraper.logger,
        scraper.data,
        scraper.info,
        rootDashboard=scraper.dashboard,
    )

    return scraper.getWorkbook().worksheets[0].data


def trim_heavy_columns(df: pd.DataFrame) -> pd.DataFrame:
    heavy_patterns = ("geometry", "longitude", "latitude", "collect(")
    keep = [
        col
        for col in df.columns
        if not any(pattern in col.lower() for pattern in heavy_patterns)
    ]
    return df.loc[:, keep]


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out-dir", required=True)
    args = parser.parse_args()

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    manifest_rows = []
    for label, sheet in SHEETS.items():
        df = extract_sheet(sheet)
        if label == "latest_lga_map":
            df = trim_heavy_columns(df)
        path = out_dir / f"aihw_tableau_{label}.csv"
        df.to_csv(path, index=False)
        manifest_rows.append(
            {
                "label": label,
                "sheet": sheet,
                "rows": len(df),
                "columns": len(df.columns),
                "path": path.name,
                "source_workbook": WORKBOOK,
            }
        )

    pd.DataFrame(manifest_rows).to_csv(out_dir / "aihw_tableau_manifest.csv", index=False)


if __name__ == "__main__":
    main()
