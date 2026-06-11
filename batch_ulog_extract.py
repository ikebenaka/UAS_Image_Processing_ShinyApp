#!/usr/bin/env python3
"""
Batch-export PX4 ULog topics for Astro logs.

Two modes:
  1) CLI (default): invoke a specific ulog2csv executable.
  2) Pure: bypass CLI, parse with pyulog and write CSVs directly.

Example (CLI mode, recommended since you've verified it works):
  python batch_ulog_extract.py ^
    --base-dir "\\nefscdata\\PSD-UAS\\Species_Projects\\Whales\\2025\\Beakers" ^
    --ulog2csv "C:\\Users\\Isaac.Benaka\\AppData\\Local\\Programs\\Python\\Python313\\Scripts\\ulog2csv.exe"

Example (pure mode):
  python batch_ulog_extract.py ^
    --base-dir "\\nefscdata\\PSD-UAS\\Species_Projects\\Whales\\2025\\Beakers" ^
    --pure
"""
import argparse, csv, glob, os, sys, subprocess
from typing import List, Tuple

DEFAULT_MESSAGES = "distance_sensor,vehicle_air_data"

def find_ulg_files(base_dir: str) -> List[str]:
    """Walk flight-day subfolders and collect Astro/log/*.ulg."""
    ulogs: List[str] = []
    for entry in os.listdir(base_dir):
        day = os.path.join(base_dir, entry)
        if not os.path.isdir(day):
            continue
        log_dir = os.path.join(day, "Astro", "log")
        if not os.path.isdir(log_dir):
            continue
        for fn in os.listdir(log_dir):
            if fn.lower().endswith(".ulg"):
                ulogs.append(os.path.join(log_dir, fn))
    return sorted(ulogs)

def run_cli_ulog2csv(ulog2csv_path: str, ulg_path: str, messages: str) -> Tuple[bool, str]:
    """Run the specified ulog2csv on one .ulg. Return (ok, stdout+stderr)."""
    out_dir = os.path.dirname(ulg_path)
    cmd = [ulog2csv_path, "-m", messages, "-o", out_dir, ulg_path]
    try:
        p = subprocess.run(
            cmd, check=True, cwd=out_dir,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True
        )
        return True, p.stdout
    except FileNotFoundError:
        return False, f"[ERROR] ulog2csv not found: {ulog2csv_path}"
    except subprocess.CalledProcessError as e:
        return False, e.stdout or ""

def verify_outputs(out_dir: str) -> Tuple[List[str], List[str]]:
    ds = sorted(glob.glob(os.path.join(out_dir, "distance_sensor_*.csv")))
    va = sorted(glob.glob(os.path.join(out_dir, "vehicle_air_data_*.csv")))
    return ds, va

def write_log(out_dir: str, ulg_path: str, text: str) -> None:
    base = os.path.splitext(os.path.basename(ulg_path))[0]
    log_path = os.path.join(out_dir, f"{base}_ulog2csv.txt")
    with open(log_path, "w", encoding="utf-8") as f:
        f.write(text)

# ------------------- Pure-Python fallback -------------------

def pure_export(ulg_path: str) -> Tuple[List[str], str]:
    """
    Parse with pyulog and write CSVs for distance_sensor_* and vehicle_air_data_*.
    Returns (written_paths, log_text).
    """
    out_dir = os.path.dirname(ulg_path)
    try:
        from pyulog import ULog
    except Exception as e:
        return [], f"[ERROR] importing pyulog failed: {e}"

    try:
        u = ULog(ulg_path)
    except Exception as e:
        return [], f"[ERROR] reading {ulg_path}: {e}"

    wanted = {"distance_sensor", "vehicle_air_data"}
    written: List[str] = []
    lines = []

    for ds in u.data_list:
        if ds.name not in wanted:
            continue
        name = ds.name
        mid = ds.multi_id
        data = ds.data  # dict of arrays
        cols = ["timestamp"] + [k for k in data.keys() if k != "timestamp"]
        out = os.path.join(out_dir, f"{name}_{mid}.csv")
        try:
            with open(out, "w", newline="") as f:
                w = csv.writer(f)
                w.writerow(cols)
                n = len(data["timestamp"])
                for i in range(n):
                    w.writerow([data["timestamp"][i]] + [data[k][i] for k in cols[1:]])
            written.append(out)
            lines.append(f"[pure] wrote {os.path.basename(out)} ({n} rows)")
        except Exception as e:
            lines.append(f"[pure][ERROR] writing {out}: {e}")

    if not any(os.path.basename(p).startswith("distance_sensor_") for p in written):
        lines.append("[pure][WARN] no distance_sensor_* instances written")

    return written, "\n".join(lines)

# ------------------- Main -------------------

def main() -> None:
    ap = argparse.ArgumentParser(description="Batch-extract Astro ULog CSVs")
    ap.add_argument("--base-dir", required=True,
                    help="Root containing flight-day subfolders (each with Astro/log/*.ulg)")
    ap.add_argument("--ulog2csv", default=None,
                    help="Full path to ulog2csv.exe (recommended). If omitted and --pure not set, tries 'ulog2csv' on PATH.")
    ap.add_argument("--messages", default=DEFAULT_MESSAGES,
                    help="Comma-separated topic base names. Default: %(default)s")
    ap.add_argument("--pure", action="store_true",
                    help="Bypass CLI and parse with pyulog directly.")
    args = ap.parse_args()

    ulogs = find_ulg_files(args.base_dir)
    if not ulogs:
        sys.exit("No .ulg files found under flight-day folders.")

    if not args.pure:
        u2c = args.ulog2csv or "ulog2csv"

    total = len(ulogs)
    ok_count = 0
    missing_ds = 0

    for idx, ulg in enumerate(ulogs, 1):
        out_dir = os.path.dirname(ulg)
        print(f"[{idx}/{total}] {ulg}")

        if args.pure:
            written, log_text = pure_export(ulg)
            write_log(out_dir, ulg, log_text)
            ds, va = verify_outputs(out_dir)
            if any(p in written for p in ds + va):
                ok_count += 1
            if not ds:
                missing_ds += 1
                print("  -> WARN: no distance_sensor_* output (pure mode)")
            else:
                print(f"  -> OK: {len(ds)} distance_sensor_*, {len(va)} vehicle_air_data_*")
            continue

        ok, text = run_cli_ulog2csv(u2c, ulg, args.messages)
        write_log(out_dir, ulg, text)
        ds, va = verify_outputs(out_dir)
        if ok and (ds or va):
            ok_count += 1
        if not ds:
            # Try a focused retry for distance_sensor
            ok_retry, text2 = run_cli_ulog2csv(u2c, ulg, "distance_sensor")
            write_log(out_dir, ulg, text + "\n\n[retry distance_sensor]\n" + text2)
            ds, va = verify_outputs(out_dir)
            if not ds:
                missing_ds += 1
                print("  -> WARN: no distance_sensor_* output after retry")
            else:
                print(f"  -> OK on retry: {len(ds)} distance_sensor_*")
        else:
            print(f"  -> OK: {len(ds)} distance_sensor_*, {len(va)} vehicle_air_data_*")

    print(f"\nDone. {ok_count}/{total} logs processed. "
          f"{missing_ds} logs missing distance_sensor_* CSVs.")

if __name__ == "__main__":
    main()
