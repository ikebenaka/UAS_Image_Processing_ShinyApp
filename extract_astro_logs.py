#!/usr/bin/env python3
import os, argparse, subprocess, sys, glob

def run_ulog2csv_on_file(ulg_path: str) -> bool:
    log_dir = os.path.dirname(ulg_path)

    # Primary attempt: both topics
    cmd = [
        "ulog2csv",
        "-m", "distance_sensor,vehicle_air_data",   # single -m, comma list
        "-o", log_dir,
        ulg_path
    ]

    try:
        res = subprocess.run(
            cmd, check=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            cwd=log_dir
        )
    except FileNotFoundError:
        print("❌  `ulog2csv` not found on PATH. Install/upgrade: pip install -U pyulog")
        return False
    except subprocess.CalledProcessError as e:
        print(f"❌  ulog2csv failed for {ulg_path}:\n{e.stderr.decode(errors='ignore')}")
        return False

    # Verify distance_sensor output exists; if not, retry requesting it alone.
    pattern = os.path.join(log_dir, "distance_sensor_*.csv")
    if glob.glob(pattern):
        print(f"✅  Converted (with distance_sensor): {ulg_path}")
        return True

    # Retry: force just distance_sensor
    retry_cmd = [
        "ulog2csv", "-m", "distance_sensor",
        "-o", log_dir, ulg_path
    ]
    try:
        subprocess.run(
            retry_cmd, check=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            cwd=log_dir
        )
    except subprocess.CalledProcessError as e:
        print(f"❌  Retry for distance_sensor failed: {e.stderr.decode(errors='ignore')}")
        return False

    if glob.glob(pattern):
        print(f"✅  Converted on retry (distance_sensor): {ulg_path}")
        return True
    else:
        print(f"⚠️  No distance_sensor_* CSV produced for {ulg_path}. "
              f"Run `ulog_info` to confirm the topic is present.")
        return False


def process_flight_day(flight_day_path: str) -> None:
    log_dir = os.path.join(flight_day_path, "Astro", "log")
    if not os.path.isdir(log_dir):
        print(f"⚠️  No Astro/log in {flight_day_path}; skipping.")
        return
    for fname in os.listdir(log_dir):
        if fname.lower().endswith(".ulg"):
            run_ulog2csv_on_file(os.path.join(log_dir, fname))


def main(base_dir: str) -> None:
    if not os.path.isdir(base_dir):
        sys.exit(f"ERROR: base-dir does not exist: {base_dir}")
    for entry in os.listdir(base_dir):
        path = os.path.join(base_dir, entry)
        if os.path.isdir(path):
            process_flight_day(path)


if __name__ == "__main__":
    p = argparse.ArgumentParser(
        description="Extract Astro distance_sensor + air-data CSVs via ulog2csv"
    )
    p.add_argument("--base-dir", required=True,
                   help="Root containing flight-day subfolders (each with Astro/log/*.ulg)")
    args = p.parse_args()
    main(args.base_dir)
