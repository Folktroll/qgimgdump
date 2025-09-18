#!/usr/bin/env python3
"""mp2json.py

Tool for normalizing and exporting Polish Format (.mp) files to JSON for easier comparison.

Features:
- Robust parsing (handles [END] markers, skips comments)
- Deduplication of identical sections
- Skipping empty props
- Property filtering with wildcards:
  * `--drop-props`: remove matching keys
  * `--allow-props`: keep only matching keys
- Coordinate trimming for Data* keys (round to N decimals)
- Flexible output formatting:
  * newline-delimited JSON
  * pretty-print with `--indent`
  * compact diff-friendly with `--compact`
  * auto-wrap with `--maxwidth`
- Encoding choice: utf-8 (default) or cp1251
- Splitting output:
  * by section type (`--split`)
  * by max file size MB (`--split-size`)
  * can combine both

Usage examples:
    python mp2json.py input.mp output.json --indent 2
    python mp2json.py input.mp output.json --drop-props Nod*,*Idx
    python mp2json.py input.mp output.json --allow-props Data*,Label*
    python mp2json.py input.mp output.json --coord-trim 3
    python mp2json.py input.mp output.json --encoding cp1251 --split-size 50
"""

import argparse
import json
import re
import fnmatch
from collections import defaultdict
from pathlib import Path

# regex to match coordinates inside parentheses
coord_pair = re.compile(r"\((-?\d+(?:\.\d+)?),(-?\d+(?:\.\d+)?)\)")


def trim_data_coords(val, digits):
    """Trim coordinates to N decimals in Data* props."""

    def repl(m):
        try:
            lat = round(float(m.group(1)), digits)
            lon = round(float(m.group(2)), digits)
            fmt = f"{{:.{digits}f}}"
            lat_s = fmt.format(lat).rstrip("0").rstrip(".")
            lon_s = fmt.format(lon).rstrip("0").rstrip(".")
            return f"({lat_s},{lon_s})"
        except ValueError:
            return m.group(0)

    return coord_pair.sub(repl, val)


def parse_mp(path):
    """Parse a Polish .mp file and yield sections as dicts."""
    with open(path, encoding="cp1251", errors="ignore") as fh:
        current = None
        for raw in fh:
            line = raw.strip()
            if not line or line.startswith("//") or line.startswith(";"):
                continue
            if line.startswith("[") and line.endswith("]"):
                tag = line[1:-1]
                if tag.upper().startswith("END"):
                    if current:
                        yield current
                        current = None
                    continue
                if current:
                    yield current
                current = {"type": tag, "props": {}}
                continue
            if "=" in line and current is not None:
                k, v = line.split("=", 1)
                current["props"][k.strip()] = v.strip()
        if current:
            yield current


def signature_of(section):
    """Create a unique signature for deduplication."""
    t = section["type"]
    items = tuple(sorted(section["props"].items()))
    return (t, items)


def should_match(key, patterns):
    return any(fnmatch.fnmatch(key, pat) for pat in patterns)


def process_file(
    path,
    drop_patterns=None,
    allow_patterns=None,
    skip_empty=True,
    dedup=True,
    coord_trim=None,
):
    """Process .mp file: parse, filter, dedup, trim."""
    drop_patterns = drop_patterns or []
    allow_patterns = allow_patterns or []
    seen = set()
    results = {}

    for sec in parse_mp(path):
        # filter props
        for k in list(sec["props"].keys()):
            drop = drop_patterns and should_match(k, drop_patterns)
            allow = not allow_patterns or should_match(k, allow_patterns)
            if drop or not allow:
                sec["props"].pop(k, None)

        # trim coordinates
        if coord_trim is not None:
            for k, v in list(sec["props"].items()):
                if k.startswith("Data"):
                    sec["props"][k] = trim_data_coords(v, coord_trim)

        if skip_empty and not sec["props"]:
            continue

        sig = signature_of(sec)
        if dedup and sig in seen:
            continue
        seen.add(sig)
        results[sig] = sec["props"].copy()
    return results


def format_object(obj, indent=None, compact=False):
    if indent:
        return json.dumps(obj, ensure_ascii=False, indent=indent)
    elif compact:
        inner = ", ".join(
            f'"{k}": {json.dumps(v, ensure_ascii=False)}'
            for k, v in obj["props"].items()
        )
        return f'{{   "type": "{obj["type"]}",   {inner} }}'
    else:
        return json.dumps(obj, ensure_ascii=False)


def write_jsonl(
    results_map,
    outfile,
    indent=None,
    maxwidth=None,
    encoding="utf-8",
    compact=False,
    split_size=None,
):
    """Write results to file(s) with chosen formatting."""
    outp = Path(outfile)
    arr = [
        {"type": sig[0], "props": dict(sig[1])}
        for sig in sorted(results_map.keys(), key=lambda s: (s[0], s[1]))
    ]

    def flush_to_file(objs, fname):
        with open(fname, "w", encoding=encoding) as fh:
            if indent or compact:
                fh.write("[\n")
                for i, obj in enumerate(objs):
                    fh.write(format_object(obj, indent=indent, compact=compact))
                    fh.write(",\n" if i < len(objs) - 1 else "\n")
                fh.write("]\n")
            elif maxwidth:
                fh.write("[\n")
                for i, obj in enumerate(objs):
                    line = json.dumps(obj, ensure_ascii=False)
                    if len(line) > maxwidth:
                        fh.write(json.dumps(obj, ensure_ascii=False, indent=2))
                    else:
                        fh.write("  " + line)
                    fh.write(",\n" if i < len(objs) - 1 else "\n")
                fh.write("]\n")
            else:
                for obj in objs:
                    fh.write(json.dumps(obj, ensure_ascii=False) + "\n")

    if split_size:
        max_bytes = split_size * 1024 * 1024
        part, buf, size = 1, [], 0
        for obj in arr:
            line = format_object(obj, indent=indent, compact=compact)
            encoded = (line + "\n").encode(encoding)
            if size + len(encoded) > max_bytes and buf:
                flush_to_file(buf, f"{outp}_part{part}.json")
                part, buf, size = part + 1, [], 0
            buf.append(obj)
            size += len(encoded)
        if buf:
            flush_to_file(buf, f"{outp}_part{part}.json")
    else:
        flush_to_file(arr, str(outp))


def main():
    parser = argparse.ArgumentParser(
        description="Normalize and compare Polish .mp files to JSON"
    )
    parser.add_argument("infile", help="input .mp file")
    parser.add_argument("outfile", help="output file or prefix")
    parser.add_argument(
        "--split", action="store_true", help="split output by section type"
    )
    parser.add_argument(
        "--drop-props",
        default="",
        help="comma-separated list of props to drop (wildcards supported)",
    )
    parser.add_argument(
        "--allow-props",
        default="",
        help="comma-separated list of props to allow (wildcards supported)",
    )
    parser.add_argument(
        "--no-dedup",
        action="store_true",
        help="disable deduplication of identical sections",
    )
    parser.add_argument(
        "--no-skip-empty", action="store_true", help="do not skip empty sections"
    )
    parser.add_argument(
        "--indent", type=int, help="pretty-print JSON with given indent"
    )
    parser.add_argument(
        "--compact", action="store_true", help="compact diff-friendly style"
    )
    parser.add_argument("--maxwidth", type=int, help="wrap lines longer than this")
    parser.add_argument(
        "--coord-trim", type=int, help="round coordinates in Data* props to N decimals"
    )
    parser.add_argument(
        "--encoding", default="utf-8", help="output encoding (utf-8 or cp1251)"
    )
    parser.add_argument(
        "--split-size",
        type=int,
        help="max size of each output file in MB (creates _partX files)",
    )
    args = parser.parse_args()

    drop_patterns = [p.strip() for p in args.drop_props.split(",") if p.strip()]
    allow_patterns = [p.strip() for p in args.allow_props.split(",") if p.strip()]

    res_map = process_file(
        args.infile,
        drop_patterns=drop_patterns,
        allow_patterns=allow_patterns,
        skip_empty=not args.no_skip_empty,
        dedup=not args.no_dedup,
        coord_trim=args.coord_trim,
    )

    if args.split:
        groups = defaultdict(list)
        for sig, props in res_map.items():
            groups[sig[0]].append(props)
        base = Path(args.outfile).with_suffix("")
        for t in groups:
            write_jsonl(
                {(t, tuple(sorted(p.items()))): p for p in groups[t]},
                f"{base}_{t}.json",
                indent=args.indent,
                maxwidth=args.maxwidth,
                encoding=args.encoding,
                compact=args.compact,
                split_size=args.split_size,
            )
    else:
        write_jsonl(
            res_map,
            args.outfile,
            indent=args.indent,
            maxwidth=args.maxwidth,
            encoding=args.encoding,
            compact=args.compact,
            split_size=args.split_size,
        )


if __name__ == "__main__":
    main()
