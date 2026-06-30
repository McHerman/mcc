#!/usr/bin/env python3
"""Convert a flat binary to readmemh-style hex (32-bit words)."""
import sys, struct, argparse

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("input")
    ap.add_argument("output")
    args = ap.parse_args()

    with open(args.input, "rb") as f:
        data = f.read()

    # pad to word boundary
    while len(data) % 4:
        data += b'\x00'

    with open(args.output, "w") as f:
        for i in range(0, len(data), 4):
            word, = struct.unpack_from("<I", data, i)
            f.write(f"{word:08x}\n")

if __name__ == "__main__":
    main()
