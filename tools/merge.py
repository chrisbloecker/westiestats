#!/usr/bin/env python3

from argparse import ArgumentParser


def mkParser():
  p = ArgumentParser()
  p.add_argument("--files", required=True, nargs="+", help="Files to merge")
  p.add_argument("--out", required=True, help="Output file")
  return p


def readContent(f):
  fh  = open(f, "r")
  res = fh.read()
  fh.close()
  return res


def main():
  args = mkParser().parse_args()

  fh = open(args.out, "w")

  if len(args.files) == 0:
    return
  elif len(args.files) == 1:
    fh.write(readContent(args.files[0]))

  else:
    #chop off the ] in the end
    fh.write(readContent(args.files[0])[:-1])

    for f in args.files[1:-1]:
      # chop off the [ in the beginning and end
      # add a , in the beginning to join with previous file
      fh.write("," + readContent(f)[1:-1])

    fh.write("," + readContent(args.files[-1])[1:])

  fh.flush()
  fh.close()

if __name__ == '__main__':
  main()