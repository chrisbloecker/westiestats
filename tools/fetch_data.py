#!/usr/bin/env pyhton3

from requests import post
from argparse import ArgumentParser
from re       import findall

# -----------------------------------------------------------------------------

def mkParser():
  p = ArgumentParser()
  p.add_argument("--fromid", default=1,         help="first wscid to fetch")
  p.add_argument("--toid",   default=12905,     help="last  wscid to fetch")
  p.add_argument("--out",    default="db.json", help="name of the output file")
  return p

# -----------------------------------------------------------------------------

URL = "http://swingdancecouncil.herokuapp.com/pages/dancer_point_history.json?wscid="

def fetchData(outfile : str, fromid : int, toid: int):
  fh = open(outfile, "w")
  fh.write("[")

  print("[INFO] Fetching data for " + str(fromid))
  data = post(URL + str(fromid)).text
  if len(findall("full_name", data)) > 0:
    fh.write(data)
  else:
    print("[INFO] No data for " + str(wscid))

  for wscid in range(fromid+1, toid+1):
    print("[INFO] Fetching data for " + str(wscid))
    data = post(URL + str(wscid)).text
    if len(findall("full_name", data)) > 0:
      fh.write(",")
      fh.write(data)
    else:
      print("[INFO] No data for " + str(wscid))

  fh.write("]")

  fh.flush()
  fh.close()

# -----------------------------------------------------------------------------

def main():
  args = mkParser().parse_args()

  fetchData(args.out, int(args.fromid), int(args.toid))

if __name__ == '__main__':
  main()