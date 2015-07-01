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


def writeStart(fh):
  fh.write("{\"persons\":[")


def writeEnd(fh):
  fh.write("]}")


def fetchEntry(id):
  try:
    return post(URL + str(id)).text
  except:
    print("[ERR ] Problem fetching data for " + str(id) + ". Retrying...")
    return fetchEntry(id)


def hasData(data):
  return len(findall("full_name", data)) > 0


def writeFirst(fh, data):
  fh.write(data)


def writeEntry(fh, data):
  fh.write(",\n")
  writeFirst(fh, data)


def fetchData(outfile : str, fromid : int, toid: int):
  fh = open(outfile, "w")
  writeStart(fh)

  print("[INFO] Fetching data for " + str(fromid))
  data = fetchEntry(fromid)
  if hasData(data):
    writeFirst(fh, data)
  else:
    print("[INFO] No data for " + str(wscid))

  for wscid in range(fromid+1, toid+1):
    print("[INFO] Fetching data for " + str(wscid))
    data = fetchEntry(wscid)
    if hasData(data):
      writeEntry(fh, data)
    else:
      print("[INFO] No data for " + str(wscid))

  writeEnd(fh)
  fh.flush()
  fh.close()

# -----------------------------------------------------------------------------

def main():
  args = mkParser().parse_args()

  fetchData(args.out, int(args.fromid), int(args.toid))

if __name__ == '__main__':
  main()
