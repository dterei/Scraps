#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
import sys

def usage():
  print("""
Usage: %s <iatimes expected> <iatimes measured>
""" % sys.argv[0])
  sys.exit(1)

def load_iatimes(fname):
  with open(fname, 'rb') as ia:
    iatimes = np.genfromtxt(ia, autostrip=True, dtype=np.int64)
    return iatimes

def check_ia_size(ia1, ia2):
  if len(ia1) != len(ia2):
    print("IA times are of same length...")
    print("IA1 length", len(ia1))
    print("IA2 length", len(ia2))
    raise

def ss_res(ia_exp, ia_mes):
  sumsq = 0
  for i in range(0,len(ia_exp)):
    a, b = ia_exp[i], ia_mes[i]
    r = a - b
    sumsq += r*r
  return sumsq

def ss_tot(ia_mes, mean):
  sumsq = 0
  for y in ia_mes:
    r = y - mean
    sumsq += r*r
  return sumsq

def plot_ecdf(ia_exp, ia_mes):
  bins = 10000
  plt.hist(ia_exp, bins, alpha=0.5, label='expected', cumulative=True,
      histtype='step')
  plt.hist(ia_mes, bins, alpha=0.5, label='observed', cumulative=True,
      histtype='step')
  plt.legend(loc='upper right')
  plt.show()

def check_dis(exp, mes):
  ia_exp = load_iatimes(exp)
  ia_mes = load_iatimes(mes)
  check_ia_size(ia_exp, ia_mes)
  ks_res = stats.ks_2samp(ia_exp, ia_mes)
  ssq_res = ss_res(ia_exp, ia_mes)
  ssq_tot = ss_tot(ia_mes, np.mean(ia_mes))
  r2 = 1 - (ssq_res / ssq_tot)

  print("Distributions:")
  print(" - [Exp] mean = %10d" % np.mean(ia_exp))
  print(" -        std = %10d" % np.std(ia_exp))
  print(" -        max = %10d" % np.max(ia_exp))
  print(" - [Obs] mean = %10d" % np.mean(ia_mes))
  print(" -        std = %10d" % np.std(ia_mes))
  print(" -        max = %10d" % np.max(ia_mes))
  print("KS Test:")
  print(" - Stat = %f" % ks_res[0])
  print(" - PVal = %f" % ks_res[1])
  print("R Squared:")
  print(" - SS_res = %d" % ssq_res)
  print(" - SS_tot = %d" % ssq_tot)
  print(" - R2     = %f" % r2)

  plot_ecdf(ia_exp, ia_mes)

if __name__ == '__main__':
  if len(sys.argv) != 3:
    usage()
  check_dis(sys.argv[1], sys.argv[2])

